--  Copyright (c) 2021 Bob Goddard <git@1.git.bgcomp.co.uk>
--
--  This file is free software: you may copy, redistribute and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 2 of the License, or (at your
--  option) any later version.
--
--  This file is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Exceptions;
with Ada.Text_IO;
with AdaBase; use AdaBase;
with AdaBase.Results.Sets;
with AdaBase.Statement;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Arithmetic;
with Ada.Calendar.Conversions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Source_Info;
with GNAT.Calendar; use GNAT.Calendar;
with GNAT.Calendar.Time_IO;
with GNAT.Traceback;
with GNAT.Traceback.Symbolic;
with Counter_Arithmetic;
with Holiday;
with List_Handlers;
with Structures; use Structures;
with Unix_Utils;

package body DB_Routines is
   DR            : AdaBase.Driver.Base.MySQL.MySQL_Driver;
   Trace         : GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
   Length        : Natural;

   function DB_Connect return Local_Defs.Trilean is
      delay_time       : Duration := 1.0;
      delay_time_count : Duration := 0.0;
      target_time      : Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      DB_Disconnect;
      delay_loop :
      loop
         delay until target_time;

         if DB_Connect_Private = Local_Defs.TTrue then
            return Local_Defs.TTrue;
         end if;

         if delay_time_count = delay_time then
            delay_time := delay_time * 2.0;
            delay_time_count := 0.0;
         else
            delay_time := delay_time + 1.0;
         end if;

         target_time := target_time + delay_time;
         delay_time_count := delay_time_count + 1.0;
      end loop delay_loop;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         return Local_Defs.TBroken;
   end DB_Connect;

   function DB_Connect_Private return Local_Defs.Trilean is
   begin
      DR.basic_connect (database => Ada.Strings.Unbounded.To_String (Database),
                        username => Ada.Strings.Unbounded.To_String (DB_User),
                        password => Ada.Strings.Unbounded.To_String (DB_Pass),
                        hostname => Ada.Strings.Unbounded.To_String (DB_Host),
                        port     => DB_Port);

      return Local_Defs.TTrue;

   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         return Local_Defs.TBroken;
   end DB_Connect_Private;

   procedure DB_Disconnect is
   begin
      DR.disconnect;
   end DB_Disconnect;

   procedure Get_Day_Counters (TS_Start, TS_End : Ada.Calendar.Time; DB_Connect_Res : out Local_Defs.Trilean) is
      NS1            : constant String := "SELECT ts, ifIndex, ifAdminStatus, ifOperStatus, ifLastChange, ifInOctets, ifInDiscards, ifInErrors, ifInUnknownProtos, ifOutOctets, ifOutDiscards," &
                         "ifOutErrors, ifInMulticastPkts, ifInBroadcastPkts, ifOutMulticastPkts, ifOutBroadcastPkts, ifHCInOctets, ifHCInUcastPkts, ifHCInMulticastPkts," &
                         "ifHCInBroadcastPkts, ifHCOutOctets, ifHCOutUcastPkts, ifHCOutMulticastPkts, ifHCOutBroadcastPkts FROM counters where ";
      NS2            : constant String := Prepare_Day_Time_String (TS_Start, TS_End);
      --  NS2            : constant String := "TS >= " & Ada.Calendar.Conversions.To_Unix_Time (TS_Start)'Image & " AND TS <= " & Ada.Calendar.Conversions.To_Unix_Time (TS_End)'Image & " ";
      NS3            : constant String := "AND (ifInDiscards!=0 OR ifInErrors!=0 OR ifInUnknownProtos!=0 OR ifOutDiscards!=0 OR ifOutErrors!=0 OR ifInMulticastPkts!=0 OR " &
                         "ifInBroadcastPkts!= 0 OR " & "ifOutMulticastPkts!=0 OR ifOutBroadcastPkts!=0 OR ifHCInOctets!=0 OR ifHCInUcastPkts!=0 OR ifHCInMulticastPkts!=0 OR " &
                         "ifHCInBroadcastPkts!=0 OR ifHCOutOctets!=0 OR ifHCOutUcastPkts!=0 OR ifHCOutMulticastPkts!=0 OR ifHCOutBroadcastPkts!=0) ORDER BY ts, ifIndex";
      STMT           : Stmt_Type_Local := DR.prepare (NS1 & NS2 & NS3);
      Num_Rows       : AdaBase.Affected_Rows;
      Row            : AdaBase.Results.Sets.Datarow;
      Hour_Mark      : Local_Defs.Hours;
   begin
--      if Local_Defs.Do_Terminal_Debug then
--         Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " b : " & NS1 & NS2 & NS3);
--         Ada.Text_IO.New_Line;
--      end if;

      if STMT.execute then
         if STMT.rows_returned = 0 then
            DB_Connect_Res := Local_Defs.TFalse;
            return;
         else
            DB_Connect_Res := Local_Defs.TTrue;
         end if;
      else
         DB_Connect_Res := Local_Defs.TBroken;
         return;
      end if;

      Num_Rows := STMT.rows_returned;

      if Num_Rows > 0 then
         process_loops  :
         loop
            declare
               Data : Structures.Interface_Day_Record_Type;
            begin
               Row := STMT.fetch_next;
               exit process_loops when Row.data_exhausted;
               Hour_Mark                                                     := Local_Defs.Hours ((Interfaces.C.long (Row.column  (1).as_nbyte8) - Ada.Calendar.Conversions.To_Unix_Time (TS_Start)) / (60 * 60));
               Data.TS                                                       := Interfaces.C.long                    (Row.column  (1).as_nbyte8);
               Data.ifIndex                                                  := Integer                              (Row.column  (2).as_nbyte4);
               Data.ifAdminStatus                                            := Integer                              (Row.column  (3).as_nbyte4);
               Data.ifOperStatus                                             := Integer                              (Row.column  (4).as_nbyte4);
               Data.ifLastChange                                             := Local_Defs.Counter64                 (Row.column  (5).as_nbyte4);
               Data.Data_Available                                           := True;
               Data.Counters.If_Counter_32 (Structures.ifInOctets)           := Local_Defs.Counter32                 (Row.column  (6).as_nbyte4);
               Data.Counters.If_Counter_32 (Structures.ifInDiscards)         := Local_Defs.Counter32                 (Row.column  (7).as_nbyte4);
               Data.Counters.If_Counter_32 (Structures.ifInErrors)           := Local_Defs.Counter32                 (Row.column  (8).as_nbyte4);
               Data.Counters.If_Counter_32 (Structures.ifInUnknownProtos)    := Local_Defs.Counter32                 (Row.column  (9).as_nbyte4);
               Data.Counters.If_Counter_32 (Structures.ifOutOctets)          := Local_Defs.Counter32                 (Row.column (10).as_nbyte4);
               Data.Counters.If_Counter_32 (Structures.ifOutDiscards)        := Local_Defs.Counter32                 (Row.column (11).as_nbyte4);
               Data.Counters.If_Counter_32 (Structures.ifOutErrors)          := Local_Defs.Counter32                 (Row.column (12).as_nbyte4);
               Data.Counters.If_Counter_32 (Structures.ifInMulticastPkts)    := Local_Defs.Counter32                 (Row.column (13).as_nbyte4);
               Data.Counters.If_Counter_32 (Structures.ifInBroadcastPkts)    := Local_Defs.Counter32                 (Row.column (14).as_nbyte4);
               Data.Counters.If_Counter_32 (Structures.ifOutMulticastPkts)   := Local_Defs.Counter32                 (Row.column (15).as_nbyte4);
               Data.Counters.If_Counter_32 (Structures.ifOutBroadcastPkts)   := Local_Defs.Counter32                 (Row.column (16).as_nbyte4);
               Data.Counters.If_Counter_64 (Structures.ifHCInOctets)         := Local_Defs.Counter64                 (Row.column (17).as_nbyte8);
               Data.Counters.If_Counter_64 (Structures.ifHCInUcastPkts)      := Local_Defs.Counter64                 (Row.column (18).as_nbyte8);
               Data.Counters.If_Counter_64 (Structures.ifHCInMulticastPkts)  := Local_Defs.Counter64                 (Row.column (19).as_nbyte8);
               Data.Counters.If_Counter_64 (Structures.ifHCInBroadcastPkts)  := Local_Defs.Counter64                 (Row.column (20).as_nbyte8);
               Data.Counters.If_Counter_64 (Structures.ifHCOutOctets)        := Local_Defs.Counter64                 (Row.column (21).as_nbyte8);
               Data.Counters.If_Counter_64 (Structures.ifHCOutUcastPkts)     := Local_Defs.Counter64                 (Row.column (22).as_nbyte8);
               Data.Counters.If_Counter_64 (Structures.ifHCOutMulticastPkts) := Local_Defs.Counter64                 (Row.column (23).as_nbyte8);
               Data.Counters.If_Counter_64 (Structures.ifHCOutBroadcastPkts) := Local_Defs.Counter64                 (Row.column (24).as_nbyte8);
               List_Handlers.Push_Day (Data, Hour_Mark);
            exception
               when E : others =>
                  DB_Connect_Res := Local_Defs.TFalse;
                  GNAT.Traceback.Call_Chain (Trace, Length);
                  Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                          " message: " & Ada.Exceptions.Exception_Message (E));
                  Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
                  return;
            end;
         end loop process_loops;
      end if;
   exception
      when E : others =>
         DB_Connect_Res := Local_Defs.TFalse;
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         DB_Connect_Res := Local_Defs.TBroken;
         return;
   end Get_Day_Counters;

   procedure Get_Month_Counters (TS_Start, TS_End : Ada.Calendar.Time; DB_Connect_Res : out Local_Defs.Trilean) is
      Row              : AdaBase.Results.Sets.Datarow;
      Period_Current   : Structures.Period_Type;
      Period_Next      : Structures.Period_Type;
      TS_Current_Start : Ada.Calendar.Time;
      TS_Current_End   : Ada.Calendar.Time;
      TM_Start         : Local_Defs.TM;
      Diff_Range       : Structures.Diff_Range_Array_Type;
      Data_Diff        : Structures.If_Counter_All_Type;
      IfIndex          : Integer              := 0;
      Local_ifIndex    : Integer := 21;
   begin
      GNAT.Calendar.Split_At_Locale (TS_Start,      TM_Start.Year,   TM_Start.Month,  TM_Start.Day,
                                     TM_Start.Hour, TM_Start.Minute, TM_Start.Second, TM_Start.Sub_Second);

      loop
         TS_Current_Start := TS_Start;

         while TS_Current_Start < TS_End loop
            Period_Current   := Holiday.Get_Type (TS_Current_Start);

            if Period_Current = Structures.Unmetered then
               Period_Next := Period_Current;

               while Period_Next = Period_Current loop
                  TS_Current_End := TS_Current_Start + 24.0 * 60.0 * 60.0;  -- Upto 3 days worth
                  Period_Next := Holiday.Get_Type (TS_Current_End);
               end loop;
            elsif Period_Current = Structures.Night_Early then
               TS_Current_End := TS_Current_Start + 2.0 * 60.0 * 60.0;  --  Midnight_early, must check for DST change
            elsif Period_Current = Structures.Night_Special then
               TS_Current_End := TS_Current_Start + 4.0 * 60.0 * 60.0;  --  0:00 - 02:00 weekend
            elsif Period_Current = Structures.Weekend_Pre then
               TS_Current_End := TS_Current_Start + 2.0 * 60.0 * 60.0;  --  Weekend before 2am
            elsif Period_Current = Structures.Weekend_Post then
               TS_Current_End := TS_Current_Start + 18.0 * 60.0 * 60.0;  --  Weekend after 6am
            elsif Period_Current = Structures.Morning then
               TS_Current_End := TS_Current_Start + 3.0 * 60.0 * 60.0;  --  Morning
            elsif Period_Current = Structures.Working then
               TS_Current_End := TS_Current_Start + 9.0 * 60.0 * 60.0;  --  Daytime
            elsif Period_Current = Structures.Evening then
               TS_Current_End := TS_Current_Start  + 6.0 * 60.0 * 60.0;  --  Evening
            elsif Period_Current = Structures.Holiday_Pre then
               TS_Current_End := TS_Current_Start + 2.0 * 60.0 * 60.0;  --
            elsif Period_Current = Structures.Holiday_Post then
               TS_Current_End := TS_Current_Start + 18.0 * 60.0 * 60.0;  --
            else
               raise Program_Error;
            end if;

            declare
               NS1              : constant String := "(SELECT ts, ifIndex, ifInOctets, ifInDiscards, ifInErrors, ifInUnknownProtos," &
                                    "ifOutOctets, ifOutDiscards, ifOutErrors, ifInMulticastPkts, ifInBroadcastPkts, ifOutMulticastPkts," &
                                    "ifOutBroadcastPkts, ifHCInOctets, ifHCInUcastPkts, ifHCInMulticastPkts, ifHCInBroadcastPkts," &
                                    "ifHCOutOctets, ifHCOutUcastPkts, ifHCOutMulticastPkts, ifHCOutBroadcastPkts " &
                                    "FROM counters WHERE ifIndex = " & Local_ifIndex'Image & " AND " &
                                    "TS >= " & Ada.Calendar.Conversions.To_Unix_Time (TS_Current_Start)'Image & " AND " &
                                  "(ifInDiscards!=0 OR ifInErrors!=0 OR ifInUnknownProtos!=0 OR ifOutDiscards!=0 OR " &
                                    "ifOutErrors!= 0 OR ifInMulticastPkts!= 0 OR ifInBroadcastPkts!= 0 OR ifOutMulticastPkts!=0 OR " &
                                    "ifOutBroadcastPkts!=0 OR ifHCInOctets!=0 OR ifHCInUcastPkts!=0 OR ifHCInMulticastPkts!=0 OR " &
                                    "ifHCInBroadcastPkts!=0 OR ifHCOutOctets!=0 OR ifHCOutUcastPkts!=0 OR ifHCOutMulticastPkts!=0 OR " &
                                    "ifHCOutBroadcastPkts!=0) ORDER BY ts LIMIT 1) UNION ALL " &
                                    "(SELECT ts, ifIndex, ifInOctets, ifInDiscards, ifInErrors, ifInUnknownProtos," &
                                    "ifOutOctets, ifOutDiscards, ifOutErrors, ifInMulticastPkts, ifInBroadcastPkts, ifOutMulticastPkts," &
                                    "ifOutBroadcastPkts, ifHCInOctets, ifHCInUcastPkts, ifHCInMulticastPkts, ifHCInBroadcastPkts," &
                                    "ifHCOutOctets, ifHCOutUcastPkts, ifHCOutMulticastPkts, ifHCOutBroadcastPkts " &
                                    "FROM counters WHERE ifIndex = " & Local_ifIndex'Image & " AND " &
                                    "TS <= " & Ada.Calendar.Conversions.To_Unix_Time (TS_Current_End)'Image & " AND " &
                                    "(ifInDiscards!=0 OR ifInErrors!=0 OR ifInUnknownProtos!=0 OR ifOutDiscards!=0 OR " &
                                    "ifOutErrors!= 0 OR ifInMulticastPkts!= 0 OR ifInBroadcastPkts!= 0 OR ifOutMulticastPkts!=0 OR " &
                                    "ifOutBroadcastPkts!=0 OR ifHCInOctets!=0 OR ifHCInUcastPkts!=0 OR ifHCInMulticastPkts!=0 OR " &
                                    "ifHCInBroadcastPkts!=0 OR ifHCOutOctets!=0 OR ifHCOutUcastPkts!=0 OR ifHCOutMulticastPkts!=0 OR " &
                                    "ifHCOutBroadcastPkts!=0) ORDER BY ts DESC LIMIT 1) ORDER BY ts";
               NS2              : constant String := "SELECT boot_time FROM host_boot_time where " &
                                    "boot_time>=" & Ada.Calendar.Conversions.To_Unix_Time (TS_Current_Start)'Image & " AND " &
                                  "boot_time<" & Ada.Calendar.Conversions.To_Unix_Time (TS_Current_End)'Image;
               STMT             : Stmt_Type_Local := DR.prepare (NS1);
               STMT_Boot_Time   : Stmt_Type_Local := DR.prepare (NS2);
            begin

               if Local_Defs.Do_Terminal_Debug then
                  Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " b : " & NS1);
                  Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " b : " & NS2);
                  Ada.Text_IO.New_Line;
               end if;

               if STMT.execute then
                  if STMT.rows_returned /= 2 then
                     DB_Connect_Res := Local_Defs.TFalse;
                     if STMT.rows_returned > 0 then
                        if not Row.data_exhausted then
                           Row := STMT.fetch_next;
                        end if;
                     end if;
                  else
                     for I in Structures.Diff_Range_Type loop
                        Row                                                            := STMT.fetch_next;
                        IfIndex                                                        := Integer              (Row.column  (2).as_nbyte4);
                        Diff_Range (I).If_Counter_32 (Structures.ifInOctets)           := Local_Defs.Counter32 (Row.column  (3).as_nbyte4);
                        Diff_Range (I).If_Counter_32 (Structures.ifInDiscards)         := Local_Defs.Counter32 (Row.column  (4).as_nbyte4);
                        Diff_Range (I).If_Counter_32 (Structures.ifInErrors)           := Local_Defs.Counter32 (Row.column  (5).as_nbyte4);
                        Diff_Range (I).If_Counter_32 (Structures.ifInUnknownProtos)    := Local_Defs.Counter32 (Row.column  (6).as_nbyte4);
                        Diff_Range (I).If_Counter_32 (Structures.ifOutOctets)          := Local_Defs.Counter32 (Row.column  (7).as_nbyte4);
                        Diff_Range (I).If_Counter_32 (Structures.ifOutDiscards)        := Local_Defs.Counter32 (Row.column  (8).as_nbyte4);
                        Diff_Range (I).If_Counter_32 (Structures.ifOutErrors)          := Local_Defs.Counter32 (Row.column  (9).as_nbyte4);
                        Diff_Range (I).If_Counter_32 (Structures.ifInMulticastPkts)    := Local_Defs.Counter32 (Row.column (10).as_nbyte4);
                        Diff_Range (I).If_Counter_32 (Structures.ifInBroadcastPkts)    := Local_Defs.Counter32 (Row.column (11).as_nbyte4);
                        Diff_Range (I).If_Counter_32 (Structures.ifOutMulticastPkts)   := Local_Defs.Counter32 (Row.column (12).as_nbyte4);
                        Diff_Range (I).If_Counter_32 (Structures.ifOutBroadcastPkts)   := Local_Defs.Counter32 (Row.column (13).as_nbyte4);
                        Diff_Range (I).If_Counter_64 (Structures.ifHCInOctets)         := Local_Defs.Counter64 (Row.column (14).as_nbyte8);
                        Diff_Range (I).If_Counter_64 (Structures.ifHCInUcastPkts)      := Local_Defs.Counter64 (Row.column (15).as_nbyte8);
                        Diff_Range (I).If_Counter_64 (Structures.ifHCInMulticastPkts)  := Local_Defs.Counter64 (Row.column (16).as_nbyte8);
                        Diff_Range (I).If_Counter_64 (Structures.ifHCInBroadcastPkts)  := Local_Defs.Counter64 (Row.column (17).as_nbyte8);
                        Diff_Range (I).If_Counter_64 (Structures.ifHCOutOctets)        := Local_Defs.Counter64 (Row.column (18).as_nbyte8);
                        Diff_Range (I).If_Counter_64 (Structures.ifHCOutUcastPkts)     := Local_Defs.Counter64 (Row.column (19).as_nbyte8);
                        Diff_Range (I).If_Counter_64 (Structures.ifHCOutMulticastPkts) := Local_Defs.Counter64 (Row.column (20).as_nbyte8);
                        Diff_Range (I).If_Counter_64 (Structures.ifHCOutBroadcastPkts) := Local_Defs.Counter64 (Row.column (21).as_nbyte8);
                     end loop;

                     if STMT_Boot_Time.execute then
                        if STMT_Boot_Time.rows_returned /= 0 then
                           for I in Structures.If_Counter_32_Type'Range loop
                              Diff_Range (Structures.Early).If_Counter_32 (I) := 0;
                           end loop;
                           for I in Structures.If_Counter_64_Type'Range loop
                              Diff_Range (Structures.Early).If_Counter_64 (I) := 0;
                           end loop;
                        end if;
                     end if;

                     Counter_Arithmetic.Diff_Month (Data_Diff, Diff_Range (Structures.Early), Diff_Range (Structures.late));

                     if Local_Defs.Do_Terminal_Debug then
                        Ada.Text_IO.Put_Line ("Start: " & GNAT.Calendar.Time_IO.Image (TS_Current_Start, "%c") &
                                                ", End: " & GNAT.Calendar.Time_IO.Image (TS_Current_End, "%c") &
                                                ", ifIndex: " & IfIndex'Image & ", Type: " & Period_Current'Image &
                                                " - " & Data_Diff (Structures.ifHCInOctets)'Image);
                     end if;
                     List_Handlers.Push_Month (TS_Start, IfIndex, Data_Diff, Period_Current);
                  end if;
               end if;
            exception
               when E : others =>
                  GNAT.Traceback.Call_Chain (Trace, Length);
                  Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                          " message: " & Ada.Exceptions.Exception_Message (E));
                  Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
            end;

            TS_Current_Start := TS_Current_End;
         end loop;

         if Local_ifIndex = 21 then
            Local_ifIndex := 30;
         elsif Local_ifIndex = 30 then
            exit;
         end if;
      end loop;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         DB_Connect_Res := Local_Defs.TBroken;
   end Get_Month_Counters;

   procedure Normalize (L : in out Ada.Calendar.Time) is
      LTM : Local_Defs.TM;
   begin
      GNAT.Calendar.Split_At_Locale (L,        LTM.Year,   LTM.Month,  LTM.Day,
                                     LTM.Hour, LTM.Minute, LTM.Second, LTM.Sub_Second);
      LTM.Hour := 0;
      LTM.Minute := 0;
      LTM.Second := 0;
      LTM.Sub_Second := 0.0;
      L := GNAT.Calendar.Time_Of_At_Locale (LTM.Year, LTM.Month,  LTM.Day,
                                            LTM.Hour, LTM.Minute, LTM.Second);
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Normalize;

   procedure Populate_IFDesc (DB_Connect_Res : out Local_Defs.Trilean) is
      STMT           : Stmt_Type_Local := DR.prepare ("SELECT hostid, interface, shortname, ifIndex, ts, last_seen from interface"); -- where last_seen >= " &
--                                                        Ada.Calendar.Conversions.To_Unix_Time (TS_Start)'Image &
--                                                        " and ts <= " & Ada.Calendar.Conversions.To_Unix_Time (TS_End)'Image);
   begin
      if STMT.execute then
         if STMT.rows_returned = 0 then
            DB_Connect_Res := Local_Defs.TFalse;
            return;
         else
            DB_Connect_Res := Local_Defs.TTrue;
         end if;
      else
         DB_Connect_Res := Local_Defs.TBroken;
         return;
      end if;

      declare
         Row_Set     : constant AdaBase.Results.Sets.Datarow_Set := STMT.fetch_all;
         Data_Record : Structures.Interface_Descripion_Record_Type;
      begin
         process_loops  :
         for row in Natural range 1 .. Row_Set'Length loop
            Data_Record.HostID    := Integer (Row_Set (row).column (1).as_nbyte4);
            Data_Record.IF_Desc   := Ada.Strings.Unbounded.To_Unbounded_String (Row_Set (row).column (2).as_string);
            Data_Record.ShortName := Ada.Strings.Unbounded.To_Unbounded_String (Row_Set (row).column (3).as_string);
            Data_Record.ifIndex   := Integer (Row_Set (row).column (4).as_nbyte4);
            Data_Record.TS        := Interfaces.C.long (Row_Set (row).column (5).as_nbyte4);
            Data_Record.Last_Seen := Interfaces.C.long (Row_Set (row).column (6).as_nbyte4);
            List_Handlers.Push_Description (Data_Record);
         end loop process_loops;
      exception
         when E : others =>
            GNAT.Traceback.Call_Chain (Trace, Length);
            Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                    GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                    " message: " & Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
      end;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Populate_IFDesc;

   function Prepare_Day_Time_String (TS_Start_l, TS_End_l : Ada.Calendar.Time) return String is
      UBS            : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("");
   begin
      for Z in Ada.Calendar.Conversions.To_Unix_Time (TS_Start_l) .. Ada.Calendar.Conversions.To_Unix_Time (TS_End_l) loop
         if Z mod (60 * 60) = 0 then
            if Z = Ada.Calendar.Conversions.To_Unix_Time (TS_Start_l) then
               UBS := Ada.Strings.Unbounded.To_Unbounded_String (" (ts=") & Z'Image;
            else
               UBS := UBS & " OR ts=" & Z'Image;
            end if;
         end if;
      end loop;

      UBS := UBS & Ada.Strings.Unbounded.To_Unbounded_String (") ");
      return Ada.Strings.Unbounded.To_String (UBS);
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         return "FAILED";
   end Prepare_Day_Time_String;

   function Prepare_Month_Time_String (TS_Start_l, TS_End_l : Ada.Calendar.Time) return String is
      UBS            : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("");
      TMP_Time       : Ada.Calendar.Time;
--      Date_Str_Left  : constant GNAT.Calendar.Time_IO.Picture_String := "%a %b %_d %T ";
--      Date_Str_Right : constant GNAT.Calendar.Time_IO.Picture_String := " %Y";
      TM_Details     : Unix_Utils.tm;
      TS_Current     : Unix_Utils.ts;
      Night_Early_C  : constant Duration := 2 * 60.0 * 60.0; --  00 - 02
      Night_Low_C    : constant Duration := 4 * 60.0 * 60.0; --  02 - 06
      Morning_C      : constant Duration := 3 * 60.0 * 60.0; --  06 - 09
      Daytime_C      : constant Duration := 9 * 60.0 * 60.0; --  09 - 18
      Evening_C      : constant Duration := 6 * 60.0 * 60.0; --  18 - 00
      DST_Offset     : constant Duration :=     60.0 * 60.0;
   begin
      TMP_Time := TS_Start_l;
      UBS := Ada.Strings.Unbounded.To_Unbounded_String (" (ts=") & Ada.Calendar.Conversions.To_Unix_Time (TMP_Time)'Image;
      TS_Current.TV_Sec  := Ada.Calendar.Conversions.To_Unix_Time (TMP_Time);
      TS_Current.TV_NSec := 0;
      Unix_Utils.localtime_r (TS_Current'Address, TM_Details'Address);

      while TMP_Time < TS_End_l loop

         if Holiday.Is_Weekend (TMP_Time) or else Holiday.Is_Holiday (TMP_Time) or else Holiday.Is_Unmetered (TMP_Time) then
            --  If Last Sunday in either March OR October, then we add OR subtract DST offset
            TMP_Time := Ada.Calendar.Arithmetic."+" (TMP_Time, 1);

            if Holiday.Is_Hour_Long (TMP_Time) then
               TMP_Time := TMP_Time - DST_Offset;
            elsif Holiday.Is_Hour_Short (TMP_Time) then
               TMP_Time := TMP_Time + DST_Offset;
            end if;

         else
            --  Night_Early : constant Duration := 2 * 60.0 * 60.0; --  00 - 02
            --  Night_Low   : constant Duration := 4 * 60.0 * 60.0; --  02 - 06
            --  Morning     : constant Duration := 3 * 60.0 * 60.0; --  06 - 09
            --  Daytime     : constant Duration := 9 * 60.0 * 60.0; --  09 - 18
            --  Evening     : constant Duration := 6 * 60.0 * 60.0; --  18 - 00

            TMP_Time := TMP_Time + Night_Early_C;
            UBS := UBS & " OR ts=" & Ada.Calendar.Conversions.To_Unix_Time (TMP_Time)'Image;
            TS_Current.TV_Sec  := Ada.Calendar.Conversions.To_Unix_Time (TMP_Time);
            TS_Current.TV_NSec := 0;
            Unix_Utils.localtime_r (TS_Current'Address, TM_Details'Address);

            TMP_Time := TMP_Time + Night_Low_C;
            UBS := UBS & " OR ts=" & Ada.Calendar.Conversions.To_Unix_Time (TMP_Time)'Image;
            TS_Current.TV_Sec  := Ada.Calendar.Conversions.To_Unix_Time (TMP_Time);
            TS_Current.TV_NSec := 0;
            Unix_Utils.localtime_r (TS_Current'Address, TM_Details'Address);

            TMP_Time := TMP_Time + Morning_C;
            UBS := UBS & " OR ts=" & Ada.Calendar.Conversions.To_Unix_Time (TMP_Time)'Image;
            TS_Current.TV_Sec  := Ada.Calendar.Conversions.To_Unix_Time (TMP_Time);
            TS_Current.TV_NSec := 0;
            Unix_Utils.localtime_r (TS_Current'Address, TM_Details'Address);

            TMP_Time := TMP_Time + Daytime_C;
            UBS := UBS & " OR ts=" & Ada.Calendar.Conversions.To_Unix_Time (TMP_Time)'Image;
            TS_Current.TV_Sec  := Ada.Calendar.Conversions.To_Unix_Time (TMP_Time);
            TS_Current.TV_NSec := 0;
            Unix_Utils.localtime_r (TS_Current'Address, TM_Details'Address);

            TMP_Time := TMP_Time + Evening_C;
         end if;

         UBS := UBS & " OR ts=" & Ada.Calendar.Conversions.To_Unix_Time (TMP_Time)'Image;
         TS_Current.TV_Sec  := Ada.Calendar.Conversions.To_Unix_Time (TMP_Time);
         TS_Current.TV_NSec := 0;
         Unix_Utils.localtime_r (TS_Current'Address, TM_Details'Address);
      end loop;

      UBS := UBS & Ada.Strings.Unbounded.To_Unbounded_String (")");
      return Ada.Strings.Unbounded.To_String (UBS);
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         return "FAILED";
   end Prepare_Month_Time_String;

   procedure Set_Account_Details (Host, DB, User, Pass : Ada.Strings.Unbounded.Unbounded_String; Port : Integer := 3306) is
   begin
      Database := DB;
      DB_Host  := Host;
      DB_User  := User;
      DB_Pass  := Pass;
      DB_Port  := Port;
   end Set_Account_Details;
end DB_Routines;
