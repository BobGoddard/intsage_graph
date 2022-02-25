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

with Ada.Calendar.Conversions;
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO;
with Ada.Strings;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with GNAT.Calendar;
with GNAT.Calendar.Time_IO;
with GNAT.Source_Info;
with GNAT.Traceback;
with GNAT.Traceback.Symbolic;
with Counter_Arithmetic;
with Local_Defs; use Local_Defs;

package body List_Handlers is
   Trace       : GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
   Length      : Natural;

   procedure Diff is
   begin
      Ada.Long_Long_Integer_Text_IO.Default_Width := 0; -- Might as well do it just once here

      if Interface_Day_Map.Length = 0 then
         return;
      end if;

      Ada.Integer_Text_IO.Default_Width := 0;
      Ada.Long_Integer_Text_IO.Default_Width := 0;

      for IfIndex_Cursor in Interface_Day_Map.Iterate loop
         declare
            Rec_Array : Structures.Interface_Data_Record_Type := Interface_Day_Map.Reference (IfIndex_Cursor);
         begin
            for H in Local_Defs.Hours'First .. Local_Defs.Hours'Last - 1 loop
               if Rec_Array.Data (H).Data_Available and then Rec_Array.Data (H + 1).Data_Available then
                  declare
                     Low  : constant Structures.Interface_Day_Record_Type := Rec_Array.Data (H);
                     High : constant Structures.Interface_Day_Record_Type := Rec_Array.Data (H + 1);
                  begin
                     Rec_Array.Data_Diff (H).ifAdminStatus := High.ifAdminStatus;
                     Rec_Array.Data_Diff (H).ifOperStatus  := High.ifOperStatus;
                     Rec_Array.Data_Diff (H).ifLastChange  := High.ifLastChange;

                     for IF32 in Structures.If_32_Type'Range loop
                        Counter_Arithmetic.Calculate_Diff (Rec_Array.Data_Diff (H).Counters.If_Counter_32 (IF32), Low.Counters.If_Counter_32 (IF32), High.Counters.If_Counter_32 (IF32));
                     end loop;

                     for IF64 in Structures.If_64_Type'Range loop
                        Counter_Arithmetic.Calculate_Diff (Rec_Array.Data_Diff (H).Counters.If_Counter_64 (IF64), Low.Counters.If_Counter_64 (IF64), High.Counters.If_Counter_64 (IF64));
                     end loop;

                     Rec_Array.Data_Diff (H).TS := Rec_Array.Data (H).TS;
                  end;
               end if;
            end loop;

            Interface_Day_Map.Replace_Element (IfIndex_Cursor, Rec_Array);
         exception
            when E : others =>
               GNAT.Traceback.Call_Chain (Trace, Length);
               Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                       GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                       " message: " & Ada.Exceptions.Exception_Message (E));
               Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         end;
      end loop;

      Process_Day_Validity;
   end Diff;

   procedure Erase is
   begin
      Interface_Day_Map.Clear;
      Month_Map.Clear;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Erase;

   procedure Print_Counter_Header (IfBool : Structures.If_32_Bool_Type) is
   begin
      for If32 in Structures.If_32_Type'Range loop
         if IfBool (If32) or else Raw then
            Print_Tab_Or_Comma_Data (Structures.If_Counter_32_Name (If32));
         end if;
      end loop;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print_Counter_Header;

   procedure Print_Counter_Header (IfBool : Structures.If_64_Bool_Type) is
   begin
      for If64 in Structures.If_64_Type'Range loop
         if IfBool (If64) or else Raw then
            Print_Tab_Or_Comma_Data (Structures.If_Counter_64_Name (If64));
         end if;
      end loop;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print_Counter_Header;

   procedure Print_Counter_Header (IfBool : Structures.If_All_Bool_Type) is
   begin
      for IfAll in Structures.If_All_Bool_Type'Range loop
         if IfBool (IfAll) then
            Print_Tab_Or_Comma_Data (Structures.If_Counter_All_Name (IfAll));
         end if;
      end loop;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print_Counter_Header;

   procedure Print_Diff_Map is --  (TS_Diff : Integer) is -- TS_Diff tells us if the clocks have changed.
      If_Data        : Structures.Interface_Data_Record_Type;
      U_Year         : Ada.Calendar.Year_Number;
      U_Month        : Ada.Calendar.Month_Number;
      U_Day          : Ada.Calendar.Day_Number;
      U_Hour         : GNAT.Calendar.Hour_Number;
      U_Minute       : GNAT.Calendar.Minute_Number;
      U_Second       : GNAT.Calendar.Second_Number;
      U_Sub_Second   : GNAT.Calendar.Second_Duration;
   begin
      if Interface_Day_Map.Length /= 0 then
         for C in Interface_Day_Map.Iterate loop
            If_Data := Interface_Day_Map.Reference (C);

            if If_Data.Interface_Data_Available then
               Ada.Integer_Text_IO.Put (If_Data.ifIndex);
               Print_Tab_Or_Comma_Data (Interface_Description_Map.Element (If_Data.ifIndex).IF_Desc);
               Print_Tab_Or_Comma_Data (Interface_Description_Map.Element (If_Data.ifIndex).ShortName);
               Ada.Text_IO.New_Line;
               Print_Header_Interface (Ada.Strings.Unbounded.To_Unbounded_String ("Hour"));
               Print_Counter_Header (If_Data.Column_32_Data_Available);
               Print_Counter_Header (If_Data.Column_64_Data_Available);
               Ada.Text_IO.New_Line;
               for H in Local_Defs.Hours loop
                  if If_Data.Data_Diff (H).Data_Available_Diff then
                     GNAT.Calendar.Split_At_Locale (Ada.Calendar.Conversions.To_Ada_Time (If_Data.Data_Diff (H).TS), U_Year, U_Month, U_Day, U_Hour, U_Minute, U_Second, U_Sub_Second);
                     Ada.Integer_Text_IO.Put (U_Hour);
                     Print_Interface (If_Data, H);
                     Ada.Text_IO.New_Line;
                  end if;
               end loop;

               Ada.Text_IO.New_Line;
            end if;
         end loop;
      end if;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print_Diff_Map;

   procedure Print_Header_Interface (Moniker : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if Ada.Strings.Unbounded.Length (Moniker) > 0 then
         Ada.Strings.Unbounded.Text_IO.Put (Moniker);
      end if;

      Print_Tab_Or_Comma_Data ("ifAdminStatus");
      Print_Tab_Or_Comma_Data ("ifOperStatus");
      Print_Tab_Or_Comma_Data ("ifLastChange");
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print_Header_Interface;

   procedure Print_Interface (If_Data : Structures.Interface_Data_Record_Type; H : Local_Defs.Hours) is
   begin
      Print_Tab_Or_Comma_Data (Local_Defs.If_Admin_Status_Name (ifAdminStatus'Enum_Val (If_Data.Data (H).ifAdminStatus)));
      Print_Tab_Or_Comma_Data (Local_Defs.If_Oper_Status_Name (ifOperStatus'Enum_Val (If_Data.Data (H).ifOperStatus)));
      Print_Tab_Or_Comma_Data (If_Data.Data (H).ifLastChange);
      Print_Interface_Counters (If_Data.Data_Diff (H).Counters, If_Data.Column_32_Data_Available, If_Data.Column_64_Data_Available);
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print_Interface;

   procedure Print_Interface_Counters    (Counters : Structures.If_Counter_32_64_Record_Type; Data_Avail_32 : Structures.If_32_Bool_Type; Data_Avail_64 : Structures.If_64_Bool_Type) is
   begin
      for Data in Structures.If_32_Type'Range loop
         if Data_Avail_32 (Data) or else Raw then
            Print_Tab_Or_Comma_Data (Counters.If_Counter_32 (Data));
         end if;
      end loop;

      for Data in Structures.If_64_Type'Range loop
         if Data_Avail_64 (Data) or else Raw then
            Print_Tab_Or_Comma_Data (Counters.If_Counter_64 (Data));
         end if;
      end loop;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print_Interface_Counters;

   procedure Print_Interface_Counters    (Counters : Structures.If_Counter_All_Type; Data_Avail_All : Structures.If_All_Bool_Type) is
   begin
      for Data in Structures.If_All_Type'Range loop
         if Data_Avail_All (Data) or else Raw then
            Print_Tab_Or_Comma_Data (Counters (Data));
         end if;
      end loop;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print_Interface_Counters;

   procedure Print_Month_Map is
      Month_Interface_Data : Structures.Month_Data_Record_Type;
      ATS                  : Ada.Calendar.Time;
   begin
      if Month_Map.Length = 0 then
         return;
      end if;

      Ada.Integer_Text_IO.Default_Width := 0;
      Month_Interface_Data := Month_Map.First_Element;
      ATS := Month_Interface_Data.Timestamp;

      for C in Month_Map.Iterate loop
         Month_Interface_Data := Month_Map.Reference (C);
         Ada.Integer_Text_IO.Put (Month_Map_Package.Key (C));
         Print_Tab_Or_Comma_Data (Interface_Description_Map.Element (Month_Map_Package.Key (C)).IF_Desc);
         Print_Tab_Or_Comma_Data (Interface_Description_Map.Element (Month_Map_Package.Key (C)).ShortName);
         declare
            S : constant String := GNAT.Calendar.Time_IO.Image (ATS, " %a %d %b %Y");
         begin
            Print_Tab_Or_Comma_Data (S);
         exception
            when E : others =>
               GNAT.Traceback.Call_Chain (Trace, Length);
               Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                       GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                       " message: " & Ada.Exceptions.Exception_Message (E));
               Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         end;
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("Perdiod-type");
         Print_Counter_Header (Month_Interface_Data.Column_All_Data_Available);
         Ada.Text_IO.New_Line;

         for T in Structures.Period_Type loop
            Ada.Text_IO.Put (T'Image);
            Print_Interface_Counters (Month_Interface_Data.Data (T).Counters, Month_Interface_Data.Column_All_Data_Available);
            Ada.Text_IO.New_Line;
         end loop;

         Ada.Text_IO.New_Line;
      end loop;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print_Month_Map;

   procedure Print_Tab_Or_Comma_Data (Count32 : Local_Defs.Counter32) is
   begin
      if Print_Comma then
         Ada.Text_IO.Put (",");
      else
         Ada.Text_IO.Put (Ada.Characters.Latin_1.HT);
      end if;
      Ada.Long_Long_Integer_Text_IO.Put (Long_Long_Integer (Count32));
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print_Tab_Or_Comma_Data;

   procedure Print_Tab_Or_Comma_Data (Count64 : Local_Defs.Counter64) is
   begin
      if Print_Comma then
         Ada.Text_IO.Put (",");
      else
         Ada.Text_IO.Put (Ada.Characters.Latin_1.HT);
      end if;
      --  If R is > 9223372036854775806 or 2 ^ 63 - 1, then it will error, but it would take over 2000 years on a 1Gb line...
      Ada.Long_Long_Integer_Text_IO.Put (Long_Long_Integer (Count64));
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print_Tab_Or_Comma_Data;

   procedure Print_Tab_Or_Comma_Data (Int : Integer) is
   begin
      if Print_Comma then
         Ada.Text_IO.Put (",");
      else
         Ada.Text_IO.Put (Ada.Characters.Latin_1.HT);
      end if;
      Ada.Integer_Text_IO.Put (Int);
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print_Tab_Or_Comma_Data;

   procedure Print_Tab_Or_Comma_Data (Str : String) is
   begin
      if Print_Comma then
         Ada.Text_IO.Put (",");
      else
         Ada.Text_IO.Put (Ada.Characters.Latin_1.HT);
      end if;

      Ada.Text_IO.Put (Str);
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print_Tab_Or_Comma_Data;

   procedure Print_Tab_Or_Comma_Data (Str : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if Print_Comma then
         Ada.Text_IO.Put (",");
      else
         Ada.Text_IO.Put (Ada.Characters.Latin_1.HT);
      end if;

      Ada.Strings.Unbounded.Text_IO.Put (Str);
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print_Tab_Or_Comma_Data;

   procedure Print_Tab_Or_Comma_No_Data is
   begin
      if Print_Comma then
         Ada.Text_IO.Put (",");
      else
         Ada.Text_IO.Put (Ada.Characters.Latin_1.HT);
      end if;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print_Tab_Or_Comma_No_Data;

   procedure Process_Day_Validity is
      If_Data        : Structures.Interface_Data_Record_Type;
   begin
      if Interface_Day_Map.Length = 0 then
         return;
      end if;

      for If_Data_Cursor in Interface_Day_Map.Iterate loop
         If_Data := Interface_Day_Map.Reference (If_Data_Cursor);

         for Interface_Hours in Local_Defs.Hours loop
            if If_Data.Data_Diff (Interface_Hours).Data_Available_Diff then
               for IFC32 in Structures.If_Counter_32_Type'Range loop
                  if If_Data.Data_Diff (Interface_Hours).Counters.If_Counter_32 (IFC32) /= 0 then
                     If_Data.Interface_Data_Available := True;
                     If_Data.Data_Diff (Interface_Hours).Data_Available := True;
                     If_Data.Column_32_Data_Available (IFC32) := True;
                  end if;
               end loop;
               for IFC64 in Structures.If_Counter_64_Type'Range loop
                  if If_Data.Data_Diff (Interface_Hours).Counters.If_Counter_64 (IFC64) /= 0 then
                     If_Data.Interface_Data_Available := True;
                     If_Data.Data_Diff (Interface_Hours).Data_Available := True;
                     If_Data.Column_64_Data_Available (IFC64) := True;
                  end if;
               end loop;
            end if;
         end loop;

         Interface_Day_Map.Replace_Element (If_Data_Cursor, If_Data);
      end loop;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Process_Day_Validity;

   procedure Process_Month_Validity is
      L1 : Structures.Month_Data_Record_Type;
      L2 : Structures.Interface_Month_Record_Type;
      Replace_Data : Boolean;
   begin
      Ada.Long_Long_Integer_Text_IO.Default_Width := 0;
      if Month_Map.Length = 0 then
         return;
      end if;

      for IfIndex in Month_Map.Iterate loop
         L1 := Month_Map (IfIndex);
         for PType in L1.Data'Range loop
            L2 := L1.Data (PType);
            for SNMP_Counter in Structures.If_All_Type'Range loop
               if L2.Counters (SNMP_Counter) > 0 then
                  L1.Column_All_Data_Available (SNMP_Counter) := True;
                  Replace_Data := True;
               end if;
            end loop;
         end loop;

         if Replace_Data then
            Month_Map.Replace_Element (IfIndex, L1);
         end if;
      end loop;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Process_Month_Validity;

   procedure Push_Day   (If_Rec : Structures.Interface_Day_Record_Type; Hour_Mark : Local_Defs.Hours) is
      Arr : Interface_Data_Record_Type;
   begin
      if Interface_Day_Map.Contains (If_Rec.ifIndex) then
         Arr := Interface_Day_Map.Element (If_Rec.ifIndex);
         Arr.Data (Hour_Mark) := If_Rec;
         Arr.ifIndex := If_Rec.ifIndex;
         Interface_Day_Map.Replace (If_Rec.ifIndex, Arr);
      else
         Arr.Data (Hour_Mark) := If_Rec;
         Interface_Day_Map.Insert (If_Rec.ifIndex, Arr);
      end if;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Push_Day;

   procedure Push_Description (If_Desc : Structures.Interface_Descripion_Record_Type) is
      Arr : Interface_Descripion_Record_Type;
   begin
      if Interface_Description_Map.Contains (If_Desc.ifIndex) then
         Arr := Interface_Description_Map.Element (If_Desc.ifIndex);
         Arr := If_Desc;
         Interface_Description_Map.Replace (If_Desc.ifIndex, Arr);
      else
         Arr := If_Desc;
         Interface_Description_Map.Insert (If_Desc.ifIndex, Arr);
      end if;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Push_Description;

   procedure Push_Month (TS : Ada.Calendar.Time; IfIndex : Integer; Data : Structures.If_Counter_All_Type; Period_Current : Structures.Period_Type) is
   begin
      if Month_Map.Contains (IfIndex) then
         declare
            Arr : Structures.Month_Data_Record_Type := Month_Map.Element (IfIndex);
         begin
            Arr.Data (Period_Current).ifIndex := IfIndex;
            Counter_Arithmetic.Add (Arr.Data (Period_Current).Counters, Data);
            Month_Map.Replace_Element (Month_Map.Find (IfIndex), Arr);
         exception
            when E : others =>
               GNAT.Traceback.Call_Chain (Trace, Length);
               Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                       GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                       " message: " & Ada.Exceptions.Exception_Message (E));
               Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         end;
      else
         declare
            Arr : Structures.Month_Data_Record_Type;
         begin
            Counter_Arithmetic.Add (Arr.Data (Period_Current).Counters, Data);
            Arr.Timestamp := TS;
            Month_Map.Insert (IfIndex, Arr);
         exception
            when E : others =>
               GNAT.Traceback.Call_Chain (Trace, Length);
               Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                       GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                       " message: " & Ada.Exceptions.Exception_Message (E));
               Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         end;
      end if;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Push_Month;
end List_Handlers;
