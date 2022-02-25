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

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Arithmetic;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Calendar;
with GNAT.Calendar.Time_IO;
with GNAT.Command_Line;
with GNAT.Source_Info;
with GNAT.Traceback;
with GNAT.Traceback.Symbolic;
with Config_Handler;
with DB_Routines;
with List_Handlers;
with Local_Defs; use Local_Defs;

procedure Intsage_Graph is
   DB_Connect_Res    : Local_Defs.Trilean := Local_Defs.TFalse;
   Local_Time        : Ada.Calendar.Time;
   Start_Time        : Ada.Calendar.Time;
   End_Time          : Ada.Calendar.Time;
   Year_Long         : Long_Integer := 0;
   Month_Long        : Long_Integer := 0;
   Day_Long          : Long_Integer := 0;
   LTM               : Local_Defs.TM;
   CTM               : Local_Defs.TM;
   Given_Year        : Boolean := False;
   Given_Month       : Boolean := False;
   Given_Day         : Boolean := False;
   Trace             : GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
   Length            : Natural;
   XML_Settings      : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("/etc/intsage.xml");

   procedure Set_Day_time;
   procedure Set_Month_time;

   procedure Set_Day_time is
   begin
      GNAT.Calendar.Split_At_Locale (Local_Time, CTM.Year, CTM.Month, CTM.Day, CTM.Hour, CTM.Minute, CTM.Second, CTM.Sub_Second);

      if CTM.Hour = 0 and then CTM.Minute < 15 then
         Local_Time := Ada.Calendar.Arithmetic."-" (Local_Time, 1);
      end if;

      GNAT.Calendar.Split_At_Locale (Local_Time, CTM.Year, CTM.Month, CTM.Day, CTM.Hour, CTM.Minute, CTM.Second, CTM.Sub_Second);

      if Given_Day then
         CTM.Day := LTM.Day;
      end if;
      if Given_Month then
         CTM.Month := LTM.Month;
      end if;
      if Given_Year then
         CTM.Year  := LTM.Year;
      end if;

      if CTM.Month = 2 and then CTM.Day > 28 then
         CTM.Day := 29;
         if CTM.Year rem 400 /= 0 or else CTM.Year rem 4 /= 0 then     --  Is not a leap year
            CTM.Day := 28;
         end if;
      end if;

      Start_Time  := GNAT.Calendar.Time_Of_At_Locale (CTM.Year, CTM.Month, CTM.Day, 0, 0, 0, 0.0);
      End_Time    := Ada.Calendar."+" (Start_Time, 2.0 * 60.0 * 60.0);
      GNAT.Calendar.Split_At_Locale (End_Time, CTM.Year, CTM.Month, CTM.Day, CTM.Hour, CTM.Minute, CTM.Second, CTM.Sub_Second);
      End_Time    := GNAT.Calendar.Time_Of_At_Locale (CTM.Year, CTM.Month, CTM.Day, 0, 0, 0, 0.0);
   end Set_Day_time;

   procedure Set_Month_time is
   begin
      GNAT.Calendar.Split_At_Locale (Local_Time, CTM.Year, CTM.Month, CTM.Day, CTM.Hour, CTM.Minute, CTM.Second, CTM.Sub_Second);

      if CTM.Hour = 0 and then CTM.Minute < 15 then
         Local_Time := Ada.Calendar.Arithmetic."-" (Local_Time, 1);
      end if;

      GNAT.Calendar.Split_At_Locale (Local_Time, CTM.Year, CTM.Month, CTM.Day, CTM.Hour, CTM.Minute, CTM.Second, CTM.Sub_Second);

      if Given_Month then
         CTM.Month := LTM.Month;
      end if;
      if Given_Year then
         CTM.Year  := LTM.Year;
      end if;

      Start_Time     := GNAT.Calendar.Time_Of_At_Locale (CTM.Year, CTM.Month, 1, 0, 0, 0, 0.0);
      End_Time       := Ada.Calendar.Arithmetic."+" (Start_Time, 32);
      GNAT.Calendar.Split_At_Locale (End_Time, CTM.Year, CTM.Month, CTM.Day, CTM.Hour, CTM.Minute, CTM.Second, CTM.Sub_Second);
      End_Time       := GNAT.Calendar.Time_Of_At_Locale (CTM.Year, CTM.Month, 1, 0, 0, 0, 0.0);
   end Set_Month_time;

begin
   GNAT.Calendar.Split_At_Locale (Ada.Calendar.Clock, LTM.Year, LTM.Month, LTM.Day, LTM.Hour, LTM.Minute, LTM.Second, LTM.Sub_Second);

   loop
      case GNAT.Command_Line.Getopt ("y: m: d: x: ") is
         when 'y' =>
            Year_Long := Long_Integer'Value (GNAT.Command_Line.Parameter);
            Given_Year := True;
         when 'm' =>
            Month_Long := Long_Integer'Value (GNAT.Command_Line.Parameter);
            Given_Month := True;
         when 'd' =>
            Day_Long := Long_Integer'Value (GNAT.Command_Line.Parameter);
            Given_Day := True;
         when 'x' =>
            XML_Settings := Ada.Strings.Unbounded.To_Unbounded_String (GNAT.Command_Line.Parameter);
         when ASCII.NUL => exit;
            when others => raise Program_Error;
      end case;
   end loop;

   if Given_Day then
      if Day_Long >= 1 and then Day_Long <= 28 then
         LTM.Day := Ada.Calendar.Day_Number (Day_Long);
         Ada.Text_IO.Put_Line ("Day set to " & LTM.Day'Image);
      else
         raise Program_Error;
      end if;
   end if;

   if Given_Month then
      if Month_Long >= 1 and then Month_Long <= 12 then
         LTM.Month := Ada.Calendar.Month_Number (Month_Long);
         Ada.Text_IO.Put_Line ("Month set to " & LTM.Month'Image);
      else
         raise Program_Error;
      end if;
   end if;

   if Given_Year then
      if Year_Long >= 2000 and then Year_Long <= 2038 then
         LTM.Year := Ada.Calendar.Year_Number (Year_Long);
         Ada.Text_IO.Put_Line ("Year set to " & LTM.Year'Image);
      else
         raise Program_Error;
      end if;
   end if;

   Config_Handler.Set_Config_Name (XML_Settings);
   Config_Handler.Load_Config;
   DB_Connect_Res := DB_Routines.DB_Connect;

   if DB_Connect_Res = Local_Defs.TTrue then
      Local_Time := Ada.Calendar.Clock;
      Set_Day_time;
      DB_Routines.Populate_IFDesc (DB_Connect_Res);
      DB_Routines.Get_Day_Counters (Start_Time, End_Time, DB_Connect_Res);
--      Counter_Arithmetic.Diff;
      List_Handlers.Print_Comma := True;

--      if Local_Defs.Do_Terminal then
--         List_Handlers.Print_Diff_Map;
--      end if;

      Local_Time := Ada.Calendar.Clock;
      Set_Month_time;

      if Local_Defs.Do_Terminal_Debug then
         Ada.Text_IO.Put_Line ("ST: " & GNAT.Calendar.Time_IO.Image (Start_Time, "%c") & ", ET: " & GNAT.Calendar.Time_IO.Image (End_Time, "%c"));
      end if;

      DB_Routines.Get_Month_Counters (Start_Time, End_Time, DB_Connect_Res);
      List_Handlers.Process_Month_Validity;

      if Local_Defs.Do_Terminal then
         List_Handlers.Print_Month_Map;
      end if;

      if Local_Defs.Do_Terminal_Debug then
         Ada.Text_IO.Put ("DB_Connect_Res: " & Local_Defs.TTrue'Image);
      end if;

      DB_Routines.DB_Disconnect;
   end if;
exception
   when E : others =>
      GNAT.Traceback.Call_Chain (Trace, Length);
      Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                       GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) &
                                       " message: " & Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
end Intsage_Graph;
