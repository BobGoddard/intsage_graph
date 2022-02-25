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

with Ada.Calendar;
with Ada.Strings.Unbounded;
with GNAT.Calendar;
--  with System.Unsigned_Types; use System.Unsigned_Types;

package Local_Defs    is
   type Trilean       is (TTrue, TFalse, TBroken);
   type Hours         is new Natural range 0 .. 25;           -- 25 is extra hour on DST change
   for Hours'Size     use 5;
   type Counter32     is mod 2 ** 32;
   for Counter32'Size use 32;
   type Counter64     is mod 2 ** Long_Long_Integer'Size;
   for Counter64'Size use  64;
   Do_Terminal       : Boolean := True;
   Do_Terminal_Debug : Boolean := False;

   type ifAdminStatus is (
                          Up,
                          Down,
                          Testing);
   for ifAdminStatus use (
                          Up => 1,
                          Down => 2,
                          Testing => 3);
   type ifOperStatus is (
                         up,
                         down,
                         testing,
                         unknown,
                         dormant,
                         notPresent,
                         lowerLayerDown);
   for ifOperStatus use (
                         up => 1,
                         down => 2,
                         testing => 3,
                         unknown => 4,
                         dormant => 5,
                         notPresent => 6,
                         lowerLayerDown => 7);

   type If_Admin_Status_Type is array (ifAdminStatus) of Ada.Strings.Unbounded.Unbounded_String;
   type If_Oper_Status_Type  is array (ifOperStatus)  of Ada.Strings.Unbounded.Unbounded_String;
   If_Admin_Status_Name : constant If_Admin_Status_Type := (Ada.Strings.Unbounded.To_Unbounded_String ("Up"),      Ada.Strings.Unbounded.To_Unbounded_String ("Down"),    Ada.Strings.Unbounded.To_Unbounded_String ("Testing"));
   If_Oper_Status_Name  : constant If_Oper_Status_Type  := (Ada.Strings.Unbounded.To_Unbounded_String ("Up"),      Ada.Strings.Unbounded.To_Unbounded_String ("Down"),    Ada.Strings.Unbounded.To_Unbounded_String ("Testing"),
                                                            Ada.Strings.Unbounded.To_Unbounded_String ("Unknown"), Ada.Strings.Unbounded.To_Unbounded_String ("Dormant"), Ada.Strings.Unbounded.To_Unbounded_String ("NotPresent"),
                                                            Ada.Strings.Unbounded.To_Unbounded_String ("LowerLayerDown"));

   type TM is record
      Year       : Ada.Calendar.Year_Number;
      Month      : Ada.Calendar.Month_Number;
      Day        : Ada.Calendar.Day_Number;
      Hour       : GNAT.Calendar.Hour_Number;
      Minute     : GNAT.Calendar.Minute_Number;
      Second     : GNAT.Calendar.Second_Number;
      Sub_Second : GNAT.Calendar.Second_Duration;
   end record;
end Local_Defs;
