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
with Structures;

package Holiday is
   function  Get_Easter         (Y : Ada.Calendar.Year_Number) return Ada.Calendar.Time;
   function  Get_Type           (T : Ada.Calendar.Time)        return Structures.Period_Type;
   procedure Print_Easter_Date  (T : Ada.Calendar.Time);
   function  Is_Hour_Long       (T : Ada.Calendar.Time)        return Boolean;
   function  Is_Hour_Short      (T : Ada.Calendar.Time)        return Boolean;
   function  Is_Holiday         (T : Ada.Calendar.Time)        return Boolean;
   function  Is_Unmetered       (T : Ada.Calendar.Time)        return Boolean;
   function  Is_Weekend         (T : Ada.Calendar.Time)        return Boolean;
end Holiday;
