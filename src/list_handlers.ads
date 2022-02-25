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
with Ada.Containers.Ordered_Maps; use Ada.Containers;
with Ada.Strings.Unbounded;
with Structures; use Structures;
with Local_Defs;

package List_Handlers is
   Print_Comma        : Boolean := False;
--  Map of maps
--  Map of interfaces contains map of hours
   package Interface_Map_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Integer, -- Hours & TS
      Element_Type => Structures.Interface_Data_Record_Type);
   subtype Interface_Cursor is Interface_Map_Package.Cursor;

   package Interface_Description_Map_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Integer, -- Hours & TS
      Element_Type => Structures.Interface_Descripion_Record_Type);
   subtype Interface_Description_Cursor is Interface_Description_Map_Package.Cursor;

   package Month_Map_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Integer, -- ifIndex
      Element_Type => Structures.Month_Data_Record_Type);
   subtype Month_Cursor is Month_Map_Package.Cursor;

   procedure Erase;
--   procedure Print_Interface_Map;
   procedure Print_Month_Map;
   procedure Push_Day                 (If_Rec  : Structures.Interface_Day_Record_Type;   Hour_Mark : Local_Defs.Hours);
   procedure Push_Description         (If_Desc : Structures.Interface_Descripion_Record_Type);
   procedure Push_Month               (TS      : Ada.Calendar.Time; IfIndex : Integer; Data : Structures.If_Counter_All_Type; Period_Current : Structures.Period_Type);
   procedure Process_Month_Validity;
private
   procedure Diff;
   procedure Print_Counter_Header        (IfBool    :     Structures.If_32_Bool_Type);
   procedure Print_Counter_Header        (IfBool    :     Structures.If_64_Bool_Type);
   procedure Print_Counter_Header        (IfBool    :     Structures.If_All_Bool_Type);
   procedure Print_Diff_Map;
   procedure Print_Header_Interface      (Moniker   :     Ada.Strings.Unbounded.Unbounded_String);
   procedure Print_Interface             (If_Data   :     Structures.Interface_Data_Record_Type; H : Local_Defs.Hours);
   procedure Print_Interface_Counters    (Counters  :     Structures.If_Counter_32_64_Record_Type; Data_Avail_32  : Structures.If_32_Bool_Type; Data_Avail_64 : Structures.If_64_Bool_Type);
   procedure Print_Interface_Counters    (Counters  :     Structures.If_Counter_All_Type;   Data_Avail_All : Structures.If_All_Bool_Type);
   procedure Print_Tab_Or_Comma_Data     (Int       :     Integer);
   procedure Print_Tab_Or_Comma_Data     (Count32   :     Local_Defs.Counter32);
   procedure Print_Tab_Or_Comma_Data     (Count64   :     Local_Defs.Counter64);
   procedure Print_Tab_Or_Comma_Data     (Str       :     Ada.Strings.Unbounded.Unbounded_String);
   procedure Print_Tab_Or_Comma_Data     (Str       :     String);
   procedure Print_Tab_Or_Comma_No_Data;
   procedure Process_Day_Validity;
   Interface_Day_Map         : Interface_Map_Package.Map;
   Interface_Description_Map : Interface_Description_Map_Package.Map;
   Month_Map                 : Month_Map_Package.Map;
   Raw                       : Boolean := False;
end List_Handlers;
