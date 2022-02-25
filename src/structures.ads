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
with Interfaces.C;
with Local_Defs;

package Structures is
   type Diff_Range_Type          is (Early,             late);
   type Period_Type              is (Unmetered,         Weekend_Pre,          Weekend_Post,         Holiday_Pre,      Holiday_Post,
                                     Night_Early,       Night_Special,        Morning,              Working,          Evening);
   type If_32_Type               is (ifInOctets,        ifInDiscards,         ifInErrors,           ifInUnknownProtos,
                                     ifOutOctets,       ifOutDiscards,        ifOutErrors,          ifInMulticastPkts,
                                     ifInBroadcastPkts, ifOutMulticastPkts,   ifOutBroadcastPkts);                          --  11
   type If_64_Type               is (ifHCInOctets,      ifHCInUcastPkts,      ifHCInMulticastPkts,  ifHCInBroadcastPkts,
                                     ifHCOutOctets,     ifHCOutUcastPkts,     ifHCOutMulticastPkts, ifHCOutBroadcastPkts);  --  9
   type If_All_Type              is (ifInOctets,        ifInDiscards,         ifInErrors,           ifInUnknownProtos,
                                     ifOutOctets,       ifOutDiscards,        ifOutErrors,          ifInMulticastPkts,
                                     ifInBroadcastPkts, ifOutMulticastPkts,   ifOutBroadcastPkts,   ifHCInOctets,
                                     ifHCInUcastPkts,   ifHCInMulticastPkts,  ifHCInBroadcastPkts,  ifHCOutOctets,
                                     ifHCOutUcastPkts,  ifHCOutMulticastPkts, ifHCOutBroadcastPkts);
   type If_Counter_32_Type       is array (If_32_Type)  of Local_Defs.Counter32;
   type If_Counter_64_Type       is array (If_64_Type)  of Local_Defs.Counter64;
   type If_Counter_All_Type      is array (If_All_Type) of Local_Defs.Counter64;
   type If_32_Bool_Type          is array (If_32_Type)  of Boolean;
   type If_64_Bool_Type          is array (If_64_Type)  of Boolean;
   type If_All_Bool_Type         is array (If_All_Type) of Boolean;
   type If_Counter_32_Name_Type  is array (If_32_Type)  of Ada.Strings.Unbounded.Unbounded_String;
   type If_Counter_64_Name_Type  is array (If_64_Type)  of Ada.Strings.Unbounded.Unbounded_String;
   type If_Counter_All_Name_Type is array (If_All_Type) of Ada.Strings.Unbounded.Unbounded_String;

   If_Counter_32_Name : constant If_Counter_32_Name_Type :=   (Ada.Strings.Unbounded.To_Unbounded_String ("ifInOctets"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifInDiscards"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifInErrors"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifInUnknownProtos"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifOutOctets"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifOutDiscards"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifOutErrors"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifInMulticastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifInBroadcastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifOutMulticastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifOutBroadcastPkts"));
   If_Counter_64_Name : constant If_Counter_64_Name_Type :=   (Ada.Strings.Unbounded.To_Unbounded_String ("ifHCInOctets"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifHCInUcastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifHCInMulticastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifHCInBroadcastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifHCOutOctets"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifHCOutUcastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifHCOutMulticastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifHCOutBroadcastPkts"));
   If_Counter_All_Name : constant If_Counter_All_Name_Type := (Ada.Strings.Unbounded.To_Unbounded_String ("ifInOctets"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifInDiscards"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifInErrors"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifInUnknownProtos"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifOutOctets"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifOutDiscards"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifOutErrors"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifInMulticastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifInBroadcastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifOutMulticastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifOutBroadcastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifHCInOctets"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifHCInUcastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifHCInMulticastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifHCInBroadcastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifHCOutOctets"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifHCOutUcastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifHCOutMulticastPkts"),
                                                               Ada.Strings.Unbounded.To_Unbounded_String ("ifHCOutBroadcastPkts"));

   type If_Counter_32_64_Record_Type is record
      If_Counter_32         : If_Counter_32_Type    := (others => 0);
      If_Counter_64         : If_Counter_64_Type    := (others => 0);
   end record;

   type Interface_Descripion_Record_Type is record
      IF_Desc   : Ada.Strings.Unbounded.Unbounded_String;
      ShortName : Ada.Strings.Unbounded.Unbounded_String;
      HostID    : Integer;
      ifIndex   : Integer;
      TS        : Interfaces.C.long;
      Last_Seen : Interfaces.C.long;
   end record;

   type Interface_Day_Record_Type is record
      TS                         : Interfaces.C.long    := 0;
      ifIndex                    : Integer              := 0;
      ifAdminStatus              : Integer              := 0; -- INTEGER,    7
      ifOperStatus               : Integer              := 0; -- INTEGER,    8
      ifLastChange               : Local_Defs.Counter64 := 0; -- TimeTicks,  9
      Data_Available             : Boolean              := False; --  Will be true if any of Counters.Column_Data_Available are true
      Data_Available_Diff        : Boolean              := False; --  Will be true if any of Counters.Column_Data_Available are true
      Counters                   : If_Counter_32_64_Record_Type;
   end record;

   type Interface_Day_Record_Array_type  is array (Local_Defs.Hours) of Interface_Day_Record_Type;

   type Interface_Data_Record_Type is record  --  All data for each interface
      ifIndex                  : Integer              := 0;
      Interface_Data_Available : Boolean := False; --  Will be true if any of Column_Data_Available is true
      Column_32_Data_Available : If_32_Bool_Type  := (others => False);
      Column_64_Data_Available : If_64_Bool_Type  := (others => False);
      Data                     : Interface_Day_Record_Array_type;
      Data_Diff                : Interface_Day_Record_Array_type;
   end record;

   type Interface_Month_Record_Type is record
      TS                         : Interfaces.C.long   := 0;
      ifIndex                    : Integer             := 0;
      Counters                   : If_Counter_All_Type := (others => 0);
   end record;

   type Diff_Range_Array_Type   is array (Diff_Range_Type)  of If_Counter_32_64_Record_Type;

   type Month_Record_Array_Type is array (Period_Type) of Interface_Month_Record_Type;

   type Month_Data_Record_Type is record  -- per interface in map
      Data                      : Month_Record_Array_Type;
      Timestamp                 : Ada.Calendar.Time;
      Column_All_Data_Available : If_All_Bool_Type  := (others => False);
   end record;
end Structures;
