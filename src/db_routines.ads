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
with AdaBase.Statement.Base;
with AdaBase.Driver.Base.MySQL;
with AdaBase.Statement.Base.MySQL;
with Interfaces.C; use Interfaces.C;
with Local_Defs; use Local_Defs;

package DB_Routines is
   subtype  Database_Driver  is AdaBase.Driver.Base.MySQL.MySQL_Driver;
   subtype  Stmt_Type_Local  is AdaBase.Statement.Base.MySQL.MySQL_statement;
   subtype  Stmt_Type_access is AdaBase.Statement.Base.MySQL.MySQL_statement_access;

   function  DB_Connect                                                          return Local_Defs.Trilean;
   procedure DB_Disconnect;
   function  mysql_thread_init                                                   return Interfaces.C.int;
   procedure Get_Day_Counters          (TS_Start, TS_End     : Ada.Calendar.Time; DB_Connect_Res : out Local_Defs.Trilean);
   procedure Get_Month_Counters        (TS_Start, TS_End     : Ada.Calendar.Time; DB_Connect_Res : out Local_Defs.Trilean);
   procedure Populate_IFDesc                                                    (DB_Connect_Res : out Local_Defs.Trilean);
   function  Prepare_Month_Time_String (TS_Start_l, TS_End_l : Ada.Calendar.Time) return String;
   procedure Set_Account_Details       (Host, DB, User, Pass : Ada.Strings.Unbounded.Unbounded_String; Port : Integer := 3306);
   pragma    Import                    (C, mysql_thread_init, "mysql_thread_init");
private
   function  DB_Connect_Private                                                  return Local_Defs.Trilean;
   procedure Normalize                 (L : in out Ada.Calendar.Time);
   function  Prepare_Day_Time_String   (TS_Start_l, TS_End_l : Ada.Calendar.Time) return String;
   DB_Host  : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   Database : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   DB_User  : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   DB_Pass  : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   DB_Port  : Integer := 3306;
end DB_Routines;
