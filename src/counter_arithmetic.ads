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

with Local_Defs; use Local_Defs;
with Structures; use Structures;

package  Counter_Arithmetic is
   procedure Add             (Result  : in out Structures.If_Counter_All_Type;   R                    : Structures.If_Counter_All_Type);
   procedure Add             (Count64 : out     Local_Defs.Counter64;            Count32_L, Count32_H : Local_Defs.Counter32);
   procedure Add             (Count64 : out     Local_Defs.Counter64;            Count32_L : Local_Defs.Counter32; Count64_H : Local_Defs.Counter64);
   procedure Add             (Count64 : out     Local_Defs.Counter64;            Count64_L : Local_Defs.Counter64; Count32_H : Local_Defs.Counter32);
   procedure Add             (Count64 : out     Local_Defs.Counter64;            Count64_L, Count64_H : Local_Defs.Counter64);
   procedure Calculate_Diff  (Count32 : out     Local_Defs.Counter32;            Count32_L, Count32_H : Local_Defs.Counter32);
   procedure Calculate_Diff  (Count64 : out     Local_Defs.Counter64;            Count32_L, Count32_H : Local_Defs.Counter32);
   procedure Calculate_Diff  (Count64 : out     Local_Defs.Counter64;            Count64_L, Count64_H : Local_Defs.Counter64);
   procedure Diff_Month      (Result :  out     Structures .If_Counter_All_Type; L, R                 : Structures.If_Counter_32_64_Record_Type);
end Counter_Arithmetic;
