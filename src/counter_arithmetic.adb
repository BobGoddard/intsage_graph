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
package body Counter_Arithmetic is
   procedure Add (Result : in out Structures.If_Counter_All_Type; R : Structures.If_Counter_All_Type) is
   begin
      for I64 in Structures.If_Counter_All_Type'Range loop
         Result (I64) := Result (I64) + R (I64);
      end loop;
   end Add;

   procedure Add  (Count64 : out Local_Defs.Counter64; Count32_L, Count32_H : Local_Defs.Counter32) is
   begin
      Count64 := Local_Defs.Counter64 (Count32_H) + Local_Defs.Counter64 (Count32_L);
   end Add;

   procedure Add  (Count64 : out Local_Defs.Counter64; Count32_L : Local_Defs.Counter32; Count64_H : Local_Defs.Counter64) is
   begin
      Count64 := Local_Defs.Counter64 (Count32_L) + Count64_H;
   end Add;

   procedure Add  (Count64 : out Local_Defs.Counter64; Count64_L : Local_Defs.Counter64; Count32_H : Local_Defs.Counter32) is
   begin
      Count64 := Local_Defs.Counter64 (Count32_H) + Count64_L;
   end Add;

   procedure Add  (Count64 : out Local_Defs.Counter64; Count64_L, Count64_H : Local_Defs.Counter64) is
   begin
      Count64 := Count64_H + Count64_L;
   end Add;

   procedure Calculate_Diff  (Count32 : out Local_Defs.Counter32; Count32_L, Count32_H : Local_Defs.Counter32) is
   begin
      if Count32_L > Count32_H then
         Count32 := Local_Defs.Counter32'Last - (Count32_L - Count32_H) + 1;
      else
         Count32 := Count32_H - Count32_L;
      end if;
   end Calculate_Diff;

   procedure Calculate_Diff  (Count64 : out Local_Defs.Counter64; Count32_L, Count32_H : Local_Defs.Counter32) is
   begin
      if Count32_L > Count32_H then
         Count64 := Local_Defs.Counter64 (Local_Defs.Counter32'Last - (Count32_L - Count32_H) + 1);
      else
         Count64 := Local_Defs.Counter64 (Count32_H - Count32_L);
      end if;
   end Calculate_Diff;

   procedure Calculate_Diff  (Count64 : out Local_Defs.Counter64; Count64_L, Count64_H : Local_Defs.Counter64) is
   begin
      if Count64_L > Count64_H then
         Count64 := Local_Defs.Counter64'Last - (Count64_L - Count64_H) + 1;
      else
         Count64 := Count64_H - Count64_L;
      end if;
   end Calculate_Diff;

   procedure Diff_Month (Result : out Structures.If_Counter_All_Type; L, R : Structures.If_Counter_32_64_Record_Type) is
   begin
      Calculate_Diff (Result (Structures.ifInOctets),         L.If_Counter_32 (Structures.ifInOctets),         R.If_Counter_32 (Structures.ifInOctets));
      Calculate_Diff (Result (Structures.ifInDiscards),       L.If_Counter_32 (Structures.ifInDiscards),       R.If_Counter_32 (Structures.ifInDiscards));
      Calculate_Diff (Result (Structures.ifInErrors),         L.If_Counter_32 (Structures.ifInErrors),         R.If_Counter_32 (Structures.ifInErrors));
      Calculate_Diff (Result (Structures.ifInUnknownProtos),  L.If_Counter_32 (Structures.ifInUnknownProtos),  R.If_Counter_32 (Structures.ifInUnknownProtos));
      Calculate_Diff (Result (Structures.ifOutOctets),        L.If_Counter_32 (Structures.ifOutOctets),        R.If_Counter_32 (Structures.ifOutOctets));
      Calculate_Diff (Result (Structures.ifOutDiscards),      L.If_Counter_32 (Structures.ifOutDiscards),      R.If_Counter_32 (Structures.ifOutDiscards));
      Calculate_Diff (Result (Structures.ifOutErrors),        L.If_Counter_32 (Structures.ifOutErrors),        R.If_Counter_32 (Structures.ifOutErrors));
      Calculate_Diff (Result (Structures.ifInMulticastPkts),  L.If_Counter_32 (Structures.ifInMulticastPkts),  R.If_Counter_32 (Structures.ifInMulticastPkts));
      Calculate_Diff (Result (Structures.ifInBroadcastPkts),  L.If_Counter_32 (Structures.ifInBroadcastPkts),  R.If_Counter_32 (Structures.ifInBroadcastPkts));
      Calculate_Diff (Result (Structures.ifOutMulticastPkts), L.If_Counter_32 (Structures.ifOutMulticastPkts), R.If_Counter_32 (Structures.ifOutMulticastPkts));
      Calculate_Diff (Result (Structures.ifOutBroadcastPkts), L.If_Counter_32 (Structures.ifOutBroadcastPkts), R.If_Counter_32 (Structures.ifOutBroadcastPkts));

      Calculate_Diff (Result (Structures.ifHCInOctets),         L.If_Counter_64 (Structures.ifHCInOctets),         R.If_Counter_64 (Structures.ifHCInOctets));
      Calculate_Diff (Result (Structures.ifHCInUcastPkts),      L.If_Counter_64 (Structures.ifHCInUcastPkts),      R.If_Counter_64 (Structures.ifHCInUcastPkts));
      Calculate_Diff (Result (Structures.ifHCInMulticastPkts),  L.If_Counter_64 (Structures.ifHCInMulticastPkts),  R.If_Counter_64 (Structures.ifHCInMulticastPkts));
      Calculate_Diff (Result (Structures.ifHCInBroadcastPkts),  L.If_Counter_64 (Structures.ifHCInBroadcastPkts),  R.If_Counter_64 (Structures.ifHCInBroadcastPkts));
      Calculate_Diff (Result (Structures.ifHCOutOctets),        L.If_Counter_64 (Structures.ifHCOutOctets),        R.If_Counter_64 (Structures.ifHCOutOctets));
      Calculate_Diff (Result (Structures.ifHCOutUcastPkts),     L.If_Counter_64 (Structures.ifHCOutUcastPkts),     R.If_Counter_64 (Structures.ifHCOutUcastPkts));
      Calculate_Diff (Result (Structures.ifHCOutMulticastPkts), L.If_Counter_64 (Structures.ifHCOutMulticastPkts), R.If_Counter_64 (Structures.ifHCOutMulticastPkts));
      Calculate_Diff (Result (Structures.ifHCOutBroadcastPkts), L.If_Counter_64 (Structures.ifHCOutBroadcastPkts), R.If_Counter_64 (Structures.ifHCOutBroadcastPkts));
   end Diff_Month;
end Counter_Arithmetic;
