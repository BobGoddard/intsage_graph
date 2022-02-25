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

with Ada.Text_IO;
with GNAT.Calendar; use GNAT.Calendar;
with GNAT.Calendar.Time_IO;
with Ada.Calendar; use Ada.Calendar;

with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;

--  January holiday's -
--  New Year's Day         Fri, 1 Jan 2021   1 First weekday of year
--  2nd January            Mon, 4 Jan 2021     Second weekday of year
--  Good Friday            Fri, 2 Apr 2021   3 or 4
--  Easter Monday          Mon, 5 Apr 2021     Computed below
--  Early May Bank Holiday Mon, 3 May 2021   5 First Monday in May
--  Spring Bank Holiday    Mon, 31 May 2021    Last  Monday in May
--  Summer Bank Holiday    Mon, 2 Aug 2021   8 First MOnday in Aug
--  St. Andrew's Day       Tue, 30 Nov 2021 11 Closest to 30th Nov
--  Christmas Day          Mon, 27 Dec 2021 12 On 25 or first weekday after
--  Boxing Day             Tue, 28 Dec 2021    On 26 or first weekday after

package body Holiday is
   function Get_Easter (Y : Ada.Calendar.Year_Number) return Ada.Calendar.Time is
      A           : constant Integer := Y mod 19;
      B           : constant Integer := Y / 100;
      C           : constant Integer := Y mod 100;
      D           : constant Integer := B / 4;
      E           : constant Integer := B mod 4;
      F           : constant Integer := (B + 8) / 25;
      G           : constant Integer := (B - F + 1) / 3;
      H           : constant Integer := (19 * A + B - D - G + 15) mod 30;
      I           : constant Integer := C / 4;
      K           : constant Integer := C mod 4;
      L           : constant Integer := (32 + 2 * E + 2 * I - H - K) mod 7;
      M           : constant Integer := (A + 11 * H + 22 * L) / 451;
      N           : constant Integer := H + L - 7 * M + 114;
   begin
      return GNAT.Calendar.Time_Of_At_Locale (Y, N / 31, N mod 31 + 1, 0, 0, 0);
   end Get_Easter;

   function Get_Type (T : Ada.Calendar.Time) return Structures.Period_Type is
      U_Year            : Ada.Calendar.Year_Number;
      U_Month           : Ada.Calendar.Month_Number;
      U_Day             : Ada.Calendar.Day_Number;
      U_Hour            : GNAT.Calendar.Hour_Number;
      U_Minute          : GNAT.Calendar.Minute_Number;
      U_Second          : GNAT.Calendar.Second_Number;
      U_Sub_Second      : GNAT.Calendar.Second_Duration;
      Week_Day          : GNAT.Calendar.Day_Name;
      Easter_Time       : Ada.Calendar.Time;
      Day_Diff          : Ada.Calendar.Arithmetic.Day_Count;
      Seconds_Diff      : Duration;
      Leap_Seconds_Diff : Ada.Calendar.Arithmetic.Leap_Seconds_Count;
   begin
      Week_Day := GNAT.Calendar.Day_Of_Week (T);
      GNAT.Calendar.Split_At_Locale (T, U_Year, U_Month, U_Day, U_Hour, U_Minute, U_Second, U_Sub_Second);

      if U_Month = 12 and then U_Day > 26 and then Week_Day /= GNAT.Calendar.Saturday and then Week_Day /= GNAT.Calendar.Sunday then
         return Structures.Unmetered;  --  3 days xmas unmetered - MUST come after weekend test & xmas holiday's
      elsif U_Hour >= 2 and then U_Hour < 6 then
         return Structures.Night_Special;
      elsif Week_Day = GNAT.Calendar.Saturday or else Week_Day = GNAT.Calendar.Sunday then
         if U_Hour < 2 then
            return Structures.Weekend_Pre;
         end if;
         return Structures.Weekend_Post;
      elsif      (U_Month =  1 and then (U_Day   =  1   or else   U_Day =  2))
        or else  (U_Month =  1 and then (U_Day   =  3   or else   U_Day =  4)                       and then Week_Day <= GNAT.Calendar.Tuesday)
        or else ((U_Month =  5 or else   U_Month =  8) and then   U_Day <= 7                        and then Week_Day  = GNAT.Calendar.Monday)
        or else  (U_Month = 12 and then (U_Day   = 25   or else   U_Day = 26))
        or else  (U_Month = 12 and then (U_Day   = 27   or else   U_Day = 28)                       and then Week_Day <= GNAT.Calendar.Tuesday)
        or else ((U_Month = 11 and then  U_Day   = 30)  or else ((U_Month = 12 and then U_Day <= 2) and then Week_Day  = GNAT.Calendar.Monday))
        or else  (U_Month =  5 and then  U_Day  >= 25   and then Week_Day = GNAT.Calendar.Monday)
      then
         if U_Hour < 2 then
            return Structures.Holiday_Pre;
         else
            return Structures.Holiday_Post;
         end if;
      elsif U_Hour < 2 then
         return Structures.Night_Early;
      elsif U_Hour < 9 then
         return Structures.Morning;
      elsif U_Hour >= 18 then
         return Structures.Evening;
      else
         Easter_Time := Get_Easter (U_Year);
         Ada.Calendar.Arithmetic.Difference (T, Easter_Time, Day_Diff, Seconds_Diff, Leap_Seconds_Diff);

         if (Day_Diff = -2 and then Week_Day = GNAT.Calendar.Friday)  --  Easter Friday
           or else (Day_Diff = 1 and then Week_Day = GNAT.Calendar.Monday)
         then
            if U_Hour < 2 then
               return Structures.Holiday_Pre;
            else
               return Structures.Holiday_Post;
            end if;
         end if;
      end if;

      return Structures.Working;
   end Get_Type;

   function Is_Holiday (T : Ada.Calendar.Time) return Boolean is
      U_Year            : Ada.Calendar.Year_Number;
      U_Month           : Ada.Calendar.Month_Number;
      U_Day             : Ada.Calendar.Day_Number;
      U_Hour            : GNAT.Calendar.Hour_Number;
      U_Minute          : GNAT.Calendar.Minute_Number;
      U_Second          : GNAT.Calendar.Second_Number;
      U_Sub_Second      : GNAT.Calendar.Second_Duration;
      Week_Day          : GNAT.Calendar.Day_Name;
      Easter_Time       : Ada.Calendar.Time;
      Day_Diff          : Ada.Calendar.Arithmetic.Day_Count;
      Seconds_Diff      : Duration;
      Leap_Seconds_Diff : Ada.Calendar.Arithmetic.Leap_Seconds_Count;
   begin
      GNAT.Calendar.Split_At_Locale (T, U_Year, U_Month, U_Day, U_Hour, U_Minute, U_Second, U_Sub_Second);

      if Is_Weekend (T) then
         return False;
      end if;

      Week_Day := GNAT.Calendar.Day_Of_Week (T); --  Monday is first

      --  New years & subsequent days holiday
      if U_Month = 1 then
         if U_Day = 1 or else U_Day = 2 then
            return True; -- Must be a weekday, therefore a holiday
         end if;

         if (U_Day = 3 or else U_Day = 4) and then Week_Day <= GNAT.Calendar.Tuesday then
            return True; -- 1st must be on Saturday or Sunday therefore a holiday
         end if;
      end if;

      --  May day & August bank holiday, both first Monday in month
      if (U_Month = 5 or else U_Month = 8) and then U_Day <= 7 and then Week_Day = GNAT.Calendar.Monday then
         return True;
      end if;

      --  Xmas & Boxing day on a weekday
      if U_Month = 12 and then (U_Day = 25 or else U_Day = 26) then
         return True;
         --  Xmas or Boxing day on weekend
      elsif U_Month = 12 and then (U_Day = 27 or else U_Day = 28) and then Week_Day <= GNAT.Calendar.Tuesday then
            return True;
      end if;

      --  St Andrews day on 30th Nov or first Monday after 30th Nov
      if (U_Month = 11 and then U_Day = 30) or else ((U_Month = 12 and then U_Day <= 2) and then Week_Day = GNAT.Calendar.Monday) then
         return True;
      end if;

      --  Last Monday in May bank holiday
      if U_Month = 5 and then U_Day >= 25 and then Week_Day = GNAT.Calendar.Monday then
         return True;
      end if;

      Easter_Time := Get_Easter (U_Year);

      if Easter_Time > T then  --  Check for Good Friday
         Ada.Calendar.Arithmetic.Difference (Easter_Time, T, Day_Diff, Seconds_Diff, Leap_Seconds_Diff);

         if Day_Diff = 2 and then Week_Day = GNAT.Calendar.Friday then
            return True;
         end if;
      else  --  Check for Easter Monday
         Ada.Calendar.Arithmetic.Difference (T, Easter_Time, Day_Diff, Seconds_Diff, Leap_Seconds_Diff);

         if Day_Diff = 1 and then Week_Day = GNAT.Calendar.Monday then
            return True;
         end if;
      end if;

      return False;
   end Is_Holiday;

   function Is_Hour_Long (T : Ada.Calendar.Time) return Boolean is
      U_Year            : Ada.Calendar.Year_Number;
      U_Month           : Ada.Calendar.Month_Number;
      U_Day             : Ada.Calendar.Day_Number;
      U_Hour            : GNAT.Calendar.Hour_Number;
      U_Minute          : GNAT.Calendar.Minute_Number;
      U_Second          : GNAT.Calendar.Second_Number;
      U_Sub_Second      : GNAT.Calendar.Second_Duration;
      Week_Day          : GNAT.Calendar.Day_Name;
   begin
      Week_Day := GNAT.Calendar.Day_Of_Week (T); --  Monday is first

      GNAT.Calendar.Split_At_Locale (T, U_Year, U_Month, U_Day, U_Hour, U_Minute, U_Second, U_Sub_Second);

      if U_Hour = 1 and then Week_Day = GNAT.Calendar.Monday then
         return True;
      end if;

      return False;
   end Is_Hour_Long;

   function Is_Hour_Short (T : Ada.Calendar.Time) return Boolean is
      U_Year            : Ada.Calendar.Year_Number;
      U_Month           : Ada.Calendar.Month_Number;
      U_Day             : Ada.Calendar.Day_Number;
      U_Hour            : GNAT.Calendar.Hour_Number;
      U_Minute          : GNAT.Calendar.Minute_Number;
      U_Second          : GNAT.Calendar.Second_Number;
      U_Sub_Second      : GNAT.Calendar.Second_Duration;
      Week_Day          : GNAT.Calendar.Day_Name;
   begin
      Week_Day := GNAT.Calendar.Day_Of_Week (T); --  Monday is first

      GNAT.Calendar.Split_At_Locale (T, U_Year, U_Month, U_Day, U_Hour, U_Minute, U_Second, U_Sub_Second);

      if U_Hour = 23 and then Week_Day = GNAT.Calendar.Sunday then
         return True;
      end if;

      return False;
   end Is_Hour_Short;

   function Is_Unmetered (T : Ada.Calendar.Time) return Boolean is
      U_Year       : Ada.Calendar.Year_Number;
      U_Month      : Ada.Calendar.Month_Number;
      U_Day        : Ada.Calendar.Day_Number;
      U_Hour       : GNAT.Calendar.Hour_Number;
      U_Minute     : GNAT.Calendar.Minute_Number;
      U_Second     : GNAT.Calendar.Second_Number;
      U_Sub_Second : GNAT.Calendar.Second_Duration;
   begin
      GNAT.Calendar.Split_At_Locale (T, U_Year, U_Month, U_Day, U_Hour, U_Minute, U_Second, U_Sub_Second);

      if U_Month = 12 and then U_Day > 26 and then Is_Weekend (T) = False then
         return True;
      end if;

      return False;
   end Is_Unmetered;

   function Is_Weekend (T : Ada.Calendar.Time) return Boolean is
      Week_Day : GNAT.Calendar.Day_Name;
   begin
      Week_Day := GNAT.Calendar.Day_Of_Week (T);

      if Week_Day = GNAT.Calendar.Saturday or else Week_Day = GNAT.Calendar.Sunday then
         return True;
      end if;

      return False;
   end Is_Weekend;

   procedure Print_Easter_Date (T : Ada.Calendar.Time) is
      Date_Format : constant GNAT.Calendar.Time_IO.Picture_String := "%c";
   begin
      Ada.Text_IO.Put_Line ("Easter: " & GNAT.Calendar.Time_IO.Image (T, Date_Format));
   end Print_Easter_Date;
end Holiday;
