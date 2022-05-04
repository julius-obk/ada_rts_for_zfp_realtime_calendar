------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A D A . C A L E N D A R                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2020, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This was the cert and bare metal Ravenscar runtime version of this package
--  This has been copied from some AdaCore runtime and changed into a generic
--  package

package body Calendar_Generic is

   --  Julian day range covers Ada.Time range requirement with smallest
   --  possible delta within 64 bits. This type is used for calculations that
   --  do not need to preserve the precision of type Duration and that need a
   --  larger Julian Day range than the Modified Julian Day provides, because
   --  of the algorithms used

   type Julian_Day is
     delta Duration'Small / (86_400.0 / 2.0**5) range 2415_385.5 .. 2488_069.5;
   for Julian_Day'Small use Duration'Small / (86_400.0 / 2.0**5);

   function Trunc (Arg : Time) return Integer;
   --  Truncate Time

   function Trunc (Arg : Duration) return Integer;
   --  Truncate Duration

   function Trunc (Arg : Julian_Day'Base) return Integer;
   --  Truncate Julian Day

   function Valid_Date
     (D : Day_Number; M : Month_Number; Y : Year_Number) return Boolean;
   --  Check for valid Gregorian calendar date

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Duration) return Time is
      pragma Unsuppress (Overflow_Check, Time);
      Result : Time;
   begin
      Result := Left + Time (Right / Secs_Per_Day);
      return Result;
   exception
      when Constraint_Error =>
         raise Time_Error;
   end "+";

   function "+" (Left : Duration; Right : Time) return Time is
      pragma Unsuppress (Overflow_Check, Time);
      Result : Time;
   begin
      Result := Time (Left / Secs_Per_Day) + Right;
      return Result;
   exception
      when Constraint_Error =>
         raise Time_Error;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Duration) return Time is
      pragma Unsuppress (Overflow_Check, Time);
      Result : Time;
   begin
      Result := Left - Time (Right / Secs_Per_Day);
      return Result;
   exception
      when Constraint_Error =>
         raise Time_Error;
   end "-";

   function "-" (Left : Time; Right : Time) return Duration is
      pragma Unsuppress (Overflow_Check, Time);
      Temp   : Time;
      Result : Duration;
   begin
      Temp   := Left - Right;
      Result := Duration (Secs_Per_Day * Temp);
      return Result;
   exception
      when Constraint_Error =>
         raise Time_Error;
   end "-";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Time) return Boolean is
   begin
      return Modified_Julian_Day (Left) < Modified_Julian_Day (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Time) return Boolean is
   begin
      return Modified_Julian_Day (Left) <= Modified_Julian_Day (Right);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Time) return Boolean is
   begin
      return Modified_Julian_Day (Left) > Modified_Julian_Day (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Time) return Boolean is
   begin
      return Modified_Julian_Day (Left) >= Modified_Julian_Day (Right);
   end ">=";

   -----------
   -- Clock --
   -----------

   --  function Clock return Time is separate;

   ---------
   -- Day --
   ---------

   function Day (Date : Time) return Day_Number is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;
   begin
      Split (Date, DY, DM, DD, DS);
      return DD;
   end Day;

   -----------
   -- Month --
   -----------

   function Month (Date : Time) return Month_Number is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;
   begin
      Split (Date, DY, DM, DD, DS);
      return DM;
   end Month;

   -------------
   -- Seconds --
   -------------

   function Seconds (Date : Time) return Day_Duration is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;
   begin
      Split (Date, DY, DM, DD, DS);
      return DS;
   end Seconds;

   ----------------
   -- count_days --
   ----------------

   function count_days (Seconds : Duration) return Natural is
      count : Natural := Natural (Seconds);
   begin
      count := count / Seconds_Per_Day;
      return count;
   end count_days;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date :     Time; Year : out Year_Number; Month : out Month_Number;
      Day  : out Day_Number; Seconds : out Day_Duration)
   is
      --  Algorithm is the standard astronomical one for conversion
      --  from a Julian Day number to the Gregorian calendar date.
      --  Adapted from J. Meeus' "Astronomical Algorithms" pp 63 - 4.

      --  No need to check for Trunc (Date) < 2299_161 (Ada "Time" is
      --  always a Gregorian date).

      --  Split off fractional part before losing precision:

      F : constant Time'Base := Date - Time (Trunc (Date));

      --  Do remainder of calcs on full Julian day number with less precision
      --  in fractional part (not necessary for these calcs)

      JD_Date : constant Julian_Day :=
        Julian_Day'Base (Date) + Julian_Day'Base'(240_0000.5);

      Z : constant Integer := Trunc (JD_Date + Julian_Day'Base (0.5));

      A, B, C, D, E : Integer;

      Alpha : constant Integer :=
        Trunc
          (Julian_Day'Base
             ((Julian_Day'Base (Z) - Julian_Day'Base'(1867_216.25)) /
              Julian_Day'Base'(36_524.25)));

   begin
      --  Generate intermediate values

      A := Z + 1 + Alpha - Alpha / 4;
      B := A + 1_524;
      C :=
        Trunc
          (Julian_Day'Base
             ((Julian_Day'Base (B) - Julian_Day'Base'(122.1)) /
              Julian_Day'Base'(365.25)));
      D :=
        Trunc
          (Julian_Day'Base (Julian_Day'Base'(365.25) * Julian_Day'Base (C)));
      E := Trunc (Duration (Duration (B - D) / 30.600_1));

      --  Generate results from intermediate values

      Month := E - (if E < 14 then 1 else 13);
      Year  := C - (if Month > 2 then 4_716 else 4_715);
      Day   := B - D - Trunc (Duration (30.600_1 * Duration (E)));

      --  Restore seconds from precise fractional part

      Seconds := Day_Duration (86_400.0 * F);
   end Split;


   -----------
   -- Split --
   -----------

   procedure Split(Seconds : Day_Duration; H : out Hour_Number;
                   M : out Minute_Number; S : out Second_Number) is
      subtype Lli is Long_Long_Integer;
      S_Per_H : constant := 60 * 60;
      S_Per_M : constant := 60;
      temp : lli := lli(Seconds);
   begin
      H := Hour_Number(temp / S_Per_H);
      temp := temp - lli(H * S_Per_H);
      M := Minute_Number(temp / S_Per_M);
      temp := temp - lli(M * S_Per_M);
      S := Second_Number( temp);
   end Split;

   -- get hours, mins and secs of a Number of Seconds



   procedure HMS_To_Day_Duration ( This : out Day_Duration;
                                   Hours : Hour_Number;
                                   Minutes : Minute_Number;
                                   Secs : Second_Number) is
      subtype dd is Day_Duration;
      S_Per_H : constant Dd := 60.0*60.0;
      S_Per_M : constant dd := 60.0;
   begin
      This := Dd(Secs) + Dd(Hours) * S_Per_H +
        DD(Minutes) * S_Per_M;
      end HMS_To_Day_Duration;
   -- generate day_duration out of hours/mins/secs


   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year    : Year_Number; Month : Month_Number; Day : Day_Number;
      Seconds : Day_Duration := 0.0) return Time
   is
      --  This is an adaptation of the standard astronomical algorithm for
      --  conversion from a Gregorian calendar date to a Julian day number.
      --  Taken from J. Meeus' "Astronomical Algorithms" pp 60 - 1.

      Month_Num : Natural := Month;
      Year_Num  : Integer := Year;
      A, B      : Integer;

      type Day_Type is delta Modified_Julian_Day'Delta range 0.0 .. 32.0;
      for Day_Type'Small use Modified_Julian_Day'Small;

      Day_Val : constant Day_Type :=
        Day_Type (Day) + Day_Type (Seconds / Secs_Per_Day);

      subtype Month_Fixed is Duration range 4.0 .. 15.0;

   begin
      --  Check for valid date

      if not Valid_Date (Day, Month, Year) then
         raise Time_Error;
      end if;

      if Month_Num <= 2 then
         Year_Num  := Year_Num - 1;
         Month_Num := Month_Num + 12;
      end if;

      A := Year_Num / 100;
      B := 2 - A + A / 4;

      return
        Time'Base (Day_Val) +
        Time
          (Julian_Day'Base
             ((B + Trunc (Duration (30.600_1 * Month_Fixed (Month_Num + 1))) +
               Trunc
                 (Julian_Day'Base
                    (Julian_Day'Base'(365.25) *
                     Julian_Day'Base (Year_Num + 4_716))))) -
           Julian_Day'Base'(1_524.5) - Julian_Day'Base'(240_0000.5));
   end Time_Of;

   -----------
   -- Trunc --
   -----------

   function Trunc (Arg : Time) return Integer is
      Rounded : constant Integer := Integer (Arg);
      Sign    : constant Integer := Rounded / abs (Rounded);
   begin
      if abs (Time (Rounded)) > abs (Arg) then
         return Rounded - Sign;
      else
         return Rounded;
      end if;
   end Trunc;

   function Trunc (Arg : Duration) return Integer is
      Rounded : constant Integer := Integer (Arg);
   begin
      if Rounded = 0 or else abs (Duration (Rounded)) <= abs (Arg) then
         return Rounded;
      else
         return Rounded - Rounded / abs (Rounded);
      end if;
   end Trunc;

   function Trunc (Arg : Julian_Day'Base) return Integer is
      Rounded : constant Integer := Integer (Arg);
   begin
      if Rounded = 0 or else abs (Julian_Day'Base (Rounded)) <= abs (Arg) then
         return Rounded;
      else
         return Rounded - Rounded / abs (Rounded);
      end if;
   end Trunc;

   ----------------
   -- Valid_Date --
   ----------------

   function Valid_Date
     (D : Day_Number; M : Month_Number; Y : Year_Number) return Boolean
   is
   begin
      --  Check that input values have valid representations. This check is not
      --  strictly required, since the only way we could fail this test is if
      --  the Time_Of caller suppressed checks, in which case we have erroneous
      --  execution. However, raising Time_Error in this case seems a friendly
      --  way to handle the erroneous action.

      if not (Y'Valid and then M'Valid and then D'Valid) then
         return False;
      end if;

      --  Deal with checking day according to month

      case M is

         --  Apr | Jun | Sep | Nov

         when 4 | 6 | 9 | 11 =>
            return D <= 30;

            --  Note: lower bound OK due to 'Valid. Lower bound check would be
            --  optimized away anyway, and resulted in a compilation warning.

            --  Feb

         when 2 =>
            --  Do leap year check. Note that we do not need to check centuries
            --  due to the limited range of Year_Number.

            if Y mod 4 = 0 then
               return D <= 29;

               --  Note: lower bound OK due to 'Valid
            else
               return D <= 28;

               --  Note: lower bound OK due to 'Valid
            end if;

            --  Jan | Mar | May | Jul | Aug | Oct | Dec

         when 1 | 3 | 5 | 7 | 8 | 10 | 12 =>
            return True;
      end case;
   end Valid_Date;

   ----------
   -- Year --
   ----------

   function Year (Date : Time) return Year_Number is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;
   begin
      Split (Date, DY, DM, DD, DS);
      return DY;
   end Year;

   ------------
   -- Clock ---
   ------------

   function Clock return Time is
      s     : register_seconds;
      sub_s : register_sub_seconds;
      now   : Time := Time'First;
      temp  : Duration;
   begin
      Clock_Func (seconds => s, sub_seconds => sub_s);

      --seconds, add value of radix time
      temp := Duration (s) + Duration (Radix_Time) * Secs_Per_Day;

      --sub seconds
      temp := temp + Duration (sub_s) * delta_sub_seconds;

      --convert seconds to time by diverting with seconds per day
      now := Time (temp / Secs_Per_Day);

      return now;
   end Clock;

   ---------------
   -- Set_Clock---
   ---------------

   procedure Set_Clock
     (Year     : Year_Number; Month : Month_Number; Day : Day_Number;
      Hour     : Hour_Number; Minute : Minute_Number; Second : Second_Number;
      Fraction : Fractional_Seconds := 0.0)
   is
      secs     : register_seconds;
      sub_secs : register_sub_seconds;
      new_time : Time;
      Raw_Secs : Duration;

      Day_Secs : Day_Duration;
   begin
      HMS_To_Day_Duration (This => Day_Secs,
                           Hours => Hour,
                           Minutes => Minute,
                           Secs => Second);
      new_time :=
        Time_Of (Year => Year, Month => Month, Day => Day, Seconds => Day_Secs);

      --check if we set the clock backwards (below the value of radix time)
      --, if so set new radix time and set clock register to 0
      if new_time < Radix_Time then
         Radix_Time := new_time;
      end if;

      --compute Time into second count for seconds register
      raw_secs := (Duration (new_time) * Secs_Per_Day);
      raw_secs := raw_secs - (Duration (Radix_Time) * Secs_Per_Day);

      --check if the value of raw seconds fits in the register or already is
   --the last value, if so we set value of Radix_Time to the new time and set
      --0 to our register
      if Long_Long_Integer (raw_secs) >=
        Long_Long_Integer (register_seconds'Last)
      then
         Radix_Time := new_time;
         secs       := 0;
      else
         secs := register_seconds (raw_secs);
      end if;

      --compute sub seconds for register sub seconds
      raw_secs := raw_secs - Duration (secs);
      raw_secs := raw_secs + Fraction * delta_sub_seconds;
      sub_secs := register_sub_seconds (raw_secs);
      Set_Clock_Func (sec => secs, sub_sec => sub_secs);

   end Set_Clock;

   ---------------------
   -- time_as_seconds --
   ---------------------

   function time_as_seconds (this : Time) return Long_Long_Integer is
      Seconds : Long_Long_Integer;
   begin
      Seconds := Long_Long_Integer (this * Secs_Per_Day);
      return Seconds;
   end time_as_seconds;

   --------------------
   -- Clock_Overflow --
   --------------------

   procedure Clock_Overflow is
      Secs      : Duration;
      new_radix : Time;
   begin
         Secs       := Duration (register_seconds'Last);
         new_radix  := Secs + Radix_Time;
         Radix_Time := new_radix;
      exception
         when Constraint_Error =>
            raise Time_Error
              with "Seconds :" & Duration'Image (Secs) & "  +  " &
              Time'Image (Radix_Time) & "not valid";
   end Clock_Overflow;


   -----------------
   -- Set_Handler --
   -----------------


   function Set_Handler (At_Time : Time; Do_This : Event_Handler)
                         return Calendar_Event is
      Event : aliased Calendar_Event;
   begin
      Event.Timeout := At_Time;
      Event.Do_This := Do_This;
      Event.Canceled := False;

      --check if we have a free event slot, if so put in our event
      for item of Event_List loop
         if item = Null then
            	   	item := Event'Unchecked_Access;
            --call Queue_Manager...
            Queue_Manager;
            return Event;
         end if;
      end loop;

      --else raise Event_Queue_full
      raise Event_Queue_Full with "No more room in Event Queue";
   end Set_Handler;

   -----------------
   -- Set_Handler --
   -----------------

   function Set_Handler( After_Duration : Duration;
                         Do_This :        Event_Handler;
                         Do_Periodicly : Boolean := False)
                        return Calendar_Event
   is
      Event : aliased Calendar_Event;
      new_time : Time := Clock + After_Duration;
   begin
      Event.Timeout := new_time;
      Event.Do_This := Do_This;
      Event.Canceled := False;
      Event.Do_Periodicly := Do_Periodicly;

      --check if we have a free event slot, if so put in our event
      for item of Event_List loop
         if item = Null then
            item := Event'Unchecked_Access;
            --call Queue_Manager...
            Queue_Manager;
            return Event;
         end if;
      end loop;

      --else raise Event_Queue_full
      raise Event_Queue_Full with "No more room in Event Queue";
   end Set_Handler;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler (Event : Calendar_Event) return Event_Handler is
   begin
      return Event.Do_This;
   end Current_Handler;


   --------------------
   -- Cancel_Handler --
   --------------------

   procedure Cancel_Handler
     (Event : in out Calendar_Event; Cancelled : out Boolean) is
   begin
      if not Event.Canceled then
         for Item of Event_List loop
            if Event = Item.all then
               Item := Null;
               -- call Queue_Manager for cleanup
               Queue_Manager;
               Event.Canceled := True;
               Cancelled := True;
               return;
            end if;
         end loop;
      end if;
   end Cancel_Handler;


   -------------------
   -- Time_Of_Event --
   -------------------

   function Time_Of_Event (Event : Calendar_Event) return Time is
   begin
      return Event.Timeout;
   end Time_Of_Event;

   function Find_Next_Timeout return Time is
      timeout : Time := Time'Last;
   begin
      -- iter through Event_List, find the lowest timeout time,
      -- if the list is empty we will just return Time'Last as Valid value
      for item of Event_List when item /= null loop
         if item.Timeout < timeout then
            timeout := item.Timeout;
         end if;
      end loop;
      return timeout;
   end Find_Next_Timeout;

   -------------------
   -- Queue_Manager --
   -------------------

   procedure Queue_Manager is
      now : Time;
      new_timeout : Time;
      till_timeout : Duration;
   begin

      -- if semaphore is above 0 then this is called twice,
      -- we increase the semaphore by one, so the loop of the first call will
      -- do the work for the second call

      if Semaphore_Event_List > 1 then
         Semaphore_Event_List := @ + 1;
         return;
      end if;

      Semaphore_Event_List := @ +1;

      while Semaphore_Event_List /= 0 loop

         now := Clock;

         --check entrys of list and process them
         for Item of Event_List when Item /= null loop
            if not Item.Canceled and Item.Timeout < now then
               Item.Do_This.all;
               if not Item.Do_Periodicly then
                  Item.Canceled := True;
                  Item := null;
               else
                  Item.Timeout := now + Item.Time_Span_Between;
               end if;
            end if;
         end loop;

         --find next timeout
         new_timeout := Find_Next_Timeout;
         till_timeout := new_timeout - Clock;

         --check if timespan till timeout fits into register of clock
         declare
            Register_Val_Sec : register_seconds;
            Register_Val_Sub_S : register_sub_seconds;
            Now_Sec, Register_Max, New_Val : Long_Long_Integer;
         begin

            --compute new alarm value for register and check it fits in
            Clock_Func(	seconds => Register_Val_Sec,
                       sub_seconds => Register_Val_Sub_S);

            Now_Sec := Long_Long_Integer(Register_Val_Sec);
            Register_Max := Long_Long_Integer(register_seconds'Last);

            New_Val := Long_Long_Integer(till_timeout) + Now_Sec;

            if New_Val >= Register_Max then
               -- does not fit so set to Last possible counter reading
               -- if the alarm hits that, the Queue_Manger will set a
               -- new timeout
               Set_Alarm_Time(register_seconds'Last);
            else
               Set_Alarm_Time(register_seconds(New_Val));
            end if;
         end;
         Semaphore_Event_List := @ -1;
      end loop;
   end Queue_Manager;
   -- We set this procedure as the time event handler which gets
   -- called by the interrupt.
   -- This procedure will get the current time and call all handlers of the
   -- event list which have timed out. When called they will be removed from the
   -- list, but if they are installed as periodicly, they stay and get a new
   -- timeout.
   -- After all the pending events have been handeled, we search the Event_List
   -- for the closest timeout and sets this as the alarm time by calling
   -- "Set_Alarm_Time (Alarm_Event_Time : register_seconds)"
   --
   -- the Queue Manager is also called each time after "Set_Handler" so to
   -- ensure we have an up to date alarm time for the closest timeout
   --



   Queue_Manager_Pt : constant access procedure := Queue_Manager'Access;





   --  Start of elaboration code for Ada.Calendar
begin
   --set radix
   Radix_Time := Time_Of (1_970, Month_Number'First, Day_Number'First, 0.0);

   --set clock overflow handler
   Set_Clock_Overflow_Handler (Clock_Overflow_Acc);

   --set Queuemanger as clock alarm handler
   Set_Alarm_Event_Handler(Queue_Manager_Pt);

end Calendar_Generic;




