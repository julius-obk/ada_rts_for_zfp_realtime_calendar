------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A D A . C A L E N D A R                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2004-2020, AdaCore                     --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  This was copied  from the cert and bare metal Ravenscar runtime
-- version of this package (Ada.Calendar) and changed into a generic package

generic

   type register_sub_seconds is mod <>;
   --the type the of the hardware clock register, delta 1 => fraction of a
   --second,   fraction is computed by 1/ delta_sub_seconds
   -- if we have a hardware clock with a prescaler that overruns before
   -- register_sub_seconds'Last, then we have to call the procedure
   -- recalculate_sub_sec_detla(overrun_val)

   type register_seconds is mod <>;
   --the type the of the hardware clock registers, delta 1 => delta 1 sec

   with procedure Clock_Func
     (seconds : out register_seconds; sub_seconds : out register_sub_seconds);
   --function to get the current value of the hw clock register

   with procedure Set_Clock_Func
     (sec : in register_seconds; sub_sec : in register_sub_seconds);
   --procdure to set Value in the hw clock register

   type access_to_proc is access procedure;

   with procedure Set_Clock_Overflow_Handler (Update_Clock : access_to_proc);
   --procdure to attach an handler for an overflow of the hw clock

   with procedure Set_Alarm_Event_Handler (Handler : access_to_proc);
   -- procudure to Install the Alarm Event Handler for the Clock;

   with procedure Set_Alarm_Time (Alarm_Event_Time : register_seconds);
   -- seconds of the time, when the Event is suppost to be triggered,
   -- (fraction of seconds are not supported for events)

   Max_Events : Positive := 30;

package Calendar_Generic is

   --  pragma Assert (Ticks_Per_Second >= 1_000);

   type Time is private;
   --  RQ 1-1: Time must allow representation of any point in time between
   --  January 1st 1901 and December 31st 2099 with the precision of at
   --  least System.Tick seconds.
   --
   --  Note: Time type is represented as a Modified Julian Day Number (MJDN).
   --  MJDN is a number of days elapsed since midnight of November 16th/17th,
   --  1858, so day 0 in the system of MJDN is the day 17th November 1858.

   --  Declarations representing limits of allowed local time values. Note that
   --  these do NOT constrain the possible stored values of time which may well
   --  permit a larger range of times (this is explicitly allowed in Ada 95).

   subtype Year_Number is Integer range 1_901 .. 2_099;
   subtype Month_Number is Integer range 1 .. 12;
   subtype Day_Number is Integer range 1 .. 31;

   subtype Day_Duration is Duration range 0.0 .. 86_400.0;

   ------------------------
   -- Local Declarations --
   ------------------------

   Secs_Per_Day    : constant := 86_400.0;
   Seconds_Per_Day : constant := 86_400;

   --  Number of seconds in a day

   --  pragma Assert
   --    ((clock_register_type'Last + 1) mod Ticks_Per_Second = 0,
   --     "clock_register_type shall be multible of" &
   --     " Ticks_Per_Second with remainder 0 ");

   function Clock return Time with
      Volatile_Function;
      --  RQ 2.9: This function shall return the current time in days which
      --  elapsed since 17'th November 1858.

   subtype Hour_Number is Integer range 0 .. 23;
   subtype Minute_Number is Integer range 0 .. 59;
   subtype Second_Number is Integer range 0 .. 59;

   subtype Fractional_Seconds is Duration range 0.0 .. 1.0 - Duration'Small;

   procedure Set_Clock
     (Year     : Year_Number; Month : Month_Number; Day : Day_Number;
      Hour     : Hour_Number; Minute : Minute_Number; Second : Second_Number;
      Fraction : Fractional_Seconds := 0.0);

   function Year (Date : Time) return Year_Number;
   --  RQ 2.19: This function shall return year of the specified time.

   function Month (Date : Time) return Month_Number;
   --  RQ 2.11: This function shall return month of the specified time.

   function Day (Date : Time) return Day_Number;
   --  RQ 2.10: This function shall return day of the specified time.

   function Seconds (Date : Time) return Day_Duration;
   --  RQ 2.12: This function shall return seconds of the specified time.

   procedure Split
     (Seconds :     Day_Duration; H : out Hour_Number; M : out Minute_Number;
      S       : out Second_Number);
   -- get hours, mins and secs of a Number of Seconds

   procedure Split
     (Date :     Time; Year : out Year_Number; Month : out Month_Number;
      Day  : out Day_Number; Seconds : out Day_Duration);
   --  RQ 2.13: This function shall get a Gregorian date (year number, month
   --  number, day number and duration) corresponding to the specified value
   --  of the Time type.
   --  Note: Algorithm used to convert modified Julian day number to Gregorian
   --  date has been adopted from the one described by the Jean Meeus in
   --  "Astronomical Algorithms" p. 63-4.

   procedure HMS_to_Day_Duration
     (This : out Day_Duration; Hours : Hour_Number; Minutes : Minute_Number;
      Secs :     Second_Number);
   -- generate day_duration out of hours/mins/secs

   function Time_Of
     (Year    : Year_Number; Month : Month_Number; Day : Day_Number;
      Seconds : Day_Duration := 0.0) return Time;
   --  RQ 2.14: This function shall convert the specified Greogorian date into
   --  a value of type Time.
   --  RQ 2.14.1: If the specified values do not represent a valid Gregorian
   --  calendar date, then raise the Time_Error.

   function "+" (Left : Time; Right : Duration) return Time;
   --  RQ 2.1: This function shall add the specified duration to the given
   --  point in time.
   --  RQ 2.1-1: If the result is not representable in the type Time, then
   --  raise the Time_Error.

   function "+" (Left : Duration; Right : Time) return Time;
   --  RQ 2.2: This function shall add the specified duration to the given
   --  point in time.
   --  RQ 2.2-1: If the result is not representable in the type Time, then
   --  raise the Time_Error.

   function "-" (Left : Time; Right : Duration) return Time;
   --  RQ 2.3: This function shall subtract the specified duration from the
   --  given point in time.
   --  RQ 2.3-1: If the result is not representable in the type Time, then
   --  raise the Time_Error.

   function "-" (Left : Time; Right : Time) return Duration;
   --  RQ 2.4: This function shall determine the amount of time which elapsed
   --  since <Left> point in time till <Right>.
   --  RQ 2.4-1: If the result is not representable in the type Duration, then
   --  raise the Time_Error.

   function "<" (Left, Right : Time) return Boolean;
   --  RQ 2.5: This function shall return True if the <Left> point in time is
   --  before the <Right> point in time, False otherwise.

   function "<=" (Left, Right : Time) return Boolean;
   --  RQ 2.6: This function shall return True if both times are equal or the
   --  <Left> point in time is before the <Right> point in time, False
   --  otherwise.

   function ">" (Left, Right : Time) return Boolean;
   --  RQ 2.7: This function shall return True if the <Left> point in time is
   --  after the <Right> point in time, False otherwise.

   function ">=" (Left, Right : Time) return Boolean;
   --  RQ 2.8: This function shall return True if both times are equal or the
   --  <Left> point in time is after the <Right> point in time, False
   --  otherwise.

   Time_Error : exception;

   Event_Queue_Full : exception;

   type Calendar_Event is limited private;

   type Event_Handler is access procedure;

   function Set_Handler
     (At_Time : Time; Do_This : Event_Handler) return Calendar_Event;

   function Set_Handler
     (After_Duration : Duration; Do_This : Event_Handler;
      Do_Periodicly  : Boolean := False) return Calendar_Event;

   function Current_Handler (Event : Calendar_Event) return Event_Handler;

   procedure Cancel_Handler
     (Event : in out Calendar_Event; Cancelled : out Boolean);

   function Time_Of_Event (Event : Calendar_Event) return Time;

   procedure Recalculate_Sub_Sec_Detla (Overrun_Value : register_sub_seconds);

private
   --
   Delta_Sub_Seconds : Duration :=
     1.0 / Duration (register_sub_seconds'Last + 1);
   --smallest fraction of a second which our hardware messures

   pragma Inline (Year);
   pragma Inline (Month);
   pragma Inline (Day);

   pragma Inline ("<");
   pragma Inline ("<=");
   pragma Inline (">");
   pragma Inline (">=");

   function count_days (Seconds : Duration) return Natural;
   --return the number of days for given seconds

   function time_as_seconds (this : Time) return Long_Long_Integer;
   -- convert time to Long_Long_Integer value of seconds

   procedure Clock_Overflow;
   -- procedure to set new radix time
   Clock_Overflow_Acc : access_to_proc := Clock_Overflow'Access;

   pragma Inline (count_days);
   pragma Inline (time_as_seconds);

   --  Time is represented as a Modified Julian Day number plus fraction of a
   --  day within the range of dates defined for Ada.Calendar.Time.  The small
   --  of the type preserves the precision of type Duration (64 bits).

   --  We introduce type Modified_Julian_Day so that expressions will be
   --  cleaner when we want the operations from Standard as opposed to those
   --  defined in Ada.Calendar on type Time.

   --  The Julian Day approach simplifies Time_Of and Split substantially, as
   --  well as being well-known algorithms.  The use of the Modified Julian Day
   --  number allows us to preserve the precision of type Duration.

   --  Time zones are not supported in this implementation

   --  Modified Julian Day.  The range is exactly that of the dates supported
   --  within the constraints on the components that make up a date.

   type Constrained_Modified_Julian_Day is
     delta Duration'Small / 86_400.0 range 15_385.0 .. 88_069.0;
   for Constrained_Modified_Julian_Day'Small use Duration'Small / 86_400.0;

   subtype Modified_Julian_Day is Constrained_Modified_Julian_Day'Base;

   type Time is new Modified_Julian_Day;

   Radix_Time : Time with
      Volatile;

      --  This is the zero point for time values. Time_Of returns 1 Jan 1970,
      --  0 UTC for this value. The time at which the board boots is given this
      --  conventional Time value. The BSP can adjust the initial real time
      --  relative to this value.

   type Calendar_Event is record
      Timeout : Time :=
        Time'First; -- after this time is up the Event gets called
      Do_This       : Event_Handler := null; -- Handler to call
      Do_Periodicly : Boolean       :=
        False; -- repeat forever or till stopped manually
      Time_Span_Between : Duration := 0.0; -- timespan to compute next event
      Canceled          : Boolean  := True;
   end record;

   type acc_cal_event is access all Calendar_Event;
   --access of calendar event for queue

   subtype index is Integer range 1 .. Max_Events;

   type cal_event_array is array (index) of acc_cal_event;
   Event_List : cal_event_array;

   function Find_Next_Timeout return Time;
   --iter throught the Event_List and find next Timeout

   subtype semaphore is Integer range 0 .. 2;

   Semaphore_Event_List : semaphore := 0 with
      Volatile;
   -- we use this to lock the Event_List, so Queue_Manager from Interrupt and
   -- Set_Handler calls wait till one is finished

   procedure Queue_Manager;
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

end Calendar_Generic;
