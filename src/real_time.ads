------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A D A . R E A L _ T I M E                        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                     Copyright (C) 2001-2020, AdaCore                    --
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

-- this is supposed to be used with an hardware timer, that should overflow
-- one time per second and trigger an interrupt
-- we use this interrupt to update our overflow counter, + Ticks_Per_Second for
-- each overflow

generic
   Counts_Down : Boolean := True;
   Ticks_P_Sec : Positive;
   -- times this much ticks per second, is used to compute seconds and so on,
   -- if this changes, call procedure Change_Ticks(new_value)

   type Timer_Register_Sub_Seconds is mod <>;
   -- type of the register where our time is counted

   Count_Per_Tick : Positive;
   -- at this number the timer resets / overflows / generates a tick, and restarts

   with procedure Clock_Func (Sub_S : out Timer_Register_Sub_Seconds);
   --function to get the current value of our timer

--type proc_acc is access procedure;
--with procedure Attach_Overflow_Handler (Handler : access procedure);
--handler of timer overflow

package Real_Time is
   --pragma Compile_Time_Error
   --  (Ticks_Per_Second < 50_000,
   --   "Ada RM D.8 (30) requires " &
   --   "that Time_Unit shall be less than or equal to 20 microseconds");
   --pragma Compile_Time_Error
   --  (Duration'Size /= 64,
   --   "this version of Ada.Real_Time requires 64-bit Duration");

   --  Time_Unit : constant := 1.0 / Ticks_Per_Second;

   type Time is private;
   type Time_Span is private;

   Time_Span_First : constant Time_Span;
   Time_Span_Last  : constant Time_Span;
   Time_Span_Zero  : constant Time_Span;
   Time_Span_Unit  : constant Time_Span;

   function Clock return Time;

   function "+" (Left : Time; Right : Time_Span) return Time;
   function "-" (Left : Time; Right : Time_Span) return Time;
   function "-" (Left : Time; Right : Time) return Time_Span;

   function "+" (Left : Time_Span; Right : Time) return Time is (Right + Left);

   function "<" (Left, Right : Time) return Boolean;
   function "<=" (Left, Right : Time) return Boolean;
   function ">" (Left, Right : Time) return Boolean;
   function ">=" (Left, Right : Time) return Boolean;

   function "+" (Left, Right : Time_Span) return Time_Span;
   function "-" (Left, Right : Time_Span) return Time_Span;
   function "-" (Right : Time_Span) return Time_Span;
   function "*" (Left : Time_Span; Right : Integer) return Time_Span;
   function "*" (Left : Integer; Right : Time_Span) return Time_Span;
   function "/" (Left, Right : Time_Span) return Integer;
   function "/" (Left : Time_Span; Right : Integer) return Time_Span;

   function "abs" (Right : Time_Span) return Time_Span;

   function "<" (Left, Right : Time_Span) return Boolean;
   function "<=" (Left, Right : Time_Span) return Boolean;
   function ">" (Left, Right : Time_Span) return Boolean;
   function ">=" (Left, Right : Time_Span) return Boolean;

   function To_Duration (TS : Time_Span) return Duration;
   function To_Time_Span (D : Duration) return Time_Span;

   function Nanoseconds (NS : Integer) return Time_Span;
   function Microseconds (US : Integer) return Time_Span;
   function Milliseconds (MS : Integer) return Time_Span;

   function Seconds (S : Integer) return Time_Span;
   pragma Ada_05 (Seconds);

   function Minutes (M : Integer) return Time_Span;
   pragma Ada_05 (Minutes);

   --  Seconds_Count needs 64 bits. Time is a 64-bits unsigned integer
   --  representing clock ticks, and if the clock frequency is lower than
   --  2 ** 32 Hz (~ 4 GHz), which is the case so far, we need more than 32
   --  bits to represent the number of seconds. Additionally, Time is
   --  unsigned, so Seconds_Count is always positive.

   type Seconds_Count is range 0 .. 2**63 - 1;

   procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span);

   function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time;

   Time_Error : exception;

   procedure Update_Overflow_Counter;
   --Up_Overlw_Access : constant access procedure :=
   --  Update_Overflow_Counter'Access;
   Overflow_Callback : constant access procedure :=
     Update_Overflow_Counter'Access;

   procedure Change_Ticks_Per_Sec (New_Val : Positive);
   procedure Change_Count_Per_Tick (New_Val : Positive);

private
   function Count_Per_S return Positive;

   Ticks_Per_Second : Positive := Ticks_P_Sec;
   Overflow_val     : Positive := Count_Per_Tick;

   type Time is mod 2**64;

   Time_First : constant Time := Time'First;
   Time_Last  : constant Time := Time'Last;

   type Time_Span is range -2**63 .. 2**63 - 1;

   Time_Span_First : constant Time_Span := Time_Span'First;
   Time_Span_Last  : constant Time_Span := Time_Span'Last;

   Time_Span_Zero : constant Time_Span := 0;
   Time_Span_Unit : constant Time_Span := 1;

   Tick : constant Time_Span := 1;
   --this is unsued

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "abs");

   pragma Inline (Microseconds);
   pragma Inline (Milliseconds);
   pragma Inline (Nanoseconds);
   pragma Inline (Seconds);
   pragma Inline (Minutes);

   Overflow_Counter : Time := 0 with
     Volatile;

end Real_Time;
