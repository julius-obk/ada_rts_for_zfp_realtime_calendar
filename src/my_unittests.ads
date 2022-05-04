with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with software_clock_cal;
with Calendar_Generic;
with Real_Time;

package My_Unittests is

   type cal_test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (P : in out cal_test);
   function Name (T : cal_test) return Message_String;

private

   --test for package calendar
   procedure Dummy_Test_Routine (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Get_Clock_Time (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Set_Clock_Time (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Set_Event (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Split_TimeOf (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Cal_Delay (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Set_Event_Periodicly (T : in out Test_Cases.Test_Case'Class);
   procedure Test_RT_Get_Clock_Time (T : in out Test_Cases.Test_Case'Class);

   type reg_s is mod 2**32;
   type reg_sub_sec is mod 2**16;

   package test_clock is new software_clock_cal
     (register_sub_sec => reg_sub_sec, register_sec => reg_s);

   use test_clock;

   package Calendar is new Calendar_Generic
     (register_sub_seconds       => reg_sub_sec, register_seconds => reg_s,
      Clock_Func                 => Read_Clock, Set_Clock_Func => Write_clock,
      access_to_proc             => proc_pt,
      Set_Clock_Overflow_Handler => Set_Overflow_Handler,
      Set_Alarm_Event_Handler    => Set_Alarm_Handler,
      Set_Alarm_Time             => Set_Alarm_Time);

   ticks_p_s : constant := 2**16;

   package my_real_time is new Real_Time
     (Ticks_Per_Second => ticks_p_s, Timer_Register_Sub_Seconds => reg_sub_sec,
      Clock_Func              => Read_Clock, proc_acc => proc_pt,
      Attach_Overflow_Handler => Set_Sub_S_Overflow_Handler);

end My_Unittests;
