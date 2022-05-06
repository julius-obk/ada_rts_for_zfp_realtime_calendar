pragma Ada_2012;

with Ada.Text_IO;      use Ada.Text_IO;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Assertions;

package body My_Unittests is

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (P : in out cal_test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (Test => P, Routine => Dummy_Test_Routine'Access,
         Name => "Dummy test routine");

      Register_Routine
        (Test => P, Routine => Test_Get_Clock_Time'Access,
         Name =>
           "Procedure for testing the Clock function which should return" &
           " the time which our clock has massured");

      Register_Routine
        (Test => P, Routine => Test_Split_TimeOf'Access,
         Name =>
           "procedure to install a rest event, wait for" &
           "it to occur and the handler is called");

      Register_Routine
        (Test => P, Routine => Test_Set_Clock_Time'Access,
         Name => "procedure to test setting a new time for our clock");

      Register_Routine
        (Test => P, Routine => Test_Set_Event'Access,
         Name =>
           "procedure to install a rest event, wait for" &
           "it to occur and the handler is called");

   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : cal_test) return Message_String is
   begin
      return Format ("Calendartest");
   end Name;

   ------------------------
   -- Dummy_Test_Routine --
   ------------------------

   procedure Dummy_Test_Routine (T : in out Test_Cases.Test_Case'Class) is
      use Ada.Text_IO;
   begin
      Put_Line ("dummy test routine");
   end Dummy_Test_Routine;

   --------------------
   -- Test_ Get_Clock_Time --
   --------------------

   procedure Test_Get_Clock_Time (T : in out Test_Cases.Test_Case'Class) is
      use Calendar;
      now     : Time   := Clock;
      year    : Year_Number;
      month   : Month_Number;
      day     : Day_Number;
      seconds : Day_Duration;
      bla     : String := Integer'Image (5);

   begin
      Split
        (Date    => now, Year => year, Month => month, Day => day,
         Seconds => seconds);
      Put_Line ("now it is :");
      Put_Line
        ("year " & Year_Number'Image (year) & "month " &
         Month_Number'Image (month) & "day " & Day_Number'Image (day) &
         "seconds " & Day_Duration'Image (seconds));
   end Test_Get_Clock_Time;

   procedure Test_Split_TimeOf (T : in out Test_Cases.Test_Case'Class) is
      use Calendar;
      Y     : Year_Number   := 2_024;
      M     : Month_Number  := 3;
      D     : Day_Number    := 2;
      H     : Hour_Number   := 2;
      Min   : Minute_Number := 59;
      S     : Second_Number := 3;
      Day_S : Day_Duration;
      Now   : Time;
      subtype dd is Day_Duration;
   begin
      Put_Line ("we are converting the time of :");

      HMS_to_Day_Duration
        (This => Day_S, Hours => H, Minutes => Min, Secs => S);

      Put_Line ("seconds of day ( day_s): " & Day_S'Img);
      Put_Line ("------------");
      Now := Time_Of (Year => Y, Month => M, Day => D, Seconds => Day_S);

      declare
         Y2     : Year_Number;
         M2     : Month_Number;
         D2     : Day_Number;
         H2     : Hour_Number;
         Min2   : Minute_Number;
         S2     : Second_Number;
         Day_S2 : Day_Duration;
         use AUnit.Assertions;
      begin
         Split
           (Date    => Now, Year => Y2, Month => M2, Day => D2,
            Seconds => Day_S2);

         Assert (Y2 = Y, Y2'Img & " " & Y'Img & " not matching");
         Assert (M2 = M, M2'Img & " " & M'Img & " not matching");
         Assert (D2 = D, D2'Img & " " & D'Img & " not matching");
         Assert
           (Day_S2 = Day_S, Day_S2'Img & " " & Day_S'Img & " not matching");

         Split (Seconds => Day_S2, H => H2, M => Min2, S => S2);
         Assert (H2 = H, H2'Img & " " & H'Img & " not matching");
         Assert (Min2 = Min, Min2'Img & " " & Min'Img & " not matching");
         Assert (S2 = S, S2'Img & " " & S'Img & " not matching");
      end;

   end Test_Split_TimeOf;

   --------------------
   -- Test_Set_Clock_Time --
   --------------------

   procedure Test_Set_Clock_Time (T : in out Test_Cases.Test_Case'Class) is
      use Calendar;
      y     : Year_Number   := 2_024;
      m     : Month_Number  := 8;
      d     : Day_Number    := 20;
      h     : Hour_Number   := 2;
      min   : Minute_Number := 59;
      s     : Second_Number := 3;
      day_s : Day_Duration  := 0.0;
      Now   : Time;
   begin

      Set_Clock
        (Year   => y, Month => m, Day => d, Hour => h, Minute => min,
         Second => s);

      Now := Clock;

      declare
         Y2     : Year_Number;
         M2     : Month_Number;
         D2     : Day_Number;
         H2     : Hour_Number;
         Min2   : Minute_Number;
         S2     : Second_Number;
         Day_S2 : Day_Duration;
         use AUnit.Assertions;
      begin
         Split
           (Date    => Now, Year => Y2, Month => M2, Day => D2,
            Seconds => Day_S2);

         Assert (Y2 = y, Y2'Img & " " & y'Img & " not matching");
         Assert (M2 = m, M2'Img & " " & m'Img & " not matching");
         Assert (D2 = d, D2'Img & " " & d'Img & " not matching");
         -- Assert
         -- (Day_S2 = day_s, Day_S2'Img & " " & day_s'Img & " not matching");

         Split (Seconds => Day_S2, H => H2, M => Min2, S => S2);
         Assert (H2 = h, H2'Img & " " & h'Img & " not matching");
         Assert (Min2 = min, Min2'Img & " " & min'Img & " not matching");
         Assert (S2 = s, S2'Img & " " & s'Img & " not matching");

      end;
   end Test_Set_Clock_Time;

   Done : Boolean := False with
      Volatile;

   procedure Print_Time is
      use Calendar;
      at_time : Time := Clock;
      y       : Year_Number;
      m       : Month_Number;
      d       : Day_Number;
      s       : Day_Duration;
   begin
      Split (Date => at_time, Year => y, Month => m, Day => d, Seconds => s);
      Put_Line ("now @ " & y'Img & " " & m'Img & " " & d'Img & " " & s'Img);
   end Print_Time;

   procedure Alarm is
   begin
      Put_Line ("########################");
      Put_Line ("alarm has been rised:");
      Print_Time;
      Done := True;
      Put_Line ("########################");
   end Alarm;

   ---------------
   -- Test_Set_Event --
   ---------------

   procedure Test_Set_Event (T : in out Test_Cases.Test_Case'Class) is
      use Calendar;
      now      : Time           := Clock;
      alarm_pt : Event_Handler  := Alarm'Access;
      future   : Time           := now + 30.0;
      my_event : Calendar_Event :=
        Set_Handler (At_Time => future, Do_This => alarm_pt);

   begin
      while not Done loop
         Print_Time;
         delay 1.0;
      end loop;
      Put_Line ("Event has been triggered");
   end Test_Set_Event;

   procedure Test_Cal_Delay (T : in out Test_Cases.Test_Case'Class) is
      procedure Sleep is
      begin
         delay 40.0;
      end Sleep;

      procedure Info is
      begin
         Put_Line ("testing delay");
      end Info;

      package Calendar.delays1 is new Calendar_Generic.Delays
        (Sleep_Function => Sleep);
      package Calendar.delays2 is new Calendar_Generic.Delays;
      package td1 renames Calendar.delays1;
      package Td2 renames Calendar.Delays2;
      Future : Calendar.Time := Calendar.Clock + 1.0;

   begin
      Print_Time;
      Info;
      Td1.Delay_For (1.0);

      Print_Time;
      Info;
      Td1.Delay_Until (Future);

      Print_Time;
      Info;
      Td2.Delay_For (1.0);

      Print_Time;
      Info;
      Td2.Delay_Until (Future);

      Print_Time;

   end Test_Cal_Delay;

   procedure Test_Set_Event_Periodicly (T : in out Test_Cases.Test_Case'Class)
   is
      use Calendar;
      Now         : Time    := Clock;
      Future      : Time    := Now + 3.0;
      Event_Count : Integer := 0;

      procedure Count_Alarms is
      begin
         Alarm;
         Event_Count := Event_Count + 1;
         Put_Line ("alarm has been raised" & Event_Count'Img & " times");
      end Count_Alarms;

      Alarm_Pt : Event_Handler := Alarm'Access;

      My_Event : Calendar_Event :=
        Set_Handler
          (After_Duration => Future, Do_This => Alarm_Pt,
           Do_Periodicly  => True);
   begin
      while Event_Count > 3 loop
         null;
      end loop;
      Done := False;
      Cancel_Handler (Event => My_Event, Cancelled => Done);
      Put_Line ("Event has been canceled? " & Done'Img);
   end Test_Set_Event_Periodicly;

   procedure Test_RT_Get_Clock_Time (T : in out Test_Cases.Test_Case'Class);

end My_Unittests;
