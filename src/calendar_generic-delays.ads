with dummys; use dummys;

generic
   Sleep_threshold : Duration := 60.0;
   -- threshold above this we set our system to sleep while delaying
   -- delay(..)   if duration >= sleep_threshold then sleep for duration
   -- alarm interrupt from parent package is supposed to wake us up

   with procedure Sleep_Function is do_nothing;
   -- a procedure to set our system to sleep and save power

package Calendar_Generic.Delays is
   procedure Delay_For (D : Duration);
   -- this function is supposed to be run only once at a time,
   -- if it is called a second time, while the first call is not finisched,
   -- we raise an exception

   procedure Delay_Until (T : Time);
   -- this function is supposed to be run only once at a time,
   -- if it is called a second time, while the first call is not finisched,
   -- we raise an exception

   function To_Duration (T : Time) return Duration;

private

   -- this is for our sleep / wakeup stuff
   Threshold : constant Duration := Sleep_threshold;
   procedure Sleep renames Sleep_Function;

   Active_Delay : Boolean := False with
      Volatile;
      -- we only ever want to have one active delay statement

   Wake_Up_Time : Time := Time'Last with
      Volatile;
   Wake_Up : Boolean := False with
      Volatile;

   procedure Check_Wakeup;
   Check_Wakeup_Pt : constant Event_Handler := Check_Wakeup'Access;

   Wake_Up_Event : Calendar_Event with
      Volatile;

end Calendar_Generic.Delays;
