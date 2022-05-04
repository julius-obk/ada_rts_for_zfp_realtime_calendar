pragma Ada_2012;
package body Calendar_Generic.Delays is

   ---------------
   -- Delay_For --
   ---------------

   procedure Delay_For (D : Duration) is
      future : Time := Clock + D;
   begin
      if Active_Delay then
         raise Program_Error
           with "only one delay statement at a time" &
           ", We Already have one active?!";
      else
         Delay_Until (future);
      end if;
   end Delay_For;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      Delta_D : Duration := T - Clock;
   begin
      if Active_Delay then
         raise Program_Error
           with "only one delay statement at a time" &
           ", We Already have one active?!";
      else
         Active_Delay := True;
         while Clock < T loop
            if Delta_D >= Threshold then
               Wake_Up_Event :=
                 Set_Handler
                   (At_Time => Time'Last, Do_This => Check_Wakeup_Pt);
               -- with this loop we are going to sleep and stay asleep if
               -- Something triggers an wakeup
               while not Wake_Up loop
                  Sleep;
               end loop;
               Wake_Up := False;

               --set new wake up event to time last, so have our slot blocked
               Wake_Up_Event :=
                 Set_Handler
                   (At_Time => Time'Last, Do_This => Check_Wakeup_Pt);

            else
               null;
            end if;
         end loop;
         Active_Delay := False;
      end if;

   end Delay_Until;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (T : Time) return Duration is
      temp : Duration;
   begin
      temp := Duration (T * Secs_Per_Day);
      return temp;
   end To_Duration;

   procedure Check_Wakeup is
      now : Time := Clock;
   begin
      if now >= Wake_Up_Time then
         Wake_Up := True;
      end if;
   end Check_Wakeup;

begin
   -- we block one slot in the event queue for our wakeup events
   Wake_Up_Event :=
     Set_Handler (At_Time => Time'Last, Do_This => Check_Wakeup_Pt);

end Calendar_Generic.Delays;
