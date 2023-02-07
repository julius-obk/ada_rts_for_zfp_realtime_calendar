with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;

package body software_clock_cal is

   --pragma Ada_2020;
   ----------------
   -- Clock_Task --
   ----------------

   -- this clock is false by design, one second in real world does not
   -- match one second of this clock, per 0.1 Duration we are doing a random
   -- tick
   type U64 is mod 2**64;
   package ran_num_gen is new Ada.Numerics.Discrete_Random (reg_sub_sec);
   use ran_num_gen;

   generator : ran_num_gen.Generator;

   task body Call_Overflow_Handler is
      Trigger : Boolean := False;
      stop_it : Boolean := False;
   begin
      loop
         select
            accept Call do
               Trigger := True;
            end Call;
         or
            accept Halt do
               stop_it := True;
            end Halt;
         or
            delay 1.0;
            if Trigger then
               Overflow_Handler.all;
            end if;
         end select;
         exit when stop_it;
      end loop;
   end Call_Overflow_Handler;

   task body Call_Alarm_Handler is
      Trigger : Boolean := False;
      Stop_It : Boolean := False;
   begin
      loop
         select
            accept Call do
               Trigger := True;
            end Call;
         or
            accept halt do
               Stop_It := True;
            end halt;
         or
            delay 1.0;
            if Trigger then
               Alarm_Handler.all;
            end if;
            delay 1.0;
         end select;
         exit when Stop_It;
      end loop;
   end Call_Alarm_Handler;

   task body Clock_Task is
      use Ada.Text_IO;
      Sub_Seconds : reg_sub_sec := register_sub_sec'First with
         Volatile;

      Seconds : reg_sec := register_sec'First with
         Volatile;

      Alarm_Time : reg_sec      := reg_sec'Last;
      Last_Sub_S : constant U64 := 2**reg_sub_sec'Size - 1;

      function Random_tick return reg_sub_sec is
         tick : reg_sub_sec := Random (generator);
      begin
         return tick;
      end Random_tick;

      procedure Check_Overflow
        (tick : out reg_sub_sec; overflows : out Boolean)
      is
         now : U64 := U64 (Sub_Seconds) + U64 (Random_tick);
      begin
         if now > Last_Sub_S then
            now       := now - Last_Sub_S;
            tick      := reg_sub_sec (now);
            overflows := True;
            if Sub_S_Overflow_Handler /= null then
               Sub_S_Overflow_Handler.all;
            end if;

         else
            tick      := reg_sub_sec (now);
            overflows := False;
         end if;
      end Check_Overflow;

      Stop_It : Boolean := False;
   begin
      loop
         select
            accept Read_Clock (s : out reg_sec; sub_s : out reg_sub_sec) do
               s     := Seconds;
               sub_s := Sub_Seconds;
            end Read_Clock;
         or
            accept Write_clock (s : reg_sec; sub_s : reg_sub_sec) do
               Seconds     := s;
               Sub_Seconds := sub_s;
            end Write_clock;
         or
            accept Set_Alarm_Time (s : reg_sec) do
               Alarm_Time := s;
            end Set_Alarm_Time;
         or
            accept Halt do
               Stop_It := True;
            end Halt;
         or
            delay 0.1;
            declare
               new_sub_s : reg_sub_sec;
               add_1s    : Boolean;
            begin
               Check_Overflow (tick => new_sub_s, overflows => add_1s);
               Sub_Seconds := new_sub_s;
               if add_1s then

                  -- check for Overflow
                  if Seconds = reg_sec'Last then
                     Seconds := reg_sec'First;
                     if Overflow_Handler = null then
                        Put_Line ("no Handler installed for Overflow");
                     else
                        Call_Overflow_Handler.Call;
                     end if;
                  else
                     Seconds := Seconds + 1;
                  end if;
                  -- check if we hit the alarm
                  if Seconds = Alarm_Time then
                     if Alarm_Handler = null then
                        Put_Line ("no Handler installed for Alarm");
                     else
                        Call_Alarm_Handler.Call;
                     end if;
                  end if;
               end if;
            end;
         end select;
         exit when Stop_It;
      end loop;
   end Clock_Task;

   --------------------------
   -- Set_Overflow_Handler --
   --------------------------

   procedure Set_Overflow_Handler (Handler : proc_pt) is
   begin
      Overflow_Handler := Handler;
   end Set_Overflow_Handler;

   procedure Set_Sub_S_Overflow_Handler (Handler : proc_pt) is
   begin
      Sub_S_Overflow_Handler := Handler;
   end Set_Sub_S_Overflow_Handler;

   -----------------------
   -- Set_Alarm_Handler --
   -----------------------

   procedure Set_Alarm_Handler (Handler : proc_pt) is
   begin
      Alarm_Handler := Handler;
   end Set_Alarm_Handler;

   procedure Read_Clock (s : out reg_sec; sub_s : out reg_sub_sec) is
   begin
      Clock_Task.Read_Clock (s => s, sub_s => sub_s);
   end Read_Clock;

   procedure Read_Clock (sub_s : out reg_sub_sec) is
      s : reg_sec;
   begin
      Clock_Task.Read_Clock (s => s, sub_s => sub_s);
   end Read_Clock;

   procedure Write_clock (s : reg_sec; sub_s : reg_sub_sec) is
   begin
      Clock_Task.Write_clock (s => s, sub_s => sub_s);
   end Write_clock;

   procedure Set_Alarm_Time (s : reg_sec) is
   begin
      Clock_Task.Set_Alarm_Time (S => s);
   end Set_Alarm_Time;

end software_clock_cal;
