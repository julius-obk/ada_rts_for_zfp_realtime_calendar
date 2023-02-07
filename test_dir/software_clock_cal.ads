generic
   type register_sub_sec is mod <>;
   type register_sec is mod <>;

package software_clock_cal is
   subtype reg_sub_sec is register_sub_sec;
   subtype reg_sec is register_sec;

   procedure Read_Clock (s : out reg_sec; sub_s : out reg_sub_sec);
   procedure Read_Clock (sub_s : out reg_sub_sec);
   procedure Write_clock (s : reg_sec; sub_s : reg_sub_sec);
   procedure Set_Alarm_Time (s : reg_sec);

--   procedure Clock (s : reg_sec; sub_s : reg_sub_sec);
   -- procedure Set_Clock (s : reg_sec; sub_s : reg_sub_sec);

   type proc_pt is access procedure;
   procedure Set_Overflow_Handler (Handler : proc_pt);
   procedure Set_Alarm_Handler (Handler : proc_pt);
   -- alarm will be raised if seconds_count  of the clock
   -- matches or is above, default value reg_sec'Last;

   procedure Set_Sub_S_Overflow_Handler (Handler : proc_pt);

private
   task Clock_Task is
      entry Read_Clock (s : out reg_sec; sub_s : out reg_sub_sec);
      entry Write_clock (s : reg_sec; sub_s : reg_sub_sec);
      entry Set_Alarm_Time (S : reg_sec);
      entry Halt;
   end Clock_Task;

   task Call_Overflow_Handler is
      entry Call;
      entry Halt;
   end Call_Overflow_Handler;

   task Call_Alarm_Handler is
      entry Call;
      entry halt;
   end Call_Alarm_Handler;

   Overflow_Handler       : proc_pt := null;
   Alarm_Handler          : proc_pt := null;
   Sub_S_Overflow_Handler : proc_pt := null;

end software_clock_cal;
