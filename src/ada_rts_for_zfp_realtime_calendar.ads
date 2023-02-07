--with software_clock_cal;

package Ada_Rts_For_Zfp_Realtime_Calendar is
   type delta_sec is mod 2**16;
   type sec is mod 2**32;
  -- package test_clock is new software_clock_cal
    -- (register_sub_sec => delta_sec, register_sec => sec);

end Ada_Rts_For_Zfp_Realtime_Calendar;
