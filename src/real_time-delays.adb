------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . R E A L _ T I M E . D E L A Y S                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 2001-2020, AdaCore                     --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

package body Real_Time.Delays is

   -- Delay unit Duration has passed.

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : time) is
      now : time := Clock;
   begin
      -- check the Clock until the time has come
      while now < Clock loop
         now := Clock;
      end loop;
   end Delay_Until;

   procedure Delay_For (D : Duration) is
      timeout : time      := Clock;
      delta_t : Time_Span := To_Time_Span (D);
   begin
      Delay_Until (timeout + delta_t);
   end Delay_For;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (T : time) return Duration is
      now     : time := Clock;
      delta_t : Time_Span;
      result  : Duration;
   begin
      delta_t := T - now;
      result  := To_Duration (delta_t);
      declare
         check : Long_Long_Long_Integer := Long_Long_Long_Integer (result);
      begin
         if check < 0 then
            raise Time_Error with "time is in the past";
         else
            return result;
         end if;
      end;
   end To_Duration;

end Real_Time.Delays;
