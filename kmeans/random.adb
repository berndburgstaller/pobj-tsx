with Interfaces; use Interfaces;

package body Random is
   procedure Random_Seed (Random_Ptr : access Random_t; Seed : Unsigned_32) is
   begin
      Random_Ptr.all.Mt (1) := Seed and 16#ffffffff#;
      for Mti in 2 .. N loop
         Random_Ptr.all.Mt (Mti) := Unsigned_32 (1812433253) *
            (Random_Ptr.all.Mt (Mti-1) xor (Shift_Right (Random_Ptr.all.Mt (Mti-1), 30)))
            + Mti - 1;
         Random_Ptr.all.Mt (Mti) := Random_Ptr.all.Mt (Mti) and 16#ffffffff#;
      end loop;
      Random_Ptr.Mti := N+1;
   end Random_Seed;

   function Random_Generate (Random_Ptr : access Random_t) return Unsigned_32 is
      Mt : access random_arr := Random_Ptr.all.Mt'Access;
      Mti : Unsigned_32 := Random_Ptr.Mti;
      Y : Unsigned_32;
      Mag01 : constant array (0 .. 1) of Unsigned_32 := (0, 16#9908b0df#);
      UPPER_MASK : constant Unsigned_32 := 16#80000000#;
      LOWER_MASK : constant Unsigned_32 := 16#7fffffff#;
   begin
      if Mti >= N+1 then
         if Mti = N+2 then
            Random_Seed (Random_Ptr, Unsigned_32(5489));
         end if;
         for kk in 1 .. N-M loop
            Y := (Mt (kk) and UPPER_MASK) or (Mt (kk+1) and LOWER_MASK);
            Mt (kk) := Mt (kk+M) xor Shift_Right (Y, 1) xor Mag01 (Integer (Y and 16#1#));
         end loop;
         for kk in N-M+1 .. N-1 loop
            Y := (Mt (kk) and UPPER_MASK) or (Mt (kk+1) and LOWER_MASK);
            Mt (kk) := Mt (kk+(M-N)) xor Shift_Right (Y, 1) xor Mag01 (Integer (Y and 16#1#));
         end loop;
         Y := (Mt (N) and UPPER_MASK) or (Mt (1) and LOWER_MASK);
         Mt (N) := Mt (M) xor Shift_Right (Y, 1) xor Mag01 (Integer (Y and 16#1#));

         Mti := 1;
      end if; 
      Y := Mt (Mti);

      Y := y xor Shift_Right (y, 11);
      Y := y xor (Shift_Left (y, 7) and 16#9d2c5680#);
      Y := y xor (Shift_Left (y, 15) and 16#efc60000#);
      Y := y xor Shift_Right (y, 18);

      Random_Ptr.Mti := Mti+1;

      return Y;
   end Random_Generate;
end Random;
