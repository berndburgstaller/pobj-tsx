with Interfaces; use Interfaces;

package Random is

   N : constant Unsigned_32 := 624;
   M : constant Unsigned_32 := 397;

   type Random_Arr is array(1 .. N) of Unsigned_32;
   type Random_t is record
      Mt : aliased Random_Arr;
      Mti : Unsigned_32;
   end record;

   procedure Random_Seed (Random_Ptr : access Random_t; Seed : Unsigned_32);
   function Random_Generate (Random_Ptr : access Random_t) return Unsigned_32;

   function sqrt (a : Long_Float) return Long_Float;
   pragma Import (C, sqrt, "sqrt");
end Random;
