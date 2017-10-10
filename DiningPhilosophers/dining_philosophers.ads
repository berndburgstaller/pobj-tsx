with System;
with Ada.Unchecked_Deallocation;

package Dining_Philosophers is

   protected type Fork is

      procedure Acquire (Ret : out Boolean);
      procedure Release;

      function Availability return Boolean;
      function Get_Address return System.Address;
      -- Return the address of the fork's state variable to allow checking
      -- for false sharing.
   private
      Available : Boolean := True;
   end Fork;
   type Fork_Array_Type is array (Positive range<>) of Fork;
   type Access_Fork_Array is access Fork_Array_Type;
   Fork_Array : Access_Fork_Array;
   procedure Free_Fork_Array is new Ada.Unchecked_Deallocation (Fork_Array_Type, Access_Fork_Array);

   task type Philosopher is
      entry Set_PID (ID : Integer; Num_Op : Long_Integer; N : Integer);
      entry Finish_Meal;
   end Philosopher;

   procedure Test_False_Sharing (Array_Size : Integer; Cacheline_Size : Positive);
end Dining_Philosophers;
