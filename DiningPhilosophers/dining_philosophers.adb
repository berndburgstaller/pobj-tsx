with Ada.Text_IO; use Ada.Text_IO;
with System; use type System.Address;
with System.Storage_Elements; use type System.Storage_Elements.Storage_Offset;
with System.Address_Image;
with Ada.Containers.Generic_Array_Sort; use Ada.Containers;

package body Dining_Philosophers is

   protected body Fork is

      procedure Acquire (Ret : out Boolean) is
      begin
         if Available = True then
            Available := False;
            Ret := True;
         else
            Ret := False;
         end if;
      end Acquire;

      procedure Release is
      begin
         Available := True;
      end Release;

      function Availability return Boolean is
      begin
         return Available;
      end Availability;

      function Get_Address return System.Address is
      begin
         return Available'Address;
      end Get_Address;

   end Fork;

   task body Philosopher is
      Is_Acquired : Boolean;
      PID : Integer;
      First, Second : Integer;
      Quota : Long_Integer;
   begin
      accept Set_PID (ID : Integer; Num_Op : Long_Integer; N : Integer) do
         PID := ID;
         Quota := Num_Op;

         if ((PID + 1) mod N) /= 1 then
            First := PID; Second := PID + 1;
         else
            First := 1;   Second := PID;
         end if;
      end Set_PID;

      for iter in 1 .. Quota loop
         loop
            Fork_Array (First).Acquire (Is_Acquired);
            exit when Is_Acquired = True;
         end loop;
         loop
            Fork_Array (Second).Acquire (Is_Acquired);
            exit when Is_Acquired = True;
         end loop;

         Fork_Array (First).Release;
         Fork_Array (Second).Release;
      end loop;

      accept Finish_Meal do
         null;
      end Finish_Meal;
   end Philosopher;

   subtype Fork_Address is System.Address;
   type Fork_Address_Array is array (Natural range <>) of Fork_Address;

   procedure Test_False_Sharing (Array_Size : Integer; Cacheline_Size : Positive) is
      x : Fork_Address;
      Prev_Addr : Fork_Address := System.Null_Address;
      Offset : System.Storage_Elements.Storage_Offset;
      Offset_64 : System.Storage_Elements.Storage_Offset := 64;
      Addr_Array2 : Fork_Address_Array (1 .. Array_Size);

      procedure Sort2 is new Ada.Containers.Generic_Array_Sort
         (Index_Type   => Natural,
         Element_Type => Fork_Address,
         Array_Type   => Fork_Address_Array);

   begin
      for Fork_Index in 1 .. Array_Size loop
         x := Fork_Array (Fork_Index).Get_Address;
         Addr_Array2 (Fork_Index) := x;
      end loop;

      Sort2 (Addr_Array2);
      for Address in Addr_Array2'Range loop
         if Address > 1 then
            Offset := Addr_Array2 (Address) - Prev_Addr;
            if Offset < Offset_64 then
               Put_Line ("False sharing detected!");
            end if;
         end if;
         Prev_Addr := Addr_Array2 (Address);
      end loop;
   end Test_False_Sharing;

end Dining_Philosophers;
