with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Assertions; use Ada.Assertions;
with Interfaces;        use Interfaces;
with Ada.Unchecked_Conversion;

with System.Address_Image;
with Ada.Calendar; use Ada.Calendar;

package body Hash_Table is
   procedure Free_Hashmap is new Ada.Unchecked_Deallocation
      (Pair_Array_Type, Access_Pair_Array_Type);

   type Byte is mod 2**8 with Size => 8;
   type Byte_Array is array (1 .. Key_t'size/8) of Byte;
   function To_Byte_Array is new Ada.Unchecked_Conversion
      (Source => Key_t, Target => Byte_Array);

   function Joaat_Hash (Key : Key_t) return uint32 is
      Hash : uint32 := 0;
      Key_Bytes : Byte_Array := To_Byte_Array (Key);
   begin
      for i in 1 .. (Key_t'Size/8) loop
         Hash := Hash + uint32 (Key_Bytes (i));
         Hash := Hash + uint32 (Shift_Left (Unsigned_32 (Hash), 10));
         Hash := HasH xor uint32 (Shift_Right (Unsigned_32 (Hash), 6));
      end loop;
      Hash := Hash + uint32 (Shift_Left (Unsigned_32 (Hash), 3));
      Hash := Hash xor uint32 (Shift_Right (Unsigned_32 (Hash), 11));
      Hash := Hash + uint32 (Shift_Left (Unsigned_32 (Hash), 15));

      return Hash; 
   end Joaat_Hash;
   ----------------------------------------------

   protected body Hashmap is
      procedure Init (N : Long_Integer) is
      begin
         Map_Size := uint32 (N);
         Pair_Array := new Pair_Array_Type (1 .. Map_Size);
      end init;

      procedure Insert (Key : Key_t; Value : Value_t) is
         Index : uint32;
      begin
         Index := Find_Slot (Key);
         if Index = -1 then
            Put_Line ("Not enough space in the hash table.");
         elsif Pair_Array (Index + 1).Used then   -- already occupied by same Key
            Pair_Array (Index + 1).Value := Value;
         else                                      -- insert new
            Pair_Array (Index + 1) := (True, Key, Value);
         end if;
      end Insert;

      function Lookup (Key : Key_t; Value : out Value_t) return Boolean is
         Index : uint32;
      begin
         Index := Find_Slot (Key) + 1;
         if not Pair_Array (Index).Used then
            return False;
         else
            Value := Pair_Array (Index).Value;
            return True;
         end if;
      end Lookup;

      function Find_Slot (Key : Key_t) return uint32 is
         Start : uint32 := Joaat_Hash (Key) mod uint32 (Map_Size);
         Index : uint32 := Start;
      begin
         loop
            if (not Pair_Array (Index + 1).Used) or else
               Pair_Array (Index + 1).Key = key
            then
               return Index;
            end if;
            Index := (Index + 1) mod uint32(Map_Size);
            --  open addr. likely to cause abort when Pair_Array is almost full
            exit when Index = Start;
         end loop;
         return -1;
      end Find_Slot;

      procedure Clean is 
      begin
         for i in 1 .. Map_Size loop
            if Pair_Array (i).Used then
               Pair_Array (i).Used := False;
            end if;
         end loop;
      end Clean;

      procedure Destroy is 
      begin
         Free_Hashmap (Pair_Array);
      end Destroy;

      procedure Print_Count is
         Count : uint32 := 0;
      begin
         for i in 1 .. Map_Size loop
            if Pair_Array(i).Used then
               Count := Count + 1;
            end if;
         end loop;
         Put_Line (uint32'Image (Count));
      end Print_Count;
   end Hashmap;

   -----------------------------------------------------------------
   procedure g_First_Insert (H : Access_Growing_Hashmap) is     -- GLOBAL
      tmp : Integer;
   begin
      H.Check_Resize;
      tmp := Fetch_And_Add (g_Item_Num'Address, 1);
   end g_First_Insert;

   protected body Growing_Hashmap is
      procedure Init (N : Long_Integer) is
      begin
         g_Item_Num := 0;                                       -- GLOBAL
         --Item_Num := 0;
         Map_Size := uint32 (N);
         Pair_Array := new Pair_Array_Type (1 .. Map_Size);
         Resizing := False;
      end Init;

      procedure Insert (Key : Key_t; Value : Value_t) is
         Index : uint32;
      begin
         Index := Find_Slot (Key);
         if Pair_Array (Index + 1).Used then            -- already occupied by same Key
            Pair_Array (Index + 1).Value := Value;
         else                                           -- insert new
            Pair_Array (Index + 1) := (True, Key, Value);
         end if;
      end Insert;

      --procedure Combined_Insert(Key : Key_t; Value : Value_t) is
      --begin
      --   while Resizing loop
      --      null;       --pause?
      --   end loop;
      --   Item_Num := Item_Num + 1;
      --   if Float (Item_Num) > (0.7 * Float (Map_Size)) then
      --      Check_Resize;
      --   end if;
      --   Insert (Key, Value);
      --end Combined_Insert;

      --procedure First_Insert is
      --begin
      --   Item_Num := Item_Num + 1;
      --   Check_Resize;
      --end First_Insert;

      procedure Second_Insert (Key : Key_t; Value : Value_t) is
         Index : uint32;
      begin
         Index := Find_Slot (Key);
         if Pair_Array (Index + 1).Used then            -- already occupied by same Key
            Pair_Array (Index + 1).Value := Value;
         else                                           -- insert new
            Pair_Array (Index + 1) := (True, Key, Value);
         end if;
      end Second_Insert;

      function Lookup (Key : Key_t; Value : out Value_t) return Boolean is
         Index : uint32;
      begin
         Index := Find_Slot (Key) + 1;
         if not Pair_Array (Index).Used then
            return False;
         else
            Value := Pair_Array (Index).value;
            return True;
         end if;
      end Lookup;

      function Find_Slot (Key : Key_t) return uint32 is
         Start : uint32 := Joaat_Hash (Key) mod Map_Size;
         Index : uint32 := Start;
      begin
         loop
            if (not Pair_Array (Index + 1).Used)
               or else Pair_Array (Index + 1).Key = key
            then
               return Index;
            end if;
            Index := (Index + 1) mod Map_Size;
            --  open addr. likely to cause abort when Pair_Array is almost full
            exit when Index = Start;
         end loop;
         Assert (False);
         return -1;
      end Find_Slot;

      procedure Check_Resize is
         Old_Parray : Access_Pair_Array_Type;
         Index : uint32;
         --From : Time;
      begin
         while Resizing loop
            null;       --pause?
         end loop;
         if Float (g_Item_Num) > (0.7 * Float (Map_Size)) then                   -- GLOBAL
            --Put ("Resizing from" & Map_Size'Img & " to" & uint32'Image (Map_Size*2) & "...    "); From := Clock;
            Resizing := True;
            Map_Size := Map_Size * 2;
            Old_Parray := Pair_Array;
            Pair_Array := new Pair_Array_Type (1 .. Map_Size);

            for i in 1 .. Map_Size/2 loop
               if Old_Parray (i).Used then
                  Index := Find_Slot (Old_Parray (i).Key);
                  if Pair_Array (Index + 1).Used then
                     Put_Line ("Key-Value corruption");
                     Assert (False);
                     --Pair_Array (Index + 1).Value := Old_Value;
                  else
                     Pair_Array (Index + 1) := Old_Parray (i);
                  end if;
               end if;
            end loop;

            Free_Hashmap (Old_Parray);
            Resizing := False;
            --Put_Line ("Done"); Put_Line (Duration'Image (Clock-From));
         end if;
      end Check_Resize;

      procedure Clean is 
      begin
         for i in 1 .. Map_Size loop
            if Pair_Array (i).Used then
               Pair_Array (i).Used := False;
            end if;
         end loop;
      end Clean;

      procedure Destroy is 
      begin
         Free_Hashmap (Pair_Array);
      end Destroy;

      procedure Print_Count is
         Count : uint32 := 0;
      begin
         for i in 1 .. Map_Size loop
            if Pair_Array (i).Used then
               Count := Count + 1;
            end if;
         end loop;
         Put_Line (uint32'Image (Count));
      end Print_Count;

      procedure Print_Address is
      begin
         Put_Line ("Item_Num: " & System.Address_Image (g_Item_Num'Address) & ", " & Integer'Image (g_Item_Num'Size));
         Put_Line ("Pair_Arr: " & System.Address_Image (Pair_Array'Address) & ", " & Integer'Image (Pair_Array'Size));
         Put_Line ("Map_Size: " & System.Address_Image (Map_Size'Address) & ", " & Integer'Image (Map_Size'Size));
         Put_Line ("Resizing: " & System.Address_Image (Resizing'Address) & ", " & Integer'Image (Resizing'Size));
      end Print_Address;
   end Growing_Hashmap;
end Hash_Table;
