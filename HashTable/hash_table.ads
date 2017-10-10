with System;

generic
   type Key_t is private;
   type Value_t is private;
package Hash_Table is
   type uint32 is private;
   type Pair is private;
   type Access_Pair_Array_Type is private;

   protected type hashmap is
      procedure Init (N : Long_Integer);
      procedure Clean;
      procedure Destroy;

      procedure Insert (Key : Key_t; Value : Value_t);
      function Lookup (Key : Key_t; Value : out Value_t) return Boolean;

      procedure Print_count;
   private
      Map_size : uint32;
      Pair_array : Access_Pair_Array_Type;      -- size := 1 .. map_size
      function Find_Slot (Key : Key_t) return uint32;
   end hashmap;

   protected type Growing_Hashmap is
      procedure Init (n : Long_Integer);
      procedure Clean;
      procedure Destroy;

      --procedure Combined_Insert (Key : Key_t; Value : Value_t);
      --procedure First_Insert;
      procedure Second_Insert (Key : Key_t; Value : Value_t);
      function Lookup (Key : Key_t; Value : out Value_t) return Boolean;

      procedure Print_count;
      procedure Print_Address;

      procedure Check_Resize;
   private
      --Item_Num : Aligned_uint32;
      Pair_Array : Access_Pair_Array_Type;      -- size := 1 .. map_size
      Map_Size : uint32;
      Resizing : Boolean;
      function Find_Slot (Key : Key_t) return uint32;
      procedure Insert (Key : Key_t; Value : Value_t);
   end Growing_Hashmap;
   type Access_Growing_Hashmap is access all Growing_Hashmap;
   procedure g_First_Insert (H : Access_Growing_Hashmap);        -- GLOBAL

private
   type uint32 is mod 2**32 with Size => 32;
   for uint32'Alignment use 64;
   type Pair is record
      Used : Boolean := False;
      Key : Key_t;
      Value : Value_t;
   end record;

   type Pair_Array_Type is array (uint32 range<>) of Pair;
   type Access_Pair_Array_Type is access Pair_Array_Type;

   type Aligned_uint32 is new uint32;
   for Aligned_uint32'Alignment use 64;
   g_Item_Num : Aligned_uint32;                 -- GLOBAL
   function Fetch_And_Add (Target : System.Address; Old_Value : Integer) return Integer;
   pragma Import (Intrinsic, Fetch_And_Add, "__sync_fetch_and_add_4");
end Hash_Table;
