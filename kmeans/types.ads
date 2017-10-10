with Ada.Unchecked_Deallocation;

package Types is
   type Float_Arr is array (Positive range <>) of Float;
   type Float_Arr_Access is access Float_Arr;
   procedure Free_Float_Arr is new Ada.Unchecked_Deallocation (Float_Arr, Float_Arr_Access);

   type Float_Db_Arr is array (Positive range <>, Positive range <>) of Float;
   type Float_Db_Arr_Access is access Float_Db_Arr;
   procedure Free_Float_Db_Arr is new Ada.Unchecked_Deallocation (Float_Db_Arr, Float_Db_Arr_Access);

   type Int_Arr is array (Positive range <>) of Integer; 
   type Int_Arr_Access is access Int_Arr;
   procedure Free_Int_Arr is new Ada.Unchecked_Deallocation (Int_Arr, Int_Arr_Access);

   type Member is new Integer;
   type Member_Arr is array (Positive range <>) of Member;
   type Member_Arr_Access is access Member_Arr;
   Membership : Member_Arr_Access;
   procedure Free_Member_Arr is new Ada.Unchecked_Deallocation (Member_Arr, Member_Arr_Access);

   protected type Global_Status is
      procedure Add_Index (a : Integer; Start_Pos : out Integer);
      procedure Set_Index (i : Integer);
      procedure Add_Delta (a : Float);
      procedure Set_Delta (d : Float);
      function Get_Index return Integer;
      function Get_Delta return Float;
   private
      Q_Index : Integer;   -- global queue index
      Delt : Float;      -- global delta
   end Global_Status;


   subtype Non_Negative is Integer range 0 .. Integer'Last;

   type Center_Data is new Float;
   type Center_Info is array (Positive range <>, Non_Negative range <>) of Center_Data;
   pragma Pack (Center_Info);
   for Center_Info'Alignment use 64;    --Improve 10%
   type Center_Info_Access is access Center_Info;
   procedure Free_Center_Info is new Ada.Unchecked_Deallocation (Center_Info, Center_Info_Access);

   protected type Center_t is
      procedure Init (nclusters : Integer; nFeatures : Integer);
      procedure Destroy;
      procedure Write_Len (i : Integer; l : Float);
      procedure Write_Points (I_Const : Integer; i : Integer; nFeatures: Integer; Feature : access Float_Db_Arr) ;
      procedure Init_Point (i : Integer; j : Integer);
      function Read_Len(i : Integer) return Float;
      function Read_Point (i : Integer; j : Integer) return Float ;
   private
      Data : Center_Info_Access;
   end Center_t;

   Total_Time : Duration := Duration (0);
end Types;
