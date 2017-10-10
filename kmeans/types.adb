package body Types is

   protected body global_status is
      procedure Add_Index (a : Integer; Start_Pos : out Integer) is begin
         Start_Pos := Q_Index;
         Q_Index := Q_Index + a;
      end Add_Index;
      procedure Set_Index (i : Integer) is begin
         Q_Index := i;
      end Set_Index;
      procedure Add_Delta (a : Float) is begin
         Delt := Delt + a;
      end Add_Delta;
      procedure Set_Delta (d : Float) is begin
         Delt := d;
      end Set_Delta;
      function Get_Index return Integer is begin
         return Q_Index;
      end Get_Index;
      function Get_Delta return Float is begin
         return Delt;
      end Get_Delta;
   end;

   protected body Center_t is
      procedure Init (nClusters : Integer; nFeatures : Integer) is
         Cluster_Size : Integer;
         Cacheline : Integer := 64;
         Data_Size : Integer := Center_Data'Size / 8;     -- Size in Byte
      begin
         Cluster_Size := Data_Size * (nFeatures + 1);
         Cluster_Size := Cluster_Size + (Cacheline-1) - ((Cluster_Size-1) mod Cacheline);
         Data := new Center_Info (1 .. nClusters, 0 .. (Cluster_Size/Data_Size - 1));

         for i in 1 .. nClusters loop
            for j in  0 .. nFeatures loop
               Data (i, j) := 0.0;
            end loop;
         end loop;
      end Init;
      procedure Destroy is
      begin
         Free_Center_Info (Data);
      end Destroy;
      procedure Write_Len (i : Integer; l : Float) is
      begin
         Data(i, 0) := Center_Data (l);
      end Write_Len;
      procedure Write_Points (I_Const : Integer; i : Integer; nFeatures: Integer; Feature : access Float_Db_Arr) is
      begin
         Data (I_Const, 0) := Data (I_Const, 0) + 1.0;
         for j in 1 .. nFeatures loop
            Data (I_Const, j) := Data (I_Const, j) + Center_Data (Feature (i, j));
         end loop;
      end Write_Points;
      procedure Init_Point (i : Integer; j : Integer) is
      begin
         Data (i, j) := Center_Data (0.0);
      end Init_Point;
      function Read_Len (i : Integer) return Float is
      begin
         return Float (Data (i, 0));
      end Read_Len;
      function Read_Point (i : Integer; j : Integer) return Float is
      begin
         return Float (Data (i, j));
      end Read_Point;
   end Center_t;

end Types;
