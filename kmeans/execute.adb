with types; use types;
with Random; use Random;
with cluster; use cluster;

package body Execute is
   procedure Cluster_Exec
            (nThreads : Integer; nObj : Integer; nAttr : Integer; 
             Attributes : access Float_Db_Arr; Use_Zscore_Transform : Boolean;
             Min_nClusters : Integer; Max_nClusters : Integer; Threshold : Float;
             Best_nClusters : out Integer; Cluster_Centers : out Float_Db_Arr_Access;
             Cluster_Assign : access int_arr)
   is
      Random : aliased Random_t;
      Tmp_Cluster_Centers : Float_Db_Arr_Access;
   begin
      Membership := new Member_Arr (1 .. nObj);
      for i in 1 .. nObj loop
         Membership (i) := -1;
      end loop;

      if Use_Zscore_Transform then
         Zscore_Transform (Attributes, nObj, nAttr);
      end if;

      for nClusters in Min_nClusters .. Max_nClusters loop
         Random_Seed (Random'Access, 7);
         Tmp_Cluster_Centers := Normal_Exec (nThreads, Attributes.all, nAttr, nObj,
                                             nClusters, Threshold, Random'Access);
         if Cluster_Centers /= null then
            Free_Float_Db_Arr (Cluster_Centers);
         end if;
         Cluster_Centers := Tmp_Cluster_Centers;
         Best_nClusters := nClusters;           -- why always best=n??
      end loop;

      Free_Member_Arr (Membership);
   end Cluster_Exec;

   procedure Extract_Moments
            (Data : Float_Arr; Num_Elts : Integer; Num_Moments : Integer;
             M0 : out Float ; M1 : out Float) is
   begin
      M0 := 0.0;
      for i in 1 .. Num_Elts loop
         M0 := M0 + Data (i);
      end loop;

      M0 := M0 / Float(Num_Elts);
      for j in 2 .. Num_Moments loop
         M1 := 0.0;
         for i in 1 .. Num_Elts loop
            M1 := M1 + ((Data(i)-M0) ** j);
         end loop;
         M1 := M1 / Float(Num_Elts);
      end loop;
   end Extract_Moments;

   procedure Zscore_Transform
            (Data : access Float_Db_Arr; nObj : Integer; nAttr : Integer)
   is
      Single_Var : Float_Arr(1 .. nObj);
      M0, M1 : Float;
   begin
      for i in 1 .. nAttr loop
         for j in 1 .. nObj loop
            Single_Var(j) := Data(j,i);
         end loop;
         Extract_Moments(Single_Var, nObj, 2, M0, M1);
         M1 := Float (sqrt(Long_Float(M1)));
         for j in 1 .. nObj loop
            Data (j, i) := (Data (j, i) - M0) / M1;
         end loop;
      end loop;
   end Zscore_Transform;
end Execute;
