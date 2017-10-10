with System; use System;
with System.Address_To_Access_Conversions;
with Interfaces; use Interfaces;

with Types; use Types;
with Ada.Calendar; use Ada.Calendar;
with CPU_Affinity; use CPU_Affinity;

package body Cluster is
   Global_s : Global_Status;
   New_Centers : Center_t;

   package Access_Fl_Db_Arr is new System.Address_To_Access_Conversions (Float_Db_Arr);
   package Access_Member_Arr is new System.Address_To_Access_Conversions (Member_Arr);

   task body Concurrent_Work is
      Local_Delta : Float := 0.0;
      My_Id, Start_Pos, Stop_Pos : Integer;
      Feature : access Float_Db_Arr;
      nFeatures, nPoints : Integer;
      nClusters : Integer;
      Clusters : access Float_Db_Arr;
      Index : Integer;
      Is_Repeat : Boolean;
   begin
      accept Init (T_Id : Integer; Feat : System.Address; nFeat : Integer;
                   nPoin : Integer; nClust : Integer; Clust : System.Address)
      do
         My_Id := T_Id;
         nFeatures := nFeat;
         nPoints := nPoin;
         nClusters := nClust;
         Feature := Access_Fl_Db_Arr.To_Pointer(Feat);
         Clusters := Access_Fl_Db_Arr.To_Pointer(Clust);
      end;

      loop
         accept Start do
            null;
         end;

         Start_Pos := (My_Id-1) * CHUNK;
         while Start_Pos < nPoints loop
            if (Start_Pos+CHUNK) < nPoints   then 
               Stop_Pos := Start_Pos + CHUNK;
            else
               Stop_Pos := nPoints;
            end if;

            for i in (Start_Pos+1) .. Stop_Pos loop
               Index := Find_Nearest (Feature, i, nFeatures, Clusters, nClusters);
               if Membership (i) /= Member (Index) then
                  Local_Delta := Local_Delta + 1.0;
                  Membership (i) := Member (Index);
               end if;
               ----------------Transaction begins for private work-------------------
               New_Centers.Write_points (Index, i, nFeatures, Feature);
               ----------------Ends for private work-------------------
            end loop;

            if (Start_Pos + CHUNK) < nPoints then
               ----------------Transaction begins for global status-------------------
               Global_s.Add_Index (CHUNK, Start_Pos);
               ----------------Ends for private work-------------------
            else
               exit;
            end if;
         end loop;

         --------Transaction begins for global status-------------------
         Global_s.Add_Delta (Local_Delta);
         --------Ends for private work-------------------

         accept Finish do
            null;
         end;

         accept Repeat(Val : Boolean) do
            Is_Repeat := Val;
         end;
         exit when not Is_Repeat;
      end loop;
   end;

   function Normal_exec (nThreads : Integer; Feature : Float_Db_Arr; nFeatures : Integer;
                         nPoints : Integer; nClusters : Integer; Threshold : Float;
                         Random_Ptr : access Random_t) return Float_Db_Arr_Access 
   is
      Clusters : Float_Db_Arr_Access := new Float_Db_Arr (1 .. nClusters, 1 .. nFeatures);
      N, Loop_Count : Integer;
      Start, Stop : Time;
      Delt : Float;
      Work_Group : array (1 .. nThreads) of Concurrent_Work;
   begin
      for i in 1 .. nClusters loop
         N := Integer(Random_Generate (Random_Ptr) mod Unsigned_32 (nPoints)) + 1;
         for j in 1 .. nFeatures loop
            Clusters (i, j) := Feature (N, j);
         end loop;
      end loop;

      for i in 1 .. nPoints loop
         Membership (i) := -1;
      end loop;
      New_Centers.Init (nClusters, nFeatures);
      --initialize global object 'New_Centers'

      Start := Clock;
      for i in 1 .. nThreads loop
         Work_Group (i).Init (i, Feature'Address, nFeatures, nPoints, nClusters, Clusters.all'Address);
         Set_Affinity (i, Work_Group  (i)'Identity);     -- No effect if -a option is not given while running test_kmeans
      end loop;
      Loop_Count := -1;
      loop
         Delt := 0.0;
         Global_s.Set_Index (nThreads * CHUNK);
         Global_s.Set_Delta (Delt);

         for i in 1 .. nThreads loop
            Work_Group (i).Start;
         end loop;
         for i in 1 .. nThreads loop
            Work_Group (i).finish;
         end loop;
         Delt := Global_s.Get_Delta;

         for i in 1 .. nClusters loop
            for j in 1 .. nFeatures loop
               Clusters(i,j) := New_Centers.Read_Point (i, j) / New_Centers.Read_Len (i);
               New_Centers.Init_Point (i, j);
            end loop;
            New_Centers.Write_Len (i, 0.0);
         end loop;

         Delt := Delt / Float (nPoints);
         Loop_Count := Loop_Count + 1;
         exit when (Loop_Count = 500);
         --exit when (Delt <= Threshold) or (Loop_Count = 500);

         for i in 1 .. nThreads loop
            Work_Group (i).Repeat (True);
         end loop;
      end loop;
      for i in 1 .. nThreads loop
         Work_Group (i).Repeat (False);
      end loop;

      Stop := Clock;
      Total_Time := Total_Time + (Stop - Start);

      New_Centers.Destroy;
      return Clusters;
   end;

   function Find_Nearest (Feature : access Float_Db_Arr; I_Const : Integer;
                          nFeatures : Integer; Clusters : access Float_Db_Arr;
                          nClusters : Integer) return Integer
   is
      Index : Integer := -1;
      Dist : Float;
      Max_Dist : Float := Float'Last;
      Limit : constant Float := 0.99999;
   begin
      for i in 1 .. nClusters loop
         Dist := 0.0;
         for j in 1 .. nFeatures loop -- calcualting euclid distance
            Dist := Dist +
            (Feature (I_Const, j) - Clusters(i, j)) * (Feature (I_Const, j) - Clusters(i, j));
         end loop;
         if (Dist / Max_Dist) < Limit then
            Max_Dist := Dist;
            Index := i;
            if Max_Dist = 0.0 then
               exit;
            end if;
         end if;
      end loop;
      return Index;
   end;
end Cluster;

