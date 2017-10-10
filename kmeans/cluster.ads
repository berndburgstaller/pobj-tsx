with System; use System;

with Types; use Types;
with Random; use Random;

package Cluster is

   CHUNK : constant Integer := 3;

   task type Concurrent_Work is
      entry Init (T_Id : Integer; Feat : System.Address; nFeat : Integer; nPoin : Integer; nClust : Integer; Clust : System.Address);
      entry Start;
      entry Finish;
      entry Repeat(Val : Boolean);
   end;
   function Normal_Exec (nThreads : Integer; Feature : Float_Db_Arr; nFeatures : Integer; nPoints : Integer;
                     nClusters : Integer; Threshold : Float;  Random_Ptr : access Random_t) return Float_Db_Arr_Access;
   function Find_Nearest (Feature : access Float_Db_Arr; I_Const : Integer;
                          nFeatures : Integer; Clusters : access Float_Db_Arr;
                          nClusters : Integer) return Integer;
end Cluster;
