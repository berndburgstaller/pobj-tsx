with Types; use Types;

package Execute is
   procedure Extract_Moments
            (Data : Float_Arr; Num_Elts : Integer; Num_Moments : Integer;
             M0 : out Float ; M1 : out Float);

   procedure Zscore_Transform
            (Data : access Float_Db_Arr; nObj : Integer; nAttr : Integer);

   procedure Cluster_Exec
            (nThreads : Integer; nObj : Integer; nAttr : Integer; 
             Attributes : access Float_Db_Arr; Use_Zscore_Transform : Boolean;
             Min_nClusters : Integer; Max_nClusters : Integer; Threshold : Float;
             Best_nClusters : out Integer; Cluster_Centers : out Float_Db_Arr_Access;
             Cluster_Assign : access Int_Arr);
end Execute;
