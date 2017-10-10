with Ada.Strings.Bounded;

package CMD_Args is
   package SB is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 200);
   type Benchmark_Type is (GC, SA, DP, HT, KM);

   Min_Task, Max_Task : Integer;
   Num_Op : Long_Integer;
   Threshold : Float;
   Input_File : SB.Bounded_String;
   Min_nClusters, Max_nClusters : Integer;

   function Read_Args (bm : Benchmark_Type) return Boolean;

private
   Minimum : Integer;
   Usage, Description, Exec_Message : SB.Bounded_String;
   More_Condition : Boolean;

   procedure Put_Help (bm : Benchmark_Type);
   procedure Read_GC;
   procedure Read_SA;
   procedure Read_DP;
   procedure Read_HT;
   procedure Read_KM;
end CMD_Args;
