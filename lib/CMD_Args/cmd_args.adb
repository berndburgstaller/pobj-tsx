with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Command_line; use GNAT.Command_Line;
with CPU_Affinity;

package body CMD_Args is

   function Read_Args (bm : Benchmark_Type) return Boolean is
      Show_Help : Boolean := False;
   begin
      case bm is
         when GC => Read_GC;
         when SA => Read_SA;
         when DP => Read_DP;
         when HT => Read_HT;
         when KM => Read_KM;
      end case;

      loop
         case GetOpt("a h m: n: o: i: M: N: t:") is
            when 'h' => Show_Help := True;
            when 'a' => CPU_Affinity.Use_Affinity := True;
            when 'm' => Max_Task := Integer'Value (Parameter);
            when 'n' => Min_Task := Integer'Value (Parameter);
            when 'o' =>
               if bm /= KM then  Num_Op := Long_Integer'Value (Parameter);
               else              raise Invalid_Switch;
               end if;
            when 'i' =>
               if bm = KM then   Input_File := SB.To_Bounded_String (Parameter);
               else              raise Invalid_Switch;
               end if;
            when 'M' =>
               if bm = KM then   Max_nClusters := Integer'Value (Parameter);
               else              raise Invalid_Switch;
               end if;
            when 'N' =>
               if bm = KM then   Min_nClusters := Integer'Value (Parameter);
               else              raise Invalid_Switch;
               end if;
            when 't' =>
               if bm = KM then   Threshold := Float'Value (Parameter);
               else              raise Invalid_Switch;
               end if;
            when others => exit;
         end case;
      end loop;
      if Show_Help or Min_Task < Minimum or Max_Task < Min_Task
         or (bm = KM and then (Min_nClusters < 1 or Max_nClusters < Min_nClusters))
      then
         Put_Help (bm);
         return False;
      end if;

      if bm = GC then
         Exec_Message := SB.To_Bounded_String
            ("Global counter from" & Integer'Image (Min_Task) & " to" & Integer'Image (Max_Task)
            & " for" & Long_Integer'Image (Num_Op) & " time");
      elsif bm = SA then
         Exec_Message := SB.To_Bounded_String
            ("Shared array from" & Integer'Image (Min_Task) & " to" & Integer'Image (Max_Task)
            & " for" & Long_Integer'Image (Num_Op) & " time");
      elsif bm = DP then
         Exec_Message := SB.To_Bounded_String
            ("Dining philosophers from" & Integer'Image (Min_Task) & " to" & Integer'Image (Max_Task)
            & " for" & Long_Integer'Image (Num_Op) & " time");
      elsif bm = HT then
         Exec_Message := SB.To_Bounded_String
            ("Concurrent hash table from" & Integer'Image (Min_Task) & " to" & Integer'Image (Max_Task)
            & " for" & Long_Integer'Image (Num_Op) & " time");
      elsif bm = KM then
         Exec_Message := SB.To_Bounded_String
            ("Clustering " & SB.To_String (Input_File) & " from" & Integer'Image (Min_Task) & " to" & Integer'Image (Max_Task)
            & " between" & Integer'Image (Min_nClusters) & " and" & Integer'Image (Max_nClusters));
      end if;
      Put (SB.To_String (Exec_Message));
      if CPU_Affinity.Use_Affinity then
         Put_Line (" with cpu affinity");
      else
         Put_Line (" without cpu affinity");
      end if;
      New_Line;
      return True;

   exception
      when Invalid_Switch
         => New_Line; Put_Line ("Invalid Switch!!"); Put_Help (bm); return False;
      when Invalid_Parameter
         => New_Line; Put_Line ("Invalid Parameter!!"); Put_Help (bm); return False;
   end Read_Args;

   procedure Put_Help (bm : Benchmark_Type) is
   begin
      New_Line;
      Put_Line (SB.To_String (Usage));
      Put_Line (SB.To_String (Description));
      New_Line;
      Put_Line ("Options");
      Put_Line ("   -h                       Show this");
      Put_Line ("   -a                       Use CPU affinity for each task");
      Put_Line ("   -m=MAX_TASK              Set Max_Task");
      Put_Line ("   -n=MIN_TASK              Set Min_Task (>" & Integer'Image (Minimum) & ")");
      if bm /= KM then
         Put_Line ("   -o=NUM_OP                Set Num_Op");
      else
         Put_Line ("   -i=INPUT_FILE            Set Input_File");
         Put_Line ("   -M=MAX_NCLUSTERS         Set Max_nClusters");
         Put_Line ("   -N=MIN_NCLUSTERS         Set Min_nClusters");
         Put_Line ("   -t=THRESHOLD             Set Threshold");
      end if;
      New_Line;
   end Put_Help;

   procedure Read_GC is
   begin
      Minimum := 1;
      Min_Task := Minimum; Max_Task := Minimum;
      Num_Op := 30000000;
      Usage := SB.To_Bounded_String ("Usage: ./test_gc [OPTION]...");
      Description := SB.To_Bounded_String
         ("Test global counter from 'Min_Task' to 'Max_Task' for 'Num_Op' time in total");
   end Read_GC;

   procedure Read_SA is
   begin
      Minimum := 1;
      Min_Task := Minimum; Max_Task := Minimum;
      Num_Op := 30000000;
      Usage := SB.To_Bounded_String ("Usage: ./test_sa [OPTION]...");
      Description := SB.To_Bounded_String
         ("Test shared array from 'Min_Task' to 'Max_Task' for 'Num_Op' time in total");
   end Read_SA;

   procedure Read_DP is
   begin
      Minimum := 2;
      Min_Task := Minimum; Max_Task := Minimum;
      Num_Op := 1000000;
      Usage := SB.To_Bounded_String ("Usage: ./test_dp [OPTION]...");
      Description := SB.To_Bounded_String
         ("Test dining philosophers from 'Min_Task' to 'Max_Task' for 'Num_Op' time for individual tasks");
   end Read_DP;

   procedure Read_HT is
   begin
      Minimum := 1;
      Min_Task := Minimum; Max_Task := Minimum;
      Num_Op := 50000000;
      Usage := SB.To_Bounded_String ("Usage: ./test_ht [OPTION]...");
      Description := SB.To_Bounded_String
         ("Test concurrent hash table from 'Min_Task' to 'Max_Task' for 'Num_Op' time in total");
   end Read_HT;

   procedure Read_KM is
   begin
      Minimum := 1;
      Min_Task := Minimum; Max_Task := Minimum;
      Min_nClusters := 100;
      Max_nClusters := 100;
      Threshold := 0.0001;
      Input_File := SB.To_Bounded_String ("inputs/65536-32");
      Usage := SB.To_Bounded_String ("Usage: ./test_kmeans [OPTION]...");
      Description := SB.To_Bounded_String
         ("Test K-means clustering from 'Min_Task' to 'Max_Task' to find the best cluster"
          & "between 'Max_nClusters' and 'Min_nClusters'");
   end Read_KM;
end CMD_Args;
