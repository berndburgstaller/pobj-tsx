with GNAT.Command_Line; use GNAT.Command_Line;
with Dining_Philosophers; use Dining_Philosophers;
with Ada.Text_IO; use Ada.Text_IO;
with PAPI_Binder; use PAPI_Binder;
with CPU_Affinity; use CPU_Affinity;
with CMD_Args; use CMD_Args;

procedure Test_DP is
begin
   if not Read_Args (DP) then
      return;
   end if;

   for N_Task in Min_Task .. Max_Task loop
      Put_Line (Integer'Image (N_Task) & " tasks.");
      Fork_Array := new Fork_Array_Type (1 .. N_Task);
      --  Dining_Philosophers.Test_FalseSharing (N_Task, 64);

      PAPI_Start;
      declare
         Person_Array : array (1 .. N_Task) of Philosopher;
      begin
         for Id in Person_Array'Range loop
            Person_Array (Id).Set_PID (Id, Num_Op, N_Task);
            Set_Affinity (Id, Person_Array (Id)'Identity);  -- No effect if -a option is not given
         end loop;

         for Id in Person_Array'Range loop
            Person_Array (Id).Finish_Meal;
         end loop;
      end;
      PAPI_Finish;
      New_Line;

      Free_Fork_Array (Fork_Array);
   end loop;
end Test_DP;
