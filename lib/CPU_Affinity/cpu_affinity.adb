with Ada.Task_Identification; use Ada.Task_Identification;
with System.Multiprocessors; use System.Multiprocessors;
with System.Multiprocessors.Dispatching_Domains; use System.Multiprocessors.Dispatching_Domains;

package body CPU_Affinity is
   procedure Set_Affinity (Index : Integer; Id : Task_Id) is
   begin
      if Use_Affinity then
         Set_CPU (CPU_Range (Index), Id);
      end if;
   end Set_Affinity;
end CPU_Affinity;
