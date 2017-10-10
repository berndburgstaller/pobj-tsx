with Ada.Task_Identification;

package CPU_Affinity is
   Use_Affinity : Boolean := False;
   procedure Set_Affinity (Index : Integer;
                           Id : Ada.Task_Identification.Task_Id);
end CPU_Affinity;
