with Ada.Unchecked_Deallocation;
with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;

package body PAPI_Binder is
   procedure Free_Event is new Ada.Unchecked_Deallocation (Integer, PAPI_Event);

   procedure PAPI_Start is
   begin
     Event := new Integer;
     PAPI_Setup (Event);
     From := Clock;
   end PAPI_Start;

   procedure PAPI_Finish is
   begin
     PAPI_Result (Event);
     To := Clock;
     Put_Line (Duration'Image (To - From) & " seconds");
     Free_Event (Event);
   end PAPI_Finish;
end PAPI_Binder;
