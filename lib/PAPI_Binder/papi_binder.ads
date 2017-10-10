with Ada.Calendar;

package PAPI_Binder is
   procedure PAPI_Start;
   procedure PAPI_Finish;

private
   type PAPI_Event is access Integer;
   Event : PAPI_Event;

   From, To : Ada.Calendar.Time;

   procedure PAPI_Setup (Event_Set : PAPI_Event);
   pragma Import (C, PAPI_setup, "PAPI_setup");
   procedure PAPI_Result (Event_Set : PAPI_Event);
   pragma Import (C, PAPI_result, "PAPI_result");

end PAPI_Binder;
