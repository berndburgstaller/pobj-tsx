#include <stdio.h>
#include <string.h>
#include <papi.h>

void PAPI_setup(int *eventSet)
{
   int retval = PAPI_library_init(PAPI_VER_CURRENT);
   int codes[3];
   PAPI_option_t opt;

   *eventSet = PAPI_NULL;
   if(retval != PAPI_VER_CURRENT){
      printf("PAPI library init error!\n");
      return;
   }
   
   if(PAPI_create_eventset(eventSet) != PAPI_OK){
      printf("PAPI library init error!\n");
      return;
   }

/**********To Inherit*****************/
   if(PAPI_assign_eventset_component(*eventSet, 0) != PAPI_OK){
      printf("PAPI_assign_eventset_component error!\n");
      return;
   }
   memset(&opt, 0x0, sizeof (PAPI_option_t));
   opt.inherit.inherit = PAPI_INHERIT_ALL;
   opt.inherit.eventset = *eventSet;
   if(PAPI_set_opt(PAPI_INHERIT, &opt) != PAPI_OK){
      printf("PAPI_set_opt error\n");
      return;
   }
/*************************************/

   if(PAPI_event_name_to_code("CPU_CLK_UNHALTED", &codes[0]) != PAPI_OK){
      printf("PAPI coding error0!\n");
      return;
   }
   if(PAPI_event_name_to_code("CPU_CLK_UNHALTED:intx=1", &codes[1]) != PAPI_OK){
      printf("PAPI coding error1!\n");
      return;
   }
   if(PAPI_event_name_to_code("CPU_CLK_UNHALTED:intx=1:intxcp=1", &codes[2]) != PAPI_OK){
      printf("PAPI coding error2!\n");
      return;
   }
   if(PAPI_add_events(*eventSet, codes, 3) != PAPI_OK){
      printf("PAPI add_events error!\n");
      return;
   }
   if(PAPI_start(*eventSet) != PAPI_OK){
      printf("PAPI start error!\n");
      return;
   }
}

void PAPI_result(int *eventSet){
   long long values[3];

   if(PAPI_read(*eventSet, values) != PAPI_OK){
      printf("PAPI read error!\n");
      return;
   }
   if(PAPI_stop(*eventSet, values) != PAPI_OK){
      printf("PAPI stop error!\n");
      return;
   }

   printf("total:  %llu\n", values[0]);
   printf("intx:   %llu\n", values[1]);
   printf("intxcp: %llu\n", values[2]);
   printf("succ%%:  %lf\n", (double)values[2]*100/values[1]);
}



/*


With Ada.Text_IO;   Use Ada.Text_IO;

procedure test_inst is
   type EventHandler is access Integer;
   procedure PAPI_setup(eventset : in EventHandler);
   pragma Import(C, PAPI_setup, "PAPI_setup");

   procedure PAPI_result(eventset : in EventHandler);
   pragma Import(C, PAPI_result, "PAPI_result");

   eventset : EventHandler := new Integer;
begin
   PAPI_setup(eventset);
   null;
   PAPI_result(eventset);
end test_inst;

*/
