with Hash_Table;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with CPU_Affinity; use CPU_Affinity;
with PAPI_Binder; use PAPI_Binder;
with CMD_Args; use CMD_Args;
--with Ada.Calendar; use Ada.Calendar;

procedure Test_HT is
   type Key_t is mod 2**32;
   subtype Value_t is Character;
   package HTable is new Hash_Table (Key_t, Value_t); 

   subtype fn is Key_t;
   package Random_fn is new Ada.Numerics.Discrete_Random (fn);
   use Random_fn;

   Table_Size : Long_Integer;
   h : HTable.Hashmap;
   type Work_Type is (Insert, Lookup);
   task type Worker is
      entry Start (T_ID : Integer; T_Job : Work_Type; Num_Task : Integer);
      entry Finish;
   end Worker;
   task body Worker is
      Key : Key_t; Value : Value_t; Is_Found : Boolean;
      ID : Integer;
      Quota : Long_Integer;
      Job : Work_Type;
      G : Generator;
--      From, To : Time;
   begin
--      From := Clock;
      accept Start (T_ID : Integer; T_Job : Work_Type; Num_Task : Integer) do
         ID := T_ID;
         Job := T_Job;
         Reset (G, ID);
         Quota := Num_Op / Long_Integer (Num_Task);
         if ID = Num_Task then
            Quota := Quota + Num_Op mod Long_Integer (Num_Task);
         end if;
      end Start;
--      To := Clock;
--      put_Line ("starting " & Duration'image(To-From));

--      From := Clock;
      if Job = Insert then
         for i in 1 .. Quota loop
            Key := Random (G);
            h.Insert (Key, Value_T'Val (80));
         end loop;
      else
         for i in 1 .. Quota loop
            Key := Random (G);
            Is_Found := h.Lookup (Key, Value);
         end loop;
      end if;
--      To := Clock;
--      put_Line ("working " & Duration'image(To-From));

--      From := Clock;
      accept Finish do
         null;
      end Finish;
--      To := Clock;
--      put_Line ("finishning " & Duration'image(To-From));
   end Worker;

begin
   if not Read_Args (HT) then
      return;
   end if;

   Table_Size := Num_Op * 2;
   h.Init (Table_Size);
   for N_Task in Min_Task .. Max_Task loop
      Put_Line (Integer'image (N_Task) & " tasks.");
      for Job in Work_Type loop
         Put_Line (Work_Type'image (Job));

         PAPI_Start;
         declare
            Workers : array(1 .. N_Task) of Worker;
         begin
            for Id in 1 .. N_Task loop
               Workers (Id).Start (Id, Job, N_Task);
               Set_Affinity (Id, Workers (Id)'Identity);  -- No effect if -a option is not given
            end loop;
            for Id in 1 .. N_Task loop
               Workers (Id).Finish;
            end loop;
         end;
         PAPI_Finish;
      end loop;
      --h.print_count;
      h.clean;
   end loop;
   h.Destroy;
end Test_HT;
