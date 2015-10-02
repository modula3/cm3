MODULE Main;

(*
   Simple test of SchedulerPThread functions. Not intended to be demonstrate
   correct operation of mutex locking and priority inversion.
   To check real time priorities working run top -H
   
   Warning you may need to run this program as root.
   Check /etc/security/limits.conf has an rtprio entry for normal users.

   Also
   
  Check these sysctl parameters. 
  Here are mine from an Ubuntu box. The default is to give real time only 0.95 seconds out of a 1 second slice:

  /proc/sys/kernel/sched_rt_period_us = 1000000
  /proc/sys/kernel/sched_rt_runtime_us = 950000
  This prevents the real-time domain from taking all the CPU. If you want real real time, you have to disable these parameters.

  See: http://www.kernel.org/doc/Documentation/scheduler/sched-rt-group.txt

  If you set sched_rt_runtime_us to -1, you disable this safety mechanism.
  A runaway high priority thread can render your machine non responsive 
  to which powering off may be the only solution.

  Multi core machines provide abundant resources for simple tests. To reduce
  a machine to single cpu for resource competition tests you can
  run as root (at least on linux) 
  echo 0 > /sys/devices/system/cpu/cpu1/online 
  to disable core 1. echo 1 to renable. See enablecores and disablecores
  scripts.
  Obviously you cannot disable all cores. Run top to see which cores are
  online.
*)

IMPORT Thread,IO,Random,RTProcess;
IMPORT SchedulerPThread AS SP;

CONST 

  InitPause = 1.0d0;
  DelayPause = 0.2d0;
  EPERM = 1;
(*  EINVAL = 22; *)
  
TYPE 
  T = Thread.Closure OBJECT 
        id : CARDINAL 
       END;

VAR
  mu : MUTEX;
  mu_ceil : MUTEX;
  cond : Thread.Condition;

  lowThread,medThread,highThread,otherThread : Thread.T;
  lowTask,medTask,highTask,otherTask : T;
  
  policy : SP.PolicyT;
  priority,maxPrio,minPrio : SP.PriorityT;

  afinSet := SP.CpuSetT{};
  
  confCores,onLineCores : CARDINAL;
  
<*FATAL SP.SchedE *>
  
PROCEDURE Work(d : INTEGER) =
VAR j,k : INTEGER := 0;
    r,s : LONGREAL;
BEGIN
  FOR i := 0 TO d * 50 * 1000000 DO 
    j := j + i;
    k := j * i;
    r := FLOAT(k,LONGREAL);
    s := FLOAT(j,LONGREAL);
    s := r / s;
  END;
END Work;

PROCEDURE AllocSome() =
VAR  rand : Random.T; size : INTEGER;
BEGIN
  rand := NEW(Random.Default).init();
  FOR k := 0 TO 100 DO
    size := rand.integer(10,10000);
    VAR
      arr := NEW(REF ARRAY OF INTEGER, size);
    BEGIN
      FOR i := FIRST(arr^)+1 TO LAST(arr^) DO
        arr[i] := arr[i] - arr[i-1]
      END;
    END;
  END;
END AllocSome;

PROCEDURE LowTask(cl : T) : REFANY =
BEGIN

  IO.Put("thread: "); IO.PutInt(cl.id); IO.Put(" starting\n");
  Thread.Pause(InitPause);
      
  LOOP
    IO.Put("low thread before pause: "); IO.PutInt(cl.id); IO.Put("\n");
    Thread.Pause(DelayPause);
  
    AllocSome();
    
    IO.Put("low thread before lock mu: "); IO.PutInt(cl.id); IO.Put("\n"); 
    LOCK mu DO 
      FOR j := 1 TO 6 DO
        IO.Put("low thread: "); IO.PutInt(cl.id); IO.Put(" before work\n");    
        Work(1);  
        IO.Put("low thread: "); IO.PutInt(cl.id); IO.Put(" after work\n");     
      END;
    END;
    IO.Put("low thread after lock mu: "); IO.PutInt(cl.id); IO.Put("\n");
    LOCK mu DO
      Thread.Wait(mu,cond);
    END;
    TestPrioCeiling();
  END
END LowTask;

PROCEDURE MedTask(cl : T) : REFANY =
BEGIN

  IO.Put("thread: "); IO.PutInt(cl.id); IO.Put(" starting\n");
  Thread.Pause(InitPause);
    
  LOOP
    Thread.Pause(DelayPause);
    IO.Put("med thread: "); IO.PutInt(cl.id); IO.Put(" Before work\n");     
    Work(5);
    Thread.Signal(cond);
    IO.Put("med thread: "); IO.PutInt(cl.id); IO.Put(" After work\n");     
  END
END MedTask;

PROCEDURE HighTask(cl : T) : REFANY =
BEGIN

  IO.Put("thread: "); IO.PutInt(cl.id); IO.Put(" starting\n");
  Thread.Pause(InitPause);
  
  LOOP
    IO.Put("high thread before pause: "); IO.PutInt(cl.id); IO.Put("\n");
    Thread.Pause(DelayPause);
    IO.Put("high thread: "); IO.PutInt(cl.id); IO.Put(" locking mu\n");     
    LOCK mu DO     
      IO.Put("high thread: "); IO.PutInt(cl.id); IO.Put(" working\n");      
      Work(1);
      IO.Put("high thread: "); IO.PutInt(cl.id); IO.Put(" after work\n");      
    END;
    IO.Put("high thread: "); IO.PutInt(cl.id); IO.Put(" after locking mu\n"); 
(*
    TRY
      Thread.AlertPause(DelayPause);    
    EXCEPT
    | Thread.Alerted =>
      IO.Put("Alerted\n");
    END;
*)
  END
END HighTask;

PROCEDURE OtherTask(cl : T) : REFANY =
BEGIN

  IO.Put("thread: "); IO.PutInt(cl.id); IO.Put(" starting\n");
  Thread.Pause(InitPause);
      
  LOOP
  
    IO.Put("other thread before pause: "); IO.PutInt(cl.id); IO.Put("\n");
    Thread.Pause(DelayPause);
  
    AllocSome();
    IO.Put("other thread before lock mu: "); IO.PutInt(cl.id); IO.Put("\n"); 
    LOCK mu DO 
      FOR j := 1 TO 6 DO
        IO.Put("other thread: "); IO.PutInt(cl.id); IO.Put(" before pause\n"); 
        Thread.Pause(DelayPause);
        IO.Put("other thread: "); IO.PutInt(cl.id); IO.Put(" after pause\n");  
      END;
    END;
    
    IO.Put("other thread after lock mu: "); IO.PutInt(cl.id); IO.Put("\n");    
    
(* cannot lock priority ceiling mutex in non real time thread
    LOCK mu_ceil DO
      IO.Put("thread: "); IO.PutInt(cl.id); IO.Put(" has locked mu_ceil\n");   
    END;
*)
  END
END OtherTask;

PROCEDURE ShowAfin(affinity : SP.CpuSetT ) =
VAR empty := TRUE;
BEGIN
  IO.Put("Affin Set ");
  FOR i := 0 TO confCores - 1 DO
    IF i IN affinity THEN
      IO.PutInt(i); IO.Put(" "); empty := FALSE;
    END;
  END;
  IF empty THEN IO.Put("empty"); END;
  IO.Put("\n");
END ShowAfin;

PROCEDURE TestAffinity() =
VAR
  afinCore1,afinCore2 : CARDINAL := 0;
BEGIN
  (* test setting a thread to run on a particular core or set of cores 
     assumes at least 2 cores *)
  
  IO.Put("Test Affinity \n ");
  
  IF onLineCores < 2 THEN
    IO.Put("Need at least 2 cores for this affinity test\n");
    RETURN;
  END;

  (* just show the initial value of the set *)
  IO.Put("Sched affinity initial value of cpu set. Will be empty\n");
  ShowAfin(afinSet);
  
  (* get the initial set of cores the low thread can run on. Should be
     all cores unless set offline *)
  FOR i := 0 TO confCores - 1 DO
    afinSet := afinSet + SP.CpuSetT{i};
  END;
  SP.SetAffinity(lowThread,afinSet);
  ShowAfin(afinSet);
  
  afinSet := SP.GetAffinity(lowThread);

  IO.Put("Sched affinity for low thread initially all online cores\n");
  ShowAfin(afinSet);
  
  FOR i := confCores - 1 TO 0 BY -1 DO
    IF i IN afinSet THEN
      afinCore1 := i;
      EXIT;
    END;
  END;
  
  FOR i := afinCore1 - 1 TO 0 BY -1 DO
    IF i IN afinSet THEN
      afinCore2 := i;
      EXIT;
    END;
  END;
  
  afinSet := SP.CpuSetT{afinCore1,afinCore2};
  
  IO.Put("Sched set affinity for low thread to last 2 cores only\n");  
  (* set low thread to run on last 2 cores only *)
  SP.SetAffinity(lowThread,afinSet);
  
  (* read it back to check *)
  afinSet := SP.GetAffinity(lowThread);
  
  IO.Put("Sched get affinity. Low thread is on these cores only\n");    
  ShowAfin(afinSet); 
  
END TestAffinity;

PROCEDURE TestPrioCeiling() =
VAR
  oldCeiling : SP.PriorityT;
BEGIN

  IO.Put("Test PrioCeiling \n ");
  SP.GetSchedParams(lowThread, policy, priority);
  
  IO.Put("Sched get policy and priority "); IO.PutInt(ORD(policy)); IO.Put(" ");
  IO.PutInt(priority); IO.Put("\n");
  
  TRY
    oldCeiling := SP.GetPriorityCeiling(mu_ceil);
    IO.Put("Sched prio old ceiling "); IO.PutInt(oldCeiling); IO.Put("\n");    
  EXCEPT
  | SP.SchedE(x) => 
    IO.Put("Error: Sched getprio ceiling "); IO.PutInt(x); IO.Put("\n");
  END;

  TRY
    (* set the thread ceiling to at least as high as the highest priority 
       thread that can lock this mutex *)
    SP.SetPriorityCeiling(mu_ceil,80,oldCeiling);

    IO.Put("Sched set prio ceiling  to 80"); IO.Put("\n");
    IO.Put("Sched prio old ceiling "); IO.PutInt(oldCeiling); IO.Put("\n");     
  EXCEPT
  | SP.SchedE(x) => 
    IO.Put("Error: Sched setprio ceiling "); IO.PutInt(x); IO.Put("\n");
  END;

  LOCK mu_ceil DO
  END;
    
  TRY
    oldCeiling := SP.GetPriorityCeiling(mu_ceil);
    IO.Put("Sched prio new ceiling "); IO.PutInt(oldCeiling); IO.Put("\n");    
  EXCEPT
  | SP.SchedE(x) => 
    IO.Put("Error: Sched getprio ceiling "); IO.PutInt(x); IO.Put("\n");
  END;

END TestPrioCeiling;

BEGIN
  confCores := SP.GetConfiguredCores();
  IO.Put("Configured Cores "); IO.PutInt(confCores); IO.Put("\n");

  onLineCores := SP.GetOnlineCores();
  IO.Put("Online Cores "); IO.PutInt(onLineCores); IO.Put("\n");

  maxPrio := SP.GetSchedMaxPriority(SP.PolicyT.SCHED_FIFO);
  IO.Put("Max Priority "); IO.PutInt(ORD(maxPrio)); IO.Put("\n");

  minPrio := SP.GetSchedMinPriority(SP.PolicyT.SCHED_FIFO);
  IO.Put("Min Priority "); IO.PutInt(ORD(minPrio)); IO.Put("\n");
  
  (* create a mutex with priority inherit protocol *)
  mu := NEW(SP.MutexRT,protocol := SP.MutexProtocolT.PTHREAD_PRIO_INHERIT);
  
  (* creates the pthread mutex in the lock stmt *)
  LOCK mu DO
  END;
  
  (* create a mutex with priority ceiling protocol *)
  mu_ceil := NEW(SP.MutexRT, protocol := SP.MutexProtocolT.PTHREAD_PRIO_PROTECT);  
  
  lowTask :=  NEW (T, apply := LowTask, id := 1);
  lowThread := Thread.Fork(lowTask); 
  medTask :=  NEW (T, apply := MedTask, id := 2);
  medThread := Thread.Fork(medTask);
  highTask :=  NEW (T, apply := HighTask, id := 3);
  highThread := Thread.Fork(highTask);

  otherTask :=  NEW (T, apply := OtherTask, id := 4);
  otherThread := Thread.Fork(otherTask);
  
  cond := NEW(Thread.Condition);
  
  (* wait for threads to initialise *)
  Thread.Pause(1.5D0);
 
  (* test setting a threads affinity *)
  TestAffinity();  
   
  (*  these will not work
  SP.SetSchedParams(lowThread,  SP.PolicyT.SCHED_RR, 0); (* rt prio > 0 *) 
  SP.SetSchedParams(lowThread,  SP.PolicyT.SCHED_OTHER, 20); (* other prio = 0 *)
  SP.SetSchedParams(lowThread,  SP.PolicyT.SCHED_BATCH, 20); (* batch prio = 0 *)
  SP.SetSchedParams(lowThread,  SP.PolicyT.SCHED_IDLE, 20); (* idle prio = 0 *) 
  *)

  (* Initial setting to real time policy to check permission of user *)
  TRY  
    SP.SetSchedParams(lowThread, SP.PolicyT.SCHED_FIFO, 1);
  EXCEPT
  | SP.SchedE(x) => 
      IF x = EPERM THEN 
        IO.Put("Sched permissions exception!! Are you root?? \n");
      ELSE
        IO.Put("Unknown Sched exception "); IO.PutInt(x);  IO.Put("\n");
      END;
      RTProcess.Exit(0);      
  END;
  
  (* set the policy and priority of our 3 real time threads and one other *)
  TRY
    SP.SetSchedParams(lowThread, SP.PolicyT.SCHED_RR, 20);
    SP.SetSchedParams(medThread, SP.PolicyT.SCHED_FIFO, 30);
    SP.SetSchedParams(highThread, SP.PolicyT.SCHED_FIFO, 40);  
    SP.SetSchedParams(otherThread, SP.PolicyT.SCHED_OTHER, 0);
  EXCEPT
  | SP.SchedE(x) => 
      IO.Put("Sched exception "); IO.PutInt(x);  IO.Put("\n");
      RTProcess.Exit(0);      
  END;
  
  Thread.Pause(0.5D0);
  
  IO.Put("All threads priority set!!\n");
  
  (* lower the low threads priority *)
  SP.SetSchedPriority(lowThread,10);
  
  (* and check its set *)
  SP.GetSchedParams(lowThread, policy, priority);
  IO.Put("Sched get policy and priority "); IO.PutInt(ORD(policy)); IO.Put(" ");
  IO.PutInt(priority); IO.Put("\n");

(*
  IO.Put("Alerting now!!\n");
  Thread.Alert(highThread);
*)
  
  IO.Put("Main thread waiting 20!!\n");
  Thread.Pause(20.0D0);
  
END Main.
