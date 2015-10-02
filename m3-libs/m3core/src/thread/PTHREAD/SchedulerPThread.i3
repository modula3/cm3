INTERFACE SchedulerPThread;

  (* This interface allows threads to change their scheduling policy in particular
     to the real time policies fifo and rr (round robin). And thus to alter their
     priorities. This can be dangerous and programs using the real time policies may need to be running as root. A normal user can run such programs if they have an entry in /etc/security/limits.conf for the rtprio setting. *)
     
IMPORT Thread;

CONST

  (* Arbitrary value for max number of cores. *)
  MAX_CPU = 64;
  
  (* POSIX.1-2001 requires implementations support a minimum of 32 distinct
     priorities for real time policies. Use GetSchedMaxPriority to get the
     system max for your system *)
  MAX_PRIORITY = 99;
  
TYPE

  (* Scheduling algorithms. SCHED_OTHER is the normal time share policy. 
     SCHED_FIFO and SCHED_RR are the real time policies. SCHED_BATCH is for batch
     style execution and SCHED_IDLE is for very low priority background execution *)
     
  PolicyT = {SCHED_OTHER, SCHED_FIFO, SCHED_RR, XXX_Unused, SCHED_BATCH, SCHED_IDLE};

  (* Realtime priorities are 1 - 99 (although could be restricted by implementations) and only applicable to SCHED_FIFO and SCHED_RR. Other policies use priority 0 *)

  PriorityT = [0 .. MAX_PRIORITY];

  (* Set type for affinity support. Really need a dynamic set type with max
     elements of online cores. *)
     
  CpuSetT = SET OF [0..MAX_CPU-1];
  
  (* Policy inherits - not supported as yet. *)
 
  PolicyInheritT = {PTHREAD_INHERIT_SCHED, PTHREAD_EXPLICIT_SCHED}; 
 
  (* Mutex protocol support. *)
  
  MutexProtocolT =  {PTHREAD_PRIO_NONE, PTHREAD_PRIO_INHERIT, PTHREAD_PRIO_PROTECT};
 
  (* real time mutex for priority inversion support. *)
  
  MutexRT = Thread.Mutex OBJECT
    protocol : MutexProtocolT :=  MutexProtocolT.PTHREAD_PRIO_NONE;    
  END;
  
 (* In general the exception parm is the pthread function return code *)
EXCEPTION SchedE(INTEGER);

  
(* Scheduler Policy and Priority. *)

PROCEDURE SetSchedParams(thread : Thread.T; policy : PolicyT; priority : PriorityT) RAISES {SchedE};
PROCEDURE GetSchedParams(thread : Thread.T; VAR policy : PolicyT; VAR priority : PriorityT)  RAISES {SchedE};

(* Reset a thread priority. *)

PROCEDURE SetSchedPriority(thread : Thread.T; priority : PriorityT) RAISES {SchedE};

(* Mutex priority ceiling support. *)

PROCEDURE SetPriorityCeiling(mu : MUTEX; prioCeiling : PriorityT; VAR oldCeiling : PriorityT) RAISES {SchedE};
PROCEDURE GetPriorityCeiling(mu : MUTEX) : PriorityT RAISES {SchedE};

(* Affinity support. Allows a thread to be restricted to a particular core(s). *)

PROCEDURE SetAffinity(thread : Thread.T; cpuSet : CpuSetT) RAISES {SchedE};
PROCEDURE GetAffinity(thread : Thread.T) : CpuSetT RAISES {SchedE};

(* Number of configured cores. (ie the total the os knows about *)

PROCEDURE GetConfiguredCores() : CARDINAL;

(* Number of processor cores online. *)

PROCEDURE GetOnlineCores() : CARDINAL;

(* Get the current max and min priorities for the policy - could be less than
   MAX_PRIORITY above. *)

PROCEDURE GetSchedMaxPriority(policy : PolicyT) : PriorityT;
PROCEDURE GetSchedMinPriority(policy : PolicyT) : PriorityT;

END SchedulerPThread.
