(* This file exists because
 - Modula-3 static initialization does not happen early enough.
 - Cygwin's static initialization of pthreads objects is not all zeros.
*)

INTERFACE ThreadPThread;
IMPORT Upthread;

<*EXTERNAL "ThreadPThread__activeMu"*>
VAR activeMu : Upthread.pthread_mutex_t; (* global lock for list of active threads *)

<*EXTERNAL "ThreadPThread__slotMu"*>
VAR slotMu   : Upthread.pthread_mutex_t; (* global lock for thread slot table *)

<*EXTERNAL "ThreadPThread__initMu"*>
VAR initMu   : Upthread.pthread_mutex_t; (* global lock for initializers *)

<*EXTERNAL "ThreadPThread__perfMu"*>
VAR perfMu   : Upthread.pthread_mutex_t;

<*EXTERNAL "ThreadPThread__lockMu"*>
VAR lockMu   : Upthread.pthread_mutex_t;

<*EXTERNAL "ThreadPThread__lockCond"*>
VAR lockCond : Upthread.pthread_cond_t;

END ThreadPThread.
