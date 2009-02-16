(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

(*---------------------------------------------------------------------------*)

UNSAFE INTERFACE ThreadPThread;

FROM Ctypes IMPORT int;
FROM Cstddef IMPORT size_t;
FROM Upthread IMPORT pthread_t, start_routine_t;

(*---------------------------------------------------------------------------*)

PROCEDURE SignalHandler(sig: int; sip, uap: ADDRESS);

(*---------------------------------------------------------------------------*)

(*CONST*)
<*EXTERNAL "ThreadPThread__SIG_SUSPEND"*>VAR SIG_SUSPEND: int;

(*---------------------------------------------------------------------------*)

<*EXTERNAL "ThreadPThread__SetupHandlers"*> PROCEDURE SetupHandlers();

(*---------------------------------------------------------------------------*)

(* the semaphore is implied *)

<*EXTERNAL "ThreadPThread__sem_wait"*> PROCEDURE sem_wait (): int;
<*EXTERNAL "ThreadPThread__sem_post"*> PROCEDURE sem_post (): int;
<*EXTERNAL "ThreadPThread__sem_getvalue"*> PROCEDURE sem_getvalue (VAR value: int): int;

(*---------------------------------------------------------------------------*)

(* the signal set is implied *)

<*EXTERNAL "ThreadPThread__sigsuspend"*> PROCEDURE sigsuspend (): int;

(*---------------------------------------------------------------------------*)

(* pthread_create but replace attr with stackSize so that attr need not be known to Modula-3 *)

<*EXTERNAL "ThreadPThread__thread_create"*>
PROCEDURE thread_create(VAR pthread: pthread_t; stackSize: size_t; start_routine: start_routine_t; arg: ADDRESS): int;

(*---------------------------------------------------------------------------*)

END ThreadPThread.
