(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

(*---------------------------------------------------------------------------*)

<*EXTERNAL*> UNSAFE INTERFACE ThreadPThreadC;

FROM Ctypes IMPORT int;
FROM Cstddef IMPORT size_t;
FROM Upthread IMPORT pthread_t, start_routine_t;

(*---------------------------------------------------------------------------*)

TYPE
  SignalActionHandler = PROCEDURE (sig: int; sip: ADDRESS (*siginfo_t_star*); uap: ADDRESS (*Uucontext.ucontext_t_star*));

(*---------------------------------------------------------------------------*)

(*CONST*)
<*EXTERNAL "ThreadPThreadC_SIG_SUSPEND"*>VAR SIG_SUSPEND: int;

(*---------------------------------------------------------------------------*)

<*EXTERNAL "ThreadPThreadC_SetupHandlers"*> PROCEDURE SetupHandlers();

(*---------------------------------------------------------------------------*)

(* the semaphore is implied *)

<*EXTERNAL "ThreadPThreadC_sem_wait"*> PROCEDURE sem_wait (): int;
<*EXTERNAL "ThreadPThreadC_sem_post"*> PROCEDURE sem_post (): int;
<*EXTERNAL "ThreadPThreadC_sem_getvalue"*> PROCEDURE sem_getvalue (VAR value: int): int;

(*---------------------------------------------------------------------------*)

(* the signal set is implied *)

<*EXTERNAL "ThreadPThreadC_sigsuspend"*> PROCEDURE sigsuspend (): int;

(*---------------------------------------------------------------------------*)

(* pthread_create but replace attr with stackSize so that attr need not be known to Modula-3 *)

<*EXTERNAL "ThreadPThreadC_thread_create"*>
PROCEDURE thread_create(VAR pthread: pthread_t; stackSize: size_t; start_routine: start_routine_t; arg: ADDRESS): int;

(*---------------------------------------------------------------------------*)

END ThreadPThreadC.
