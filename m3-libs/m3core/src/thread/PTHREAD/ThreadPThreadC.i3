(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

(*---------------------------------------------------------------------------*)

<*EXTERNAL*> UNSAFE INTERFACE ThreadPThreadC;

FROM Ctypes IMPORT int;

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

END ThreadPThreadC.
