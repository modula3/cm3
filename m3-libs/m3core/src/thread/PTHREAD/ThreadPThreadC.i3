(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

(*---------------------------------------------------------------------------*)

<*EXTERNAL*> UNSAFE INTERFACE ThreadPThreadC;

FROM Usem IMPORT sem_t;
FROM Usignal IMPORT sigset_t;
FROM Ctypes IMPORT int;

(*---------------------------------------------------------------------------*)

TYPE
  SignalActionHandler = PROCEDURE (sig: int; sip: ADDRESS (*siginfo_t_star*); uap: ADDRESS (*Uucontext.ucontext_t_star*));

(*---------------------------------------------------------------------------*)

<*EXTERNAL "ThreadPThreadC_mask"*> VAR mask : sigset_t;
<*EXTERNAL "ThreadPThreadC_ackSem"*> VAR ackSem : sem_t;

<*EXTERNAL "ThreadPThreadC_SetupHandlers"*> PROCEDURE SetupHandlers(SignalHandler : SignalActionHandler; sig : int);

(*---------------------------------------------------------------------------*)

END ThreadPThreadC.
