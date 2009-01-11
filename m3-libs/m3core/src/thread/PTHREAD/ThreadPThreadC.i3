(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

(*---------------------------------------------------------------------------*)

<*EXTERNAL*> UNSAFE INTERFACE ThreadPThreadC;

FROM Usem IMPORT sem_t;
FROM Usignal IMPORT SignalActionHandler, sigset_t;
FROM Ctypes IMPORT int;

(*---------------------------------------------------------------------------*)

<*EXTERNAL "ThreadPThreadC_mask"*> VAR mask : sigset_t;
<*EXTERNAL "ThreadPThreadC_ackSem"*> VAR ackSem : sem_t;

<*EXTERNAL "ThreadPThreadC_SetupHandlers"*> PROCEDURE SetupHandlers(SignalHandler : SignalActionHandler; sig : int);

(*---------------------------------------------------------------------------*)

END ThreadPThreadC.
