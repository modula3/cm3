(* Copyright (C) 1990, Digital Equipment Corporation.                 *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)

<*EXTERNAL*> INTERFACE Usignal;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT pid_t;
IMPORT Usysdep;

CONST
  SIGINT = Usysdep.SIGINT;
  SIGKILL = Usysdep.SIGKILL;

TYPE
  SignalActionHandler = PROCEDURE (sig: int; sip: siginfo_t_star; uap: ADDRESS (* Uucontext.ucontext_t_star *) );
  siginfo_t_star = ADDRESS;

PROCEDURE kill (pid: pid_t; sig: int): int;

END Usignal.
