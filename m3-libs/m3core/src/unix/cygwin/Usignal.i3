(* Copyright (C) 1990, Digital Equipment Corporation.                 *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)

INTERFACE Usignal;

FROM Ctypes IMPORT int;
IMPORT Usysdep;

CONST
  SIGINT = Usysdep.SIGINT;
  SIGKILL = Usysdep.SIGKILL;

TYPE
  SignalHandler = Usysdep.SignalHandler;
  SignalActionHandler = Usysdep.SignalActionHandler;

<*EXTERNAL*> PROCEDURE kill (pid, sig: int): int;

END Usignal.
