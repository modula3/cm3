(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

INTERFACE Csignal;

FROM Ctypes IMPORT int;

TYPE
  Handler = PROCEDURE (s: int);

<*EXTERNAL*>
PROCEDURE signal (sig: int; func: Handler): Handler;

END Csignal.
