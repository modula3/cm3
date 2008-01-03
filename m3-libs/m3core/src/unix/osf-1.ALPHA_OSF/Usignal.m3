(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Apr  9 10:00:26 PDT 1993 by muller        *)

UNSAFE MODULE Usignal;

BEGIN
  BADSIG    := LOOPHOLE (-1, SignalHandler);
  SIG_ERR   := LOOPHOLE (-1, SignalHandler);
  SIG_DFL   := LOOPHOLE ( 0, SignalHandler);
  SIG_IGN   := LOOPHOLE ( 1, SignalHandler);
  SIG_HOLD  := LOOPHOLE ( 2, SignalHandler);
  SIG_CATCH := LOOPHOLE ( 3, SignalHandler);
END Usignal.
