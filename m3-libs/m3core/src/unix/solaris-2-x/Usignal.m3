(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking   *)
(*      modified on Fri Mar 16 12:16:52 1990 by muller        *)

UNSAFE MODULE Usignal;

BEGIN
  SIG_ERR := LOOPHOLE (-1, SignalHandler);
  SIG_DFL := LOOPHOLE ( 0, SignalHandler);
  SIG_IGN := LOOPHOLE ( 1, SignalHandler);
  SIG_HOLD:= LOOPHOLE ( 2, SignalHandler);
END Usignal.
