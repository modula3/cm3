(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Mon Oct 12 15:51:28 PDT 1992 by muller        *)

UNSAFE MODULE Usignal;

IMPORT Word, Ctypes;

PROCEDURE sigmask (n: Ctypes.int): Ctypes.int =
BEGIN
  RETURN Word.Shift (1, n-1);
END sigmask;

BEGIN
  BADSIG   := LOOPHOLE (-1, SignalActionHandler);
  SIG_ERR  := LOOPHOLE (-1, SignalActionHandler);
  SIG_DFL  := LOOPHOLE ( 0, SignalActionHandler);
  SIG_IGN  := LOOPHOLE ( 1, SignalActionHandler);
  SIG_HOLD := LOOPHOLE ( 3, SignalActionHandler);
END Usignal.
