(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Tue Sep 21 16:16:43 PDT 1993 by kalsow    *)
(*      modified on Fri May  7 14:47:56 PDT 1993 by mjordan   *)
(*      modified on Fri Mar 16 12:16:52 1990 by muller        *)

UNSAFE MODULE Usignal;

IMPORT Word, Ctypes;

PROCEDURE sigmask (n: Ctypes.int): Ctypes.int =
BEGIN
  RETURN Word.Shift (1, n-1);
END sigmask;

BEGIN
  BADSIG  := LOOPHOLE (-1, SignalHandler);
  SIG_ERR := LOOPHOLE (-1, SignalHandler);
  SIG_DFL := LOOPHOLE ( 0, SignalHandler);
  SIG_IGN := LOOPHOLE ( 1, SignalHandler);
END Usignal.
