(* Copyright (C) 1991, Digital Equipment Corporation       *)
(* All rights reserved.                                    *)
(* See the file COPYRIGHT for a full description.          *)
(*                                                         *)
(* Last modified on Fri Jan 14 10:15:43 PST 1994 by kalsow *)
(*      modified on Mon Apr 06 14:13:15 PDT 1992 by muller *)

UNSAFE MODULE TclC;

IMPORT Cstdlib;

PROCEDURE FreeResult (interp: Interp_star) = 
  BEGIN
    IF interp.freeProc # NIL THEN
      interp.freeProc (interp.result);
      interp.freeProc := NIL;
    END;
  END FreeResult;

BEGIN
  volatile := LOOPHOLE (-1, FreeProc);
  static   := LOOPHOLE (0, FreeProc);
  dynamic  := LOOPHOLE (Cstdlib.free, FreeProc);
END TclC.
