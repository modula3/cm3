(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jun 21 15:44:49 PDT 1993 by mcjones                  *)
(*      modified on Fri May  7 14:47:22 PDT 1993 by muller                   *)

INTERFACE FPU;

<*EXTERNAL ldexp*> PROCEDURE scalb (x: LONGREAL; n: INTEGER): LONGREAL;
(* Formerly, the default IEEE implementation of scalb was
  "<*ASSERT FALSE*>". *)

<*EXTERNAL*> PROCEDURE sqrt  (x: LONGREAL): LONGREAL;

END FPU.
