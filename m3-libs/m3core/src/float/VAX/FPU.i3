(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jul 27 07:45:38 PDT 1994 by kalsow                   *)
(*      modified on Mon Jun 21 15:44:49 PDT 1993 by mcjones                  *)
(*      modified on Fri May  7 14:47:22 PDT 1993 by muller                   *)

INTERFACE FPU;

<*EXTERNAL*> PROCEDURE ldexp (x: LONGREAL; n: INTEGER): LONGREAL;

<*EXTERNAL*> PROCEDURE sqrt  (x: LONGREAL): LONGREAL;

END FPU.
