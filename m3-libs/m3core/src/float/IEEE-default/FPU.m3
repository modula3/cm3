(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jun 21 15:43:33 PDT 1993 by mcjones                  *)
(*      modified on Fri May  7 22:52:59 PDT 1993 by mjordan                  *)
(*      modified on Fri May  7 14:47:07 PDT 1993 by muller                   *)

MODULE FPU;

(* Formerly, we "implemented" scaleb with
PROCEDURE scalb (x: LONGREAL; n: INTEGER): LONGREAL =
  BEGIN
    <*ASSERT FALSE*>
   END scalb;

Now we're equating it to the externally-defined (ANSI C math library)
procedure ldexp--see FPU.i3. *)

BEGIN
END FPU.
