(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the files COPYRIGHT and COPYRIGHT.extras for details.   *)
(*                                                             *)
(* Last Modified On Tue Nov  1 10:14:23 PST 1994 By kalsow     *)
(* Derived from Solve.i3 by kalsow                             *)

INTERFACE Solve2;

IMPORT Solve;

(*
 * depth, breadth, total are measured in moves generated.
 * breadth is how far to search bread-first, depth is far to
 * search depth from each leaf of breadth search, total is limit
 * on entire search.  IF callback # NIL
 * then it will be called periodically with status.
 *)
PROCEDURE NextMove (READONLY layout  : Solve.Layout;
                         VAR why     : Solve.WhyStop;
                             depth   : CARDINAL  := 2000;
                             breadth : CARDINAL  := 500;
                             total   : CARDINAL  := 100000;
                             verbose             := FALSE;
                             callback: Solve.Callback  := NIL     ): TEXT;
END Solve2.
