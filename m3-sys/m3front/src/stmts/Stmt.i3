(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Stmt.i3                                               *)
(* Last Modified On Thu Feb 23 15:22:19 PST 1995 By kalsow     *)
(*      Modified On Tue Jan 16 06:44:23 1990 By muller         *)

INTERFACE Stmt;

IMPORT M3;

TYPE
  T = M3.Stmt;
  CheckState = M3.CheckState;

TYPE
  Outcome = {FallThrough, Exits, Returns};
  Outcomes = SET OF Outcome;

PROCEDURE Parse (): T;

PROCEDURE TypeCheck (t: T;  VAR cs: CheckState);

PROCEDURE Compile (t: T): Outcomes;

PROCEDURE GetOutcome (t: T): Outcomes;

END Stmt.




