(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PDATransListFlat.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE PDATransListFlat;
(* second level of table compression *)

IMPORT PDATransList;
IMPORT RuleList;
TYPE
  T = REF ARRAY OF PDATransList.T;

PROCEDURE Flatten(VAR a: T);
(* merge transition tails, and make every state have two PDATrans.T's:
   "head" (transition) and "next" (PDATrans.ActKind.Jump) *)

PROCEDURE Format(a: T; aCode, saCode, defSymCode: INTEGER): TEXT;
(* format states table as modula-3 array constant *)

PROCEDURE UnReducedWarning(a: T; rules: RuleList.T);

END PDATransListFlat.
