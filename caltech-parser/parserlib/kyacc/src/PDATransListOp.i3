(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PDATransListOp.i3,v 1.2 2001-09-19 15:13:58 wagner Exp $ *)

INTERFACE PDATransListOp;
(* first level of table compression *)

IMPORT PDATransList;
TYPE
  T = PDATransList.T;

PROCEDURE Simplify(a: T): T;
(* identify default reduction *)
(* to achieve fewer states, default reduction will be invoked regardless
   of whether there is an error, because error will be detected after
   reductions are complete. *)

PROCEDURE MergeStates(VAR a: REF ARRAY OF T);
(* collapse equivalent states to the same state number (index),
   and introduce anonymous shifting. Then delete unused states. *)

PROCEDURE PrintArray(a: REF ARRAY OF T; len: INTEGER := 0);

END PDATransListOp.
