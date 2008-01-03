(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Mar 17 14:13:14 PST 1995 by heydon                   *)
(*      modified on Sun Jun  5 15:47:38 PDT 1994 by gnelson                  *)

(* An "RTVal.T" represents a "JunoValue.T".  "RTVals" are used in the Juno
   machine while it is running.  The procedure "Dispose" reclaims the space
   for all RTVals. *)
   
INTERFACE RTVal;

IMPORT JunoValue;

TYPE T = ROOT BRANDED "RTVal" OBJECT END;

TYPE Real = JunoValue.Real;

TYPE 
  Null = T BRANDED "Null" OBJECT END;
  Number <: NumberPublic; NumberPublic = T OBJECT val: Real END;
  Text <: TextPublic; TextPublic = T OBJECT val: TEXT END;
  Pair <: PairPublic; PairPublic = T OBJECT car, cdr: T END;

(* Invariant: The "val" field of a "Text" and the "car" and "cdr" fields of a
   "Pair" are required to be non-NIL. For Juno NIL, use the global variable
   "nil": *)
  
VAR (* READONLY *) nil: Null;

PROCEDURE FromReal(r: Real): Number;
(* Returns a new "Number" with value "r". *)

PROCEDURE FromInt(i: INTEGER): Number;
(* Returns a new "Number" with value "FLOAT(i)". *)

PROCEDURE FromText(txt: TEXT): Text;
(* Returns a new "Text" with value "txt". Requires "txt # NIL". *)

PROCEDURE FromPair(car, cdr: T): Pair;
(* Returns a new "Pair" with elements "car" and "cdr". Requires both "car" and
   "cdr" to be non-NIL. *)

PROCEDURE FromJV(jv: JunoValue.T): T;
(* Returns the run-time value equivalent to "jv". If "jv = NIL", returns
   "NIL". If "jv" has type "Null", returns "JunoValue.Nil". *)

PROCEDURE ToJV(v: T): JunoValue.T;
(* Returns the "JunoValue.T" equivalent to the run-time value "v". If "v =
   NIL", returns "NIL". If "v" has type "JunoValue.Null", returns "nil". *)

PROCEDURE Equal(v, w: T): BOOLEAN;
(* Return TRUE iff v and w are equal. *)

PROCEDURE Mark();
(* Mark all "T"'s allocated prior to this call so they will not be collected
   on the next call to "Dispose". *)

PROCEDURE Dispose();
(* Invalidate and reclaim all "T"s allocated since the last call to "Mark",
   and undo the effect of that "Mark". If there is no previous "Mark",
   invalidate and reclaim all "T"'s allocated since the last call to
   "Dispose". This never invalidates "nil". *)

END RTVal.
