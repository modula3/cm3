(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb 24 14:01:21 PST 1992 by muller                   *)
<*PRAGMA LL*>

(* A "RigidVBT.T" is a filter whose size range is set explicitly,
   independently of its child's size range.  In spite of its name,
   it size range does not have to be fixed to a single value. 
   
   All dimensions in this interface are specified in millimeters. *)

INTERFACE RigidVBT;

IMPORT VBT, Filter, Axis;

TYPE 
  T <: Public;
  Public = Filter.T OBJECT METHODS
    <* LL.sup <= VBT.mu *>
    init(ch: VBT.T; sh: Shape): T
  END;

TYPE 
  SizeRange = RECORD lo, pref, hi: REAL END;
  Shape = ARRAY Axis.T OF SizeRange;

(* The call "v.init(...)" initializes "v" as a rigid "VBT" with 
   child "ch" and shape "sh".

   A "RigidVBT.SizeRange" is like a "VBT.SizeRange", but in millimeters 
   instead of pixels, using "REAL"s instead of "INTEGER"s, and the 
   range is "[lo..hi]" instead of "[lo..hi-1]". *)


PROCEDURE New(ch: VBT.T; sh: Shape): T;
(* "New(...)" is equivalent to "NEW(T).init(...)". *)


PROCEDURE FromHV(
  ch: VBT.T; 
  hMin, vMin: REAL; 
  hMax, vMax, hPref, vPref: REAL := -1.0) : T;  
  <* LL.sup <= VBT.mu *>
(* Return a "RigidVBT" with child "ch" and the given shape. *)

(* If "hMax" or "hPref" are defaulted, they are assumed to be the
   same as "hMin", and similarly for "vMax", "vPref" and "vMin".  
   That is, "FromHV" is equivalent to: 

| IF hMax = -1.0 THEN hMax := hMin END;
| IF vMax = -1.0 THEN vMax := vMin END;
| IF hPref = -1.0 THEN hPref := hMin END;
| IF vPref = -1.0 THEN vPref := vMin END;
| RETURN New(ch, 
|   Shape{SizeRange{h, hMax, hPref},
|         SizeRange{v, vMax, vPref}})

*)

END RigidVBT.

