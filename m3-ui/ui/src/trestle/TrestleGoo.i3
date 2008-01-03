(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Oct 13 23:34:02 PDT 1992 by msm     *)
<*PRAGMA LL*>

(* A mechanism for attaching arbitrary sets of properties to an installed
   window that can be set and retrieved by a TrestleClass.  The interface
   is like VBT's property set, but supports enumeration. *)

INTERFACE TrestleGoo;

IMPORT VBT;

PROCEDURE PutProp(v: VBT.T; ref: REFANY); <* LL.sup < v *>
(* Add "ref" to "v"'s property set, replacing any existing reference of 
   the same type as "ref".  This is a checked runtime error if "ref" is 
   "NIL". *)


PROCEDURE GetProp(v: VBT.T; tc: INTEGER): REFANY;
<* LL.sup < v *>
(* Return the element of "v"'s property set with typecode "tc", or  
   "NIL" if no such element exists. *)


PROCEDURE RemProp(v: VBT.T; tc: INTEGER); <* LL.sup < v *>
(* Remove the element with typecode "tc" from "v"'s property set, if one 
   exists. *)

TYPE Enum <: REFANY;

PROCEDURE Next(v: VBT.T; VAR enum: Enum): REFANY;
(* Start with enum = NIL, continue until NIL is returned *)

PROCEDURE Alias(v, ch: VBT.T);
(* Establish v as a TrestleGoo alias for ch; that is, GetProp, etc. will all
   work on v as if it were ch *)

END TrestleGoo.


