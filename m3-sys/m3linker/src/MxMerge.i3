(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxMerge.i3                                            *)
(* Last Modified On Tue Mar 23 09:26:46 PST 1993 By kalsow     *)

INTERFACE MxMerge;

IMPORT Wr, Mx;

(*------------------------------------------------------------------------*)

PROCEDURE MergeUnit (unit   : Mx.Unit;
                     base   : Mx.LinkSet;
                     errors : Wr.T): Mx.UnitList;
(* check that the version stamps in 'unit' and the version stamps
   of the units in 'base' are consistent.  If they are, 'base'
   is updated to include 'unit' and NIL is returned, otherwise 'base' is
   left unmodified and the list of inconsistent units is returned.
   If there are inconsistencies, error messages are written on 'errors'.
   If 'errors' is NIL, the error messages are silently dropped.  It is
   an unchecked runtime error to modify any of the units or their referents
   once they've been merged with 'base'. *)

END MxMerge.
