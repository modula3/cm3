(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxCheck.i3                                            *)
(* Last Modified On Tue Mar 23 09:21:01 PST 1993 By kalsow     *)

INTERFACE MxCheck;

IMPORT Wr, Mx;

(*------------------------------------------------------------------------*)

PROCEDURE IsProgram (base: Mx.LinkSet;  errors: Wr.T): BOOLEAN;
(* check whether 'base' defines a complete program (ie. all version stamps
   are defined and consistent, all imported interfaces are defined, 'Main'
   is exported).  If the units of 'base' form a complete program,
   TRUE is returned, otherwise is returned.  If there are inconsistencies,
   error messages are written on 'errors'.  If 'errors' is NIL, the error
   messages are silently dropped.  It is an unchecked runtime error to
   modify any of the units of 'base'. *)

PROCEDURE IsLibrary (base: Mx.LinkSet;  errors: Wr.T): BOOLEAN;
(* check whether 'base' defines a complete library (ie. all version stamps
   are defined and consistent, all imported interfaces are defined, ...).
   If the units of 'base' form a complete library, TRUE is returned,
   otherwise FALSE is returned.  If there are inconsistencies, error
   messages are written on 'errors'.  If 'errors' is NIL, the error
   messages are silently dropped.  It is an unchecked runtime error to
   modify any of the units of 'base'. *)

END MxCheck.
