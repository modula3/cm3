(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxCheck.i3                                            *)
(* Last Modified On Tue Mar 23 09:21:01 PST 1993 By kalsow     *)

INTERFACE MxCheck;

IMPORT Wr, Mx;

(*------------------------------------------------------------------------*)

PROCEDURE IsProgram (base: Mx.LinkSet;  errors: Wr.T): BOOLEAN;
(* Check whether 'base' defines a complete program (ie. all version stamps
   are defined and consistent, all imported interfaces are defined, 'Main'
   is exported).  Return TRUE IFF the units of 'base' form a complete program.
   If there are inconsistencies, write error messages on 'errors'.
   If 'errors' is NIL, silently drop the error messages.
   It is an unchecked runtime error to modify any of the units of 'base'. *)

PROCEDURE IsLibrary (base: Mx.LinkSet;  errors: Wr.T): BOOLEAN;
(* Check whether 'base' defines a complete library (ie. all version stamps
   are defined and consistent, all imported interfaces are defined, ...).
   Return TRUE IFF the units of 'base' form a complete library.
   If there are inconsistencies, write error messages on 'errors'.
   If 'errors' is NIL, silently drop the error messages.
   It is an unchecked runtime error to modify any of the units of 'base'. *)

END MxCheck.
