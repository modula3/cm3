(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Tracer.i3                                             *)
(* Last Modified On Tue Jun 28 09:56:36 PDT 1994 by kalsow     *)
(*      Modified On Tue Jan 19 10:54:35 PST 1993 by mjordan    *)

INTERFACE Tracer;

TYPE T = BRANDED "Tracer.T" OBJECT next: T METHODS apply () END;

PROCEDURE Schedule (t: T);
(* schedule 't' to be called during the next 'EmitPending' *)

PROCEDURE Push (t: T);
PROCEDURE Pop (t: T);
(* delimits the region of code that's to be traced by 't'. *)

PROCEDURE EmitPending ();
(* generate all pending trace calls *)

PROCEDURE Reset ();

END Tracer.
