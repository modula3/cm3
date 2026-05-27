(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Tracer.i3                                             *)
(* Last Modified On Tue Jun 28 09:56:36 PDT 1994 by kalsow     *)
(*      Modified On Tue Jan 19 10:54:35 PST 1993 by mjordan    *)

INTERFACE Tracer;

TYPE T = BRANDED "Tracer.T" OBJECT next: T
           METHODS
             apply ();                         (* CG code-gen callback *)
             msir_apply () := NoOpMSIR;        (* MSIR code-gen callback, default no-op *)
           END;

PROCEDURE NoOpMSIR (self: T);
(* default no-op for msir_apply; exported so subclasses may re-use it *)

PROCEDURE Schedule (t: T);
(* schedule 't' to be called during the next 'EmitPending' *)

PROCEDURE Push (t: T);
PROCEDURE Pop (t: T);
(* delimits the region of code that's to be traced by 't'. *)

PROCEDURE EmitPending ();
(* generate all pending CG trace calls *)

PROCEDURE EmitPendingMSIR ();
(* generate all pending MSIR trace calls (mirrors EmitPending for the MSIR pass) *)

PROCEDURE Reset ();

END Tracer.
