(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun 21 17:40:54 PDT 1995 by heydon                   *)

INTERFACE ExternalProc;

(* Types and procedures for defining Juno modules implemented by external
   procedures. *)

IMPORT View, JunoAST, JunoScope, JunoRT;

TYPE
  Closure = JunoRT.ExternalCode BRANDED "ExternalProc.Closure" OBJECT
    rt: View.Root
  END;
  (* A closure is the object passed to external procedures. *)

PROCEDURE SetupBind(mod: JunoAST.Id; scp: JunoScope.T; rt: View.Root := NIL);
(* This module maintains a current root, a current module name, and a current
   scope to be used by the "Bind" procedure below. Set the current root to
   "rt", the current module name to "mod", and the current scope to "scp". *) 

PROCEDURE Bind(name: JunoAST.Id; cl: Closure; in, out := 0);
(* Allocate a slot in the external code table for the procedure named
   "<mod>.<name>" (where <mod> is the current module); set "cl.rt" to the
   current root; and bind "name" to a procedure entity with "in" IN
   parameters, 0 INOUT parameters, and "out" OUT parameters in the current
   scope. *)

END ExternalProc.
