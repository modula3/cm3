(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObCheck;
IMPORT ObErr, ObTree;

  TYPE

    Env <: ObTree.Env;
      (* Inherit from ObTree.Env for printing *) 

    TermEnv <: TermEnvBase;
    TermEnvBase =
      Env BRANDED OBJECT
	(* type: Type; *)
      END;

  PROCEDURE Setup();
  (* To be called before any other use of this module. *)

  PROCEDURE NewTermEnv(name: ObTree.IdeName; rest: Env): Env;

  PROCEDURE CheckTerm(term: ObTree.Term; VAR (*in-out*)env: Env) 
  RAISES {ObErr.Fail};

END ObCheck.
