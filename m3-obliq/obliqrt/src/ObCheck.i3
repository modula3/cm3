(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 15:04:17 1997
 *)

INTERFACE ObCheck;
IMPORT ObErr, ObTree;

  TYPE

    Env <: ObTree.Env;
      (* Inherit from ObTree.Env for printing *) 

    TermEnv <: TermEnvBase;
    TermEnvBase =
      Env BRANDED "ObCheck.TermEnvBase" OBJECT
	(* type: Type; *)
      END;

  PROCEDURE Setup();
  (* To be called before any other use of this module. *)

  PROCEDURE NewTermEnv(name: ObTree.IdeName; rest: Env): Env;

  PROCEDURE CheckTerm(term: ObTree.Term; VAR (*in-out*)env: Env) 
  RAISES {ObErr.Fail};

END ObCheck.
