(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObScope;
IMPORT SynLocation, ObErr, ObTree, ObLib;

TYPE
  Env <: ROOT;
  TermEnv <: Env;

  PROCEDURE Setup();
  (* To be called before any other use of this module. *)

PROCEDURE UnboundIdes(freeEnv: Env) RAISES {ObErr.Fail};

PROCEDURE NewTermEnv(location: SynLocation.T; name: ObTree.IdeName;
     libEnv: ObLib.Env; rest: TermEnv): TermEnv RAISES {ObErr.Fail};

PROCEDURE ScopeTerm(term: ObTree.Term; libEnv: ObLib.Env; 
  VAR (*in-out*)local,global: Env; temp: BOOLEAN:=FALSE) RAISES {ObErr.Fail};

END ObScope.

