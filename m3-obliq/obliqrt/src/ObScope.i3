(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObScope;
IMPORT SynWr, SynLocation, ObErr, ObTree, ObLib;

TYPE
  Env <: ROOT;
  TermEnv <: Env;

  PROCEDURE Setup();
  (* To be called before any other use of this module. *)

PROCEDURE UnboundIdes(wr: SynWr.T; freeEnv: Env) RAISES {ObErr.Fail};

PROCEDURE NewTermEnv(wr: SynWr.T; location: SynLocation.T; 
                     name: ObTree.IdeName; libEnv: ObLib.Env; 
                     rest: TermEnv): TermEnv RAISES {ObErr.Fail};

PROCEDURE ScopeTerm(wr: SynWr.T; term: ObTree.Term; libEnv: ObLib.Env; 
                    VAR (*in-out*)local,global: Env; 
                    temp: BOOLEAN:=FALSE) RAISES {ObErr.Fail};

END ObScope.

