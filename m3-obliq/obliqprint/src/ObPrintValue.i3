
(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObPrintValue;
IMPORT SynWr, ObTree, ObCheck, ObValue, ObLib;

  PROCEDURE Setup();
  (* To be called before any other use of this module. *)

  PROCEDURE PrintPhraseLet(swr: SynWr.T;
    checkEnv, checkEnvStop: ObCheck.Env; env, envStop: ObValue.Env;
    var: BOOLEAN; libEnv: ObLib.Env; depth: INTEGER);

  PROCEDURE PrintVal(swr: SynWr.T; 
    val: ObValue.Val; libEnv: ObLib.Env; printEnv: ObTree.Env; depth: INTEGER);

  PROCEDURE PrintValSummary(swr: SynWr.T; val: ObValue.Val; 
    libEnv: ObLib.Env; printEnv: ObTree.Env);

END ObPrintValue.
