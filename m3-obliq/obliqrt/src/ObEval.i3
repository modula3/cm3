(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObEval;
IMPORT SynWr, SynLocation, ObTree, ObValue;

  PROCEDURE Setup();
  (* To be called before any other use of this module. *)

  VAR interrupt: BOOLEAN := FALSE;

    PROCEDURE Term(swr: SynWr.T; term: ObTree.Term; 
                   VAR (*in-out*)env: ObValue.Env; glob: ObValue.GlobalEnv;
                   mySelf: ObValue.ValObj): ObValue.Val 
      RAISES {ObValue.Error, ObValue.Exception};

  PROCEDURE Call(clos: ObValue.ValFun; 
                 READONLY args: ObValue.Vals; 
                 swr: SynWr.T; loc: SynLocation.T:=NIL): ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception};

  PROCEDURE CallEngine(engine: ObValue.ValEngine; arg: ObValue.Val;
    loc: SynLocation.T:=NIL): ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception};

  VAR
    traceExecution: BOOLEAN;

END ObEval.
