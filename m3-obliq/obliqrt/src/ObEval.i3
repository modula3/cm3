(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObEval;
IMPORT SynLocation, ObTree, ObValue;

  PROCEDURE Setup();
  (* To be called before any other use of this module. *)

  VAR interrupt: BOOLEAN := FALSE;

    PROCEDURE Term(term: ObTree.Term; VAR (*in-out*)env: ObValue.Env; 
      glob: ObValue.GlobalEnv; mySelf: ObValue.RemObj): ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception};

  PROCEDURE Call(clos: ObValue.ValFun; READONLY args: ObValue.Vals; 
    loc: SynLocation.T:=NIL): ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception};

  PROCEDURE CallEngine(engine: ObValue.ValEngine; arg: ObValue.Val;
    loc: SynLocation.T:=NIL): ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception};

END ObEval.
