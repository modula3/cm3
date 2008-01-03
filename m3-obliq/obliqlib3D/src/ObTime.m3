(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Apr 12 15:23:00 PDT 1996 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)

MODULE ObTime;

IMPORT ObAux, ObCommand, ObLib, ObValue, Obliq, SynLocation, Time;


CONST
  pkgname = "Time";


(*****************************************************************************)
(* Setup procedures                                                          *)
(*****************************************************************************)


PROCEDURE SetupPackage () =

  PROCEDURE NewOpCode (name: TEXT; arity: INTEGER; code: Code) : OpCode =
    BEGIN
      RETURN NEW (OpCode, name := name, arity := arity, code := code);
    END NewOpCode;

  TYPE 
    OpCodes = ARRAY OF ObLib.OpCode;
  VAR 
    opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW (REF OpCodes, NUMBER (Code));
    opCodes^ := OpCodes {NewOpCode ("Now", 0, Code.Now)};

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {Now};

  OpCode = ObLib.OpCode OBJECT
    code: Code;
  END;
    
  Package = ObLib.T OBJECT
  OVERRIDES
    Eval := DoEval;
  END;


PROCEDURE DoEval (<* UNUSED *> self         : Package; 
                               opCode       : ObLib.OpCode; 
                  <* UNUSED *> arity        : ObLib.OpArity; 
                  <* UNUSED *> READONLY args: ObValue.ArgArray; 
                  <* UNUSED *> temp         : BOOLEAN;
                  <* UNUSED *> loc          : SynLocation.T) : ObValue.Val =
  BEGIN
    CASE NARROW (opCode, OpCode).code OF
    | Code.Now => 
      RETURN Obliq.NewReal (Time.Now ());
    END;
  END DoEval;


(*****************************************************************************)
(* Help                                                                      *)
(*****************************************************************************)


PROCEDURE Help (self : ObCommand.T; arg : TEXT; <* UNUSED *> data : REFANY) =
  BEGIN
    ObAux.Help (self, arg, pkgname);
  END Help;


BEGIN
END ObTime.
