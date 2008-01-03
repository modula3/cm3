(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jul 28 11:47:12 PDT 1994 by najork                   *)
(*       Created on Tue Jan 20 14:00:00 PST 1994 by najork                   *)


MODULE ObLongReal;

IMPORT ObLib, ObValue, Obliq, SynLocation;

PROCEDURE M3ToObliq (val : LONGREAL) : ObValue.Val =
  BEGIN
    RETURN NEW (ObValue.ValReal, real := val);
  END M3ToObliq;


PROCEDURE ObliqToM3 (val : ObValue.Val) : LONGREAL RAISES {ObValue.Error} =
  BEGIN
    TYPECASE val OF
    | ObValue.ValReal (node) => RETURN node.real;
    | ObValue.ValInt  (node) => RETURN FLOAT (node.int, LONGREAL);
    ELSE 
      Obliq.RaiseError ("Expected Real or Int");
      RETURN 0.0d0;          (* ... only to suppress compiler warning *)
    END;
  END ObliqToM3;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : LONGREAL RAISES {ObValue.Error} =
  BEGIN
    TYPECASE args[idx] OF
    | ObValue.ValReal (node) => RETURN node.real;
    | ObValue.ValInt  (node) => RETURN FLOAT (node.int, LONGREAL);
    ELSE 
      ObValue.BadArgType (idx, "real", package.name, opCode.name, loc);
      RETURN 0.0d0;         (* ... only to suppress compiler warning *)
    END;
  END GetArg;


BEGIN
END ObLongReal.
