(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jul 28 10:25:11 PDT 1994 by najork                   *)
(*       Created on Tue Jan 20 14:00:00 PST 1994 by najork                   *)


MODULE ObReal;

IMPORT ObLib, ObValue, Obliq, SynLocation;

PROCEDURE M3ToObliq (val : REAL) : ObValue.Val =
  BEGIN
    RETURN NEW (ObValue.ValReal, real := FLOAT (val, LONGREAL));
  END M3ToObliq;


PROCEDURE ObliqToM3 (val : ObValue.Val) : REAL RAISES {ObValue.Error} =
  BEGIN
    TYPECASE val OF
    | ObValue.ValReal (node) => RETURN FLOAT (node.real);
    | ObValue.ValInt  (node) => RETURN FLOAT (node.int);
    ELSE 
      Obliq.RaiseError ("Expected Real or Int");
      RETURN 0.0;          (* ... only to suppress compiler warning *)
    END;
  END ObliqToM3;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : REAL RAISES {ObValue.Error} =
  BEGIN
    TYPECASE args[idx] OF
    | ObValue.ValReal (node) => RETURN FLOAT (node.real);
    | ObValue.ValInt  (node) => RETURN FLOAT (node.int);
    ELSE 
      ObValue.BadArgType (idx, "real", package.name, opCode.name, loc);
      RETURN 0.0;           (* ... only to suppress compiler warning *)
    END;
  END GetArg;


BEGIN
END ObReal.
