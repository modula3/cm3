(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jul 28 11:50:01 PDT 1994 by najork                   *)
(*       Created on Tue Jan 20 14:00:00 PST 1994 by najork                   *)


MODULE ObInt;

IMPORT ObLib, ObValue, Obliq, SynLocation;


PROCEDURE M3ToObliq (val : INTEGER) : ObValue.Val =
  BEGIN
    RETURN NEW (ObValue.ValInt, int := val);
  END M3ToObliq;


PROCEDURE ObliqToM3 (val : ObValue.Val) : INTEGER RAISES {ObValue.Error} =
  BEGIN
    TYPECASE val OF
    | ObValue.ValInt  (node) => RETURN node.int;
    ELSE 
      Obliq.RaiseError ("Expected Int");
      RETURN 0;          (* ... only to suppress compiler warning *)
    END;
  END ObliqToM3;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : INTEGER RAISES {ObValue.Error} =
  BEGIN
    TYPECASE args[idx] OF
    | ObValue.ValInt (node) =>
      RETURN node.int;
    ELSE 
      ObValue.BadArgType (idx, "int", package.name, opCode.name, loc);
      RETURN 0;            (* ... only to suppress compiler warning *)
    END;
  END GetArg;


BEGIN
END ObInt.
