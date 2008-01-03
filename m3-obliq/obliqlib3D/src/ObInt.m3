(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jul 28 11:50:01 PDT 1994 by najork                   *)
(*       Created on Tue Jan 20 14:00:00 PST 1994 by najork                   *)


MODULE ObInt;

IMPORT ObLib, ObValue, SynLocation;


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
