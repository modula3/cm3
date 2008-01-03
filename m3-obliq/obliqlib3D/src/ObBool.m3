(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jul 28 10:23:41 PDT 1994 by najork                   *)
(*       Created on Tue Jan 20 14:00:00 PST 1994 by najork                   *)


MODULE ObBool;

IMPORT ObLib, ObValue, SynLocation;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : BOOLEAN RAISES {ObValue.Error} =
  BEGIN
    TYPECASE args[idx] OF
      ObValue.ValBool (node) => RETURN node.bool;
    ELSE 
      ObValue.BadArgType (idx, "Bool", package.name, opCode.name, loc);
      RETURN FALSE;         (* ... only to suppress compiler warning *)
    END;
  END GetArg;


BEGIN
END ObBool.
