(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug 22 12:03:20 PDT 1995 by najork                   *)
(*       Created on Fri Jul  7 10:53:18 PDT 1995 by najork                   *)


INTERFACE ObPoint;

IMPORT ObLib, ObValue, Obliq, Point, SynLocation;

PROCEDURE M3ToObliq (p : Point.T) : Obliq.Val;
PROCEDURE ObliqToM3 (val : Obliq.Val) : Point.T RAISES {ObValue.Error};

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : Point.T RAISES {ObValue.Error};

END ObPoint.
