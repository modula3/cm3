(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jul 28 10:17:21 PDT 1994 by najork                   *)
(*       Created on Tue Jan 20 14:00:00 PST 1994 by najork                   *)


INTERFACE ObReal;

IMPORT ObLib, ObValue, SynLocation;

PROCEDURE M3ToObliq (val : REAL) : ObValue.Val;
PROCEDURE ObliqToM3 (val : ObValue.Val) : REAL RAISES {ObValue.Error};

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : REAL RAISES {ObValue.Error};

END ObReal.
