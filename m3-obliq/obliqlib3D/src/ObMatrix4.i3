(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon May 30 13:30:34 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)


INTERFACE ObMatrix4;

IMPORT ObLib, ObValue, Matrix4, SynLocation;

TYPE T <: ObValue.Val;

PROCEDURE SetupPackage ();

PROCEDURE M3ToObliq (READONLY val : Matrix4.T) : T;
PROCEDURE ObliqToM3 (val : T) : Matrix4.T;

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : Matrix4.T RAISES {ObValue.Error};

END ObMatrix4.
