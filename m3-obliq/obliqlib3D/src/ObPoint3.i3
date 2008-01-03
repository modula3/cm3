(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jul 28 10:09:40 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)


INTERFACE ObPoint3;

IMPORT ObLib, ObValue, Point3, SynLocation;

TYPE T <: ObValue.Val;

PROCEDURE SetupPackage ();

PROCEDURE M3ToObliq (val : Point3.T) : T;
PROCEDURE ObliqToM3 (val : T) : Point3.T RAISES {ObValue.Error};

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : Point3.T RAISES {ObValue.Error};

END ObPoint3.
