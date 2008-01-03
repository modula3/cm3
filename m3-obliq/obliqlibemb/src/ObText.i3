(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 16:04:23 PDT 1994 by najork                   *)
(*       Created on Mon Jan 24 09:34:27 PST 1994 by najork                   *)


INTERFACE ObText;

IMPORT ObLib, ObValue, SynLocation;

TYPE T = ObValue.ValText;

PROCEDURE M3ToObliq (val : TEXT) : T;
PROCEDURE ObliqToM3 (val : T) : TEXT;

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : TEXT RAISES {ObValue.Error};

END ObText.
