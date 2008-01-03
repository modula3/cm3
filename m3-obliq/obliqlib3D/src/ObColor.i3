(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon May 30 11:52:06 PDT 1994 by najork                   *)
(*       Created on Tue Jan 20 14:00:00 PST 1994 by najork                   *)


INTERFACE ObColor;

IMPORT Color, ObLib, ObLibUI, ObValue, SynLocation;

TYPE T = ObLibUI.ValColor;

PROCEDURE M3ToObliq (val : Color.T) : T;
PROCEDURE ObliqToM3 (val : ObValue.Val) : Color.T RAISES {ObValue.Error};

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : Color.T RAISES {ObValue.Error};

END ObColor.
