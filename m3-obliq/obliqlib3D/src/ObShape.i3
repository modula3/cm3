(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jun  3 22:49:00 PDT 1994 by najork                   *)
(*       Created on Fri Jan 21 19:07:02 PST 1994 by najork                   *)


INTERFACE ObShape;

IMPORT GO, ObLib, ObValue, SynLocation;

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : GO.Shape 
    RAISES {ObValue.Error};


END ObShape.
