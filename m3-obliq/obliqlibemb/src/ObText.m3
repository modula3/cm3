(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 16:07:27 PDT 1994 by najork                   *)
(*       Created on Mon Jan 24 09:35:48 PST 1994 by najork                   *)


MODULE ObText;

IMPORT ObLib, ObValue, SynLocation;

PROCEDURE M3ToObliq (val : TEXT) : T =
  BEGIN
    RETURN NEW (T, text := val);
  END M3ToObliq;


PROCEDURE ObliqToM3 (val : T) : TEXT =
  BEGIN
    RETURN val.text;
  END ObliqToM3;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : TEXT RAISES {ObValue.Error} =
  BEGIN
    TYPECASE args[idx] OF
      T (node) => RETURN node.text;
    ELSE 
      ObValue.BadArgType (idx, "text", package.name, opCode.name, loc);
      RETURN "";            (* ... only to suppress compiler warning *)
    END;
  END GetArg;


BEGIN
END ObText.
