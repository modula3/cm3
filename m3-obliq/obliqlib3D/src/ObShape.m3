(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun  3 22:41:47 PDT 1994 by najork                   *)
(*       Created on Tue Jan 20 09:00:00 PST 1994 by najork                   *)


MODULE ObShape;

IMPORT GO, ObLib, ObValue, SynLocation, Text;

CONST
  pkgname = "Shape";


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : GO.Shape 
    RAISES {ObValue.Error} =
  BEGIN
    TYPECASE args[idx] OF 
    | ObValue.ValText (node) => 
      IF Text.Equal (node.text, "Unknown") THEN
        RETURN GO.Shape.Unknown;
      ELSIF Text.Equal (node.text, "Complex") THEN
        RETURN GO.Shape.Complex;
      ELSIF Text.Equal (node.text, "NonConvex") THEN
        RETURN GO.Shape.NonConvex;
      ELSIF Text.Equal (node.text, "Convex") THEN
        RETURN GO.Shape.Convex;
      ELSE
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
        RETURN GO.Shape.Complex; (* only to suppress compiler warning *)
      END;
    ELSE 
      ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
      RETURN GO.Shape.Complex; (* only to suppress compiler warning *)
    END;
  END GetArg;


BEGIN
END ObShape.
