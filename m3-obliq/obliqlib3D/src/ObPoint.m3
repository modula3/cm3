(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug 22 12:03:02 PDT 1995 by najork                   *)
(*       Created on Fri Jul  7 10:53:29 PDT 1995 by najork                   *)


MODULE ObPoint;

IMPORT ObLib, ObValue, Obliq, Point, SynLocation;

CONST 
  pkgname = "ObPoint";


PROCEDURE M3ToObliq (p : Point.T) : Obliq.Val =
  BEGIN
    RETURN Obliq.NewIntArray (ARRAY OF INTEGER {p.h, p.v});
  END M3ToObliq;


PROCEDURE ObliqToM3 (val : Obliq.Val) : Point.T RAISES {ObValue.Error} =
  BEGIN
    WITH h = Obliq.ToInt (Obliq.ArrayGet (val, 0)),
         v = Obliq.ToInt (Obliq.ArrayGet (val, 1)) DO
      RETURN Point.T {h, v};
    END;
  END ObliqToM3;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : Point.T RAISES {ObValue.Error} =
  VAR
    vals : ARRAY [0 .. 1] OF Obliq.Val;
  BEGIN
    TRY
      Obliq.ToArray (args[idx], vals);
      WITH x = Obliq.ToInt (vals[0]),
           y = Obliq.ToInt (vals[1]) DO
        RETURN Point.T {x, y};
      END;
    EXCEPT
    | ObValue.Error =>
      ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc); 
      <* ASSERT FALSE *>
    END;
  END GetArg;


BEGIN
END ObPoint.
