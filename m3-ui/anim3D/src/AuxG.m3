(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Thu Feb  2 01:19:20 PST 1995 by najork                   *)


MODULE AuxG;

IMPORT Math, Mth, Point3;


TYPE
  CircleCache = REF RECORD
    prec  : INTEGER;
    verts : REF ARRAY OF Point3.T;
    next  : CircleCache;
  END;

VAR
  circleCache : CircleCache := NIL;

PROCEDURE GetUnitCirclePoints (prec : INTEGER) : REF ARRAY OF Point3.T =
  VAR
    tmp := circleCache;
    ang := 0.0;
  BEGIN
    WHILE tmp # NIL DO
      IF tmp.prec = prec THEN
        (* We have cached a circle at this resolution, so return it *)
        RETURN tmp.verts;
      END;
      tmp := tmp.next;
    END;

    (* So far, we have not computed a circle at this resolution ... *)
    WITH v = NEW (REF ARRAY OF Point3.T, prec + 1),
         Dang = 2.0 * Math.Pi / FLOAT (prec) DO

      FOR i := 0 TO prec - 1 DO
        v[i] := Point3.T {Mth.sin(ang), Mth.cos(ang), 0.0};
        ang := ang + Dang;
      END;
      v[prec] := v[0];
      circleCache := NEW (CircleCache, 
                          prec  := prec, 
                          verts := v,
                          next  := circleCache);
      RETURN v;
    END;
  END GetUnitCirclePoints;


BEGIN
END AuxG.
