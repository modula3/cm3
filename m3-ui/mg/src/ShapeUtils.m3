(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Stephen Harrison and Steve Glassman *)
(*                                                                           *)
(* Last modified on Fri Aug 19 16:28:26 PDT 1994 by steveg                   *)
(*      modified on Sun Jul 19 13:24:57 PDT 1992 by harrison                 *)

MODULE ShapeUtils;

IMPORT R2Path, Math, R2;

PROCEDURE RegularPolygon(sides: CARDINAL := 3; radius := 1.0): R2Path.T =
  VAR
    path := NEW(R2Path.T);
  BEGIN
    path.init();
    FOR i := 1 TO sides DO
      WITH theta = 2.0 * Math.Pi * FLOAT(i) / FLOAT(sides),
           x = radius * FLOAT(Math.cos(FLOAT(theta, LONGREAL))),
           y = radius * FLOAT(Math.sin(FLOAT(theta, LONGREAL))),
           p = R2.T{x, y} DO
        IF i = 1 THEN
          path.moveTo(p);
	ELSE
          path.lineTo(p);
	END;
      END;
    END;
    path.close();

    RETURN path;
  END RegularPolygon;

BEGIN
END ShapeUtils.
