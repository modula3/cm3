(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Tue Jul 26 14:25:49 PDT 1994 by najork                   *)


MODULE Point3;

IMPORT Fmt, Math, Mth, Process;


PROCEDURE Plus (a, b : T) : T =
  BEGIN
    RETURN T{a.x + b.x, a.y + b.y, a.z + b.z};
  END Plus;


PROCEDURE Minus (a, b : T) : T =
  BEGIN
    RETURN T{a.x - b.x, a.y - b.y, a.z - b.z};
  END Minus;


PROCEDURE TimesScalar (a : T; x : REAL) : T =
  BEGIN
    RETURN T{a.x * x, a.y * x, a.z * x};
  END TimesScalar;


PROCEDURE MidPoint (a, b : T) : T =
  BEGIN
    RETURN T {a.x + (b.x - a.x) / 2.0,
              a.y + (b.y - a.y) / 2.0,
              a.z + (b.z - a.z) / 2.0};
  END MidPoint;


PROCEDURE Distance (a, b : T) : REAL =
  BEGIN
    WITH dx = b.x - a.x, dy = b.y - a.y, dz = b.z - a.z DO
      RETURN FLOAT (Math.sqrt (FLOAT (dx * dx + dy * dy + dz * dz, LONGREAL)));
    END;
  END Distance;


PROCEDURE ToText (a : T) : TEXT =
  BEGIN
    RETURN "(" & Fmt.Real (a.x) & 
           "," & Fmt.Real (a.y) & 
           "," & Fmt.Real (a.z) & 
           ")";
  END ToText;


PROCEDURE Length (p : T) : REAL =
  BEGIN
    RETURN Mth.sqrt (p.x * p.x + p.y * p.y + p.z * p.z);
  END Length;


PROCEDURE DotProduct (a, b : T) : REAL =
  BEGIN
    RETURN a.x * b.x + a.y * b.y + a.z * b.z;
  END DotProduct;


PROCEDURE CrossProduct (a, b : T) : T =
  BEGIN
    RETURN T {a.y * b.z - a.z * b.y,
              a.z * b.x - a.x * b.z,
              a.x * b.y - a.y * b.x};
  END CrossProduct;


PROCEDURE OrthoVector (n : T) : T =
  (* We are looking for a unit vector "m" that is orthogonal to "n". So, we
     have the following two equations to start with:
     (1) "m" orthogonal "n", so "DotProduct(m,n)" = 0, 
         so m.x * n.x + m.y * n.y + m.z * n.z = 0
     (2) "m" is a unit vector, so sqrt(m.x^2 + m.y^2 + m.z^2) = 1
     So we have 3 unknowns (m.x, m.y, and m.z) and 3 equations, leaving us
     with one degree of freedom. 
     If n.x # 0, and we set m.z to 0, we can solve the system to:
        m.xx = 1 / sqrt(1 + (n.x^2 / n.y^2))
        m.y = - (n.x / n.y) / sqrt(1 + (n.x^2 / n.y^2))
     The cases for n.y # 0 and n.z = 0 are similar.
     Passing n = Origin is a fatal error.
  *)
  BEGIN
    IF n.x # 0.0 THEN
      WITH p = n.y / n.x, sub = 1.0 / Mth.sqrt (1.0 + p * p) DO
        RETURN T {-p * sub, sub, 0.0};
      END;
    ELSIF n.y # 0.0 THEN
      WITH p = n.x / n.y, sub = 1.0 / Mth.sqrt (1.0 + p * p) DO
        RETURN T {sub, -p * sub, 0.0};
      END;
    ELSIF n.z # 0.0 THEN
      WITH p = n.x / n.z, sub = 1.0 / Mth.sqrt (1.0 + p * p) DO
        RETURN T {sub, 0.0, -p * sub};
      END;
    ELSE
      Process.Crash ("Fatal Error: called OrthoVector(Origin) \n");
      RETURN Origin;      (* ... only to suppress compiler warnings *)
    END;
  END OrthoVector;


PROCEDURE ScaleToLen (p : T; len : REAL) : T =
  BEGIN
    WITH l = len / Mth.sqrt (p.x * p.x + p.y * p.y + p.z * p.z) DO
      RETURN T {p.x * l, p.y * l, p.z * l};
    END;
  END ScaleToLen;


BEGIN
END Point3.
