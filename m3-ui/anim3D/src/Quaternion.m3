(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jul 26 14:27:30 PDT 1994 by najork                   *)
(*       Created on Tue Jun 21 16:16:43 PDT 1994 by najork                   *)


MODULE Quaternion;

IMPORT FloatMode, Matrix4, Mth, RealFloat;

<* FATAL FloatMode.Trap *>


PROCEDURE ToMatrix4 (q : T) : Matrix4.T =
  BEGIN
    WITH aa = q.a * q.a, ab = q.a * q.b, ac = q.a * q.c, ad = q.a * q.d, 
         bb = q.b * q.b, bc = q.b * q.c, bd = q.b * q.d, 
         cc = q.c * q.c, cd = q.c * q.d,
         dd = q.d * q.d DO
      RETURN Matrix4.T {
                 Matrix4.Row {aa-bb-cc+dd, 2.0*(ab-cd), 2.0*(ac+bd), 0.0},
                 Matrix4.Row {2.0*(ab+cd),-aa+bb-cc+dd, 2.0*(bc-ad), 0.0},
                 Matrix4.Row {2.0*(ac-bd), 2.0*(bc+ad),-aa-bb+cc+dd, 0.0},
                 Matrix4.Row {0.0, 0.0, 0.0, aa+bb+cc+dd}};
    END;
  END ToMatrix4;


PROCEDURE FromMatrix4 (READONLY M : Matrix4.T) : T =
  VAR
    q : T;
  BEGIN
    WITH aa4 =   M[0][0] - M[1][1] - M[2][2] + M[3][3],
         bb4 = - M[0][0] + M[1][1] - M[2][2] + M[3][3],
         cc4 = - M[0][0] - M[1][1] + M[2][2] + M[3][3],
         dd4 =   M[0][0] + M[1][1] + M[2][2] + M[3][3],
         max = MAX (aa4, MAX (bb4, MAX (cc4, dd4))) DO
      (* aa4 stands for 4.0 * q.a * q.a, where q is the quaternion we are 
         looking for; similar for bb4, cc4, and dd4. At this point, 
         0 <= aa4 <= 1, or M was invalid. We are using the largest of 
         aa4, bb4, cc4, dd4 to guarantee numeric stability. *)
      IF aa4 = max THEN
        WITH a  = RealFloat.Sqrt (aa4 / 4.0),
             ab = (M[1][0] + M[0][1]) / 4.0,
             ac = (M[0][2] + M[2][0]) / 4.0,
             ad = (M[2][1] - M[1][2]) / 4.0 DO
          q := T {a, ab / a, ac / a, ad / a};
        END;
      ELSIF bb4 = max THEN
        WITH b  = RealFloat.Sqrt (bb4 / 4.0),
             ab = (M[1][0] + M[0][1]) / 4.0,
             bc = (M[2][1] + M[1][2]) / 4.0,
             bd = (M[0][2] - M[2][0]) / 4.0 DO
          q := T {ab / b, b, bc / b, bd / b};
        END;
      ELSIF cc4 = max THEN
        WITH c  = RealFloat.Sqrt (cc4 / 4.0),
             ac = (M[0][2] + M[2][0]) / 4.0,
             bc = (M[2][1] + M[1][2]) / 4.0,
             cd = (M[1][0] - M[0][1]) / 4.0 DO
          q := T {ac / c, bc / c, c, cd / c};
        END;
      ELSIF dd4 = max THEN
        WITH d  = RealFloat.Sqrt (dd4 / 4.0),
             ad = (M[2][1] - M[1][2]) / 4.0,
             bd = (M[0][2] - M[2][0]) / 4.0,
             cd = (M[1][0] - M[0][1]) / 4.0 DO
          q := T {ad / d, bd / d, cd / d, d};
        END;
      ELSE
        <* ASSERT FALSE *>
      END;
    END;
    IF q.d < 0.0 THEN
      q.a := -q.a;
      q.b := -q.b;
      q.c := -q.c;
      q.d := -q.d;
    END;
    RETURN q;
  END FromMatrix4;


PROCEDURE Interpolate (q : T; f : REAL) : T =

  PROCEDURE Normalize (a, b, c, d : REAL) : T =
    BEGIN
      WITH s = RealFloat.Sqrt ((a*a + b*b + c*c) / (1.0 - d*d)) DO
        RETURN T {a / s, b / s, c / s, d};
      END;
    END Normalize;

  BEGIN
    WITH theta = f * Mth.acos (q.d) * 2.0,
         d = Mth.cos (theta / 2.0) DO
      RETURN Normalize (q.a, q.b, q.c, d);
    END;
  END Interpolate;


BEGIN
END Quaternion.
