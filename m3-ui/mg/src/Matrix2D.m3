(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Stephen Harrison and Steve Glassman *)
(*                                                                           *)
(* Last modified on Fri Jul 17 19:22:44 PDT 1992 by harrison                 *)

MODULE Matrix2D;

IMPORT R2, Math;

PROCEDURE Scale(READONLY sx, sy: REAL): T =
  BEGIN
    RETURN
      T{sx,  0.0,
        0.0, sy,
        0.0, 0.0};
  END Scale;

PROCEDURE Translate(READONLY tx, ty: REAL): T =
  BEGIN
    RETURN
      T{1.0, 0.0,
        0.0, 1.0,
        tx,  ty};
  END Translate;

PROCEDURE Rotate(READONLY theta: REAL): T =
  VAR
    s := FLOAT(Math.sin(FLOAT(theta, LONGREAL)));
    c := FLOAT(Math.cos(FLOAT(theta, LONGREAL)));
  BEGIN
    RETURN
      T{ c, s,
        -s, c,
         0.0, 0.0};
  END Rotate;

PROCEDURE Concat (READONLY m, n: T): T =
  BEGIN
    RETURN T{m[0] * n[0] + m[1] * n[2], m[0] * n[1] + m[1] * n[3],
             m[2] * n[0] + m[3] * n[2], m[2] * n[1] + m[3] * n[3],
             m[4] * n[0] + m[5] * n[2] + n[4],
             m[4] * n[1] + m[5] * n[3] + n[5]};
  END Concat;

(*| from maple:
                                     [ a  b  0 ]
                                     [         ]
                                A := [ c  d  0 ]
                                     [         ]
                                     [ e  f  1 ]


   inverse(A);
                        [      d             b        ]
                        [  ---------   - ---------  0 ]
                        [  a d - c b     a d - c b    ]
                        [                             ]
                        [       c           a         ]
                        [ - ---------   ---------   0 ]
                        [   a d - c b   a d - c b     ]
                        [                             ]
                        [  c f - d e     a f - b e    ]
                        [  ---------   - ---------  1 ]
                        [  a d - c b     a d - c b    ]
*)

PROCEDURE Inverse (READONLY m: T): T =
  VAR det := m[0] * m[3] - m[1] * m[2];
  BEGIN
    RETURN T{ m[3] / det,
             -m[1] / det,
             -m[2] / det,
              m[0] / det,
             (m[2] * m[5] - m[3] * m[4]) / det,
             (m[1] * m[4] - m[0] * m[5]) / det};
  END Inverse;

PROCEDURE Concat3 (READONLY l, m, n: T): T =
  BEGIN
    RETURN T{(l[0] * m[0] + l[1] * m[2]) * n[0]
               + (l[0] * m[1] + l[1] * m[3]) * n[2],
             (l[0] * m[0] + l[1] * m[2]) * n[1]
               + (l[0] * m[1] + l[1] * m[3]) * n[3],
             (l[2] * m[0] + l[3] * m[2]) * n[0]
               + (l[2] * m[1] + l[3] * m[3]) * n[2],
             (l[2] * m[0] + l[3] * m[2]) * n[1]
               + (l[2] * m[1] + l[3] * m[3]) * n[3],
             (l[4] * m[0] + l[5] * m[2] + m[4]) * n[0]
               + (l[4] * m[1] + l[5] * m[3] + m[5]) * n[2] + n[4],
             (l[4] * m[0] + l[5] * m[2] + m[4]) * n[1]
               + (l[4] * m[1] + l[5] * m[3] + m[5]) * n[3] + n[5]};
  END Concat3;

PROCEDURE Transform(READONLY m: T; READONLY p: R2.T): R2.T =
  BEGIN
    RETURN
      R2.T{p[0] * m[0] + p[1] * m[2] + m[4],
           p[0] * m[1] + p[1] * m[3] + m[5]};
  END Transform;

BEGIN
END Matrix2D.
