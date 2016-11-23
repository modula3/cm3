(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Wed Jul  7 13:36:21 PDT 1993 by kalsow *)
(*      modified on Mon Aug  6 23:04:50 PDT 1990 by ellis  *)
(*      modified on Wed Apr  4 20:55:51 1990 by saxe       *)

MODULE Main;
IMPORT Test, Fmt;

VAR
  a  : REAL;
  ar : ARRAY [0..1000] OF REAL;

PROCEDURE Approx(x, y: REAL;  READONLY b: BOOLEAN) =
  VAR c := ABS(x - y) < MAX(MAX(ABS(x), ABS(y)) / 20000.0, 1.0E-6);
  BEGIN
    IF (b = c) THEN RETURN END;
    IF (b)
      THEN Test.msg ("ERROR: "& Fmt.Real(x) &" is not near "& Fmt.Real (y));
      ELSE Test.msg ("ERROR: "& Fmt.Real(x) &" is too near "& Fmt.Real (y));
    END;
    Test.check (FALSE);
  END Approx;

PROCEDURE CheckR (x, y: REAL;  tag: TEXT) =
  BEGIN
    IF (x # y) THEN
      Test.msg ("ERROR: " & tag);
      Test.checkR (x, y);
    END;
  END CheckR;

PROCEDURE CheckI (x, y: INTEGER;  tag: TEXT) =
  BEGIN
    IF (x # y) THEN
      Test.msg ("ERROR: " & tag);
      Test.checkI (x, y);
    END;
  END CheckI;

VAR r_0: REAL;
    i_0, i_1, i_2: INTEGER;

BEGIN
  Approx(3.14159265, 3.14159277, TRUE);
  Approx(2.7, 2.8, FALSE);
  Approx(0.0, 0.9e-6, TRUE);
  Approx(0.0, -1.0e5, FALSE);

  ar[ 0 ] := 0.0;
  FOR i := 1 TO LAST (ar) DO
    a       := 2.31596 * FLOAT(i);
    a       := 2.0 * (a - FLOAT(TRUNC(a)) - 0.5);
    a       := a * a * a * a * a * 1.0e8;
    ar[ i ] := a;
    IF NOT (a <= 1.0e8) THEN
      Test.msg ("ERROR: ar[" & Fmt.Int (i) & "] = " & Fmt.Real (a));
      Test.check (FALSE);
    END;
  END;

  FOR i := FIRST (ar) TO LAST (ar) DO
    IF ar[ i ] >= 0.0 THEN
      CheckR (FLOAT(FLOOR(ar[ i ])), FLOAT(TRUNC(ar[ i ])), "FLOOR = TRUNC");
    ELSIF FLOAT(FLOOR(ar[ i ])) = ar[ i ] THEN
      (* ok *)
    ELSIF FLOOR(ar[ i ]) = TRUNC(ar[ i ]) - 1 THEN
      (* ok *)
    ELSE
      Test.msg ("ERROR: FLOOR (" & Fmt.Real (ar[i]) & ") = "
                  & Fmt.Int (FLOOR(ar[i])));
      Test.check (FALSE);
    END;

    IF  ABS(FLOAT(ROUND(ar[ i ])) - ar[ i ]) >= 0.500001 THEN
      Test.msg ("ERROR: ROUND (" & Fmt.Real(ar[i]) & " " & Fmt.Int (TRUNC(ar[i])) & " + "
                  & Fmt.Real (ar[i] - FLOAT (TRUNC (ar[i]))) & ") = "
                  & Fmt.Int (ROUND(ar[i])));
      Test.check (FALSE);
    END;
 
    FOR j := 1 TO 1000 DO
      CheckR (ar[ i ] + ar[ j ], ar[ j ] + ar[ i ], "x + y = y + x");
      Approx((ar[ i ] * ar[ j ]) * ar[ j ],
              ar[ i ] * (ar[ j ] * ar[ j ]), TRUE);
      Approx((ar[ i ] / ar[ j ]) * ar[ j ], ar[ i ], TRUE);
      Approx((ar[ i ] + ar[ j ]) * (ar[ i ] - ar[ j ]),
              ar[ i ] * ar[ i ] - ar[ j ] * ar[ j ], TRUE);

      r_0 := MIN (ar[ i ], ar[ j ]);
      i_0 := FLOOR (r_0);
      i_1 := FLOOR (ar[ j ]);
      i_2 := FLOOR (ar[ i ]);
      i_1 := MIN (i_1, i_2);
      CheckI (i_0, i_1, "FLOOR(MIN) = MIN(FLOOR)");

      CheckI (FLOOR(MIN(ar[ i ], ar[ j ])),
              MIN(FLOOR(ar[ j ]), FLOOR(ar[ i ])), "FLOOR(MIN) = MIN(FLOOR)");
      (*******
      ********)

      CheckR (MIN(ar[ i ], ar[ j ]) + MAX(ar[ i ], ar[ j ]),
                   ar[ i ] + ar[ j ], "MIN/MAX");
    END;
  END;

  Test.done ();

END Main.

