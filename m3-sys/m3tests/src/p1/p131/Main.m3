(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT Test;

PROCEDURE R (r: REAL;  trunc, round, floor, ceiling: INTEGER) =
  BEGIN
    Test.checkI (TRUNC (r), trunc);
    Test.checkI (ROUND (r), round);
    Test.checkI (FLOOR (r), floor);
    Test.checkI (CEILING (r), ceiling);
    Test.checkR (FLOAT (trunc), FLOAT (TRUNC (r)));
    Test.checkI (TRUNC (FLOAT (r, LONGREAL)), trunc);
  END R;

PROCEDURE L (r: LONGREAL;  trunc, round, floor, ceiling: INTEGER) =
  BEGIN
    Test.checkI (TRUNC (r), trunc);
    Test.checkI (ROUND (r), round);
    Test.checkI (FLOOR (r), floor);
    Test.checkI (CEILING (r), ceiling);
    Test.checkL (FLOAT (TRUNC (r), LONGREAL), FLOAT (trunc, LONGREAL));
    Test.checkI (TRUNC (FLOAT (r)), trunc);
  END L;

PROCEDURE X (r: EXTENDED;  trunc, round, floor, ceiling: INTEGER) =
  BEGIN
    Test.checkI (TRUNC (r), trunc);
    Test.checkI (ROUND (r), round);
    Test.checkI (FLOOR (r), floor);
    Test.checkI (CEILING (r), ceiling);
    Test.checkX (FLOAT (TRUNC (r), EXTENDED), FLOAT (trunc, EXTENDED));
    Test.checkI (TRUNC (FLOAT (r)), trunc);
  END X;

BEGIN
    R (0.0,    0,    0,    0,    0);
    L (0.0D0,  0,    0,    0,    0);
    X (0.0X0,  0,    0,    0,    0);

    R (1.0,    1,    1,    1,    1);
    L (1.0D0,  1,    1,    1,    1);
    X (1.0X0,  1,    1,    1,    1);

    R (1.2,    1,    1,    1,    2);
    L (1.2D0,  1,    1,    1,    2);
    X (1.2X0,  1,    1,    1,    2);

    R (1.7,    1,    2,    1,    2);
    L (1.7D0,  1,    2,    1,    2);
    X (1.7X0,  1,    2,    1,    2);

    R (-1.0,   -1,  -1,   -1,   -1);
    L (-1.0D0, -1,  -1,   -1,   -1);
    X (-1.0X0, -1,  -1,   -1,   -1);

    R (-1.2,   -1,  -1,   -2,   -1);
    L (-1.2D0, -1,  -1,   -2,   -1);
    X (-1.2X0, -1,  -1,   -2,   -1);

    R (-1.7,   -1,  -2,   -2,   -1);
    L (-1.7D0, -1,  -2,   -2,   -1);
    X (-1.7X0, -1,  -2,   -2,   -1);

    Test.done ();
END Main.

