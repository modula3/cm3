(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT msg, check, checkI, done;
IMPORT RTIO;

CONST I = RTIO.PutInt;
CONST T = RTIO.PutText;

VAR p1, p2 : [1 .. 20];
    n1, n2 : [-20 .. -1];
    x1, x2 : INTEGER;
    d, m: INTEGER;

BEGIN
  msg ("  p1 := 12; p2 := 3");
  p1 := 12; p2 := 3;  
    checkI (p1 DIV p2, 4); checkI (p2 DIV p1, 0);
    checkI (p1 MOD p2, 0); checkI (p2 MOD p1, 3);
  msg ("  p1 := 12; p2 := 5");
  p1 := 12; p2 := 5;  
    checkI (p1 DIV p2, 2); checkI (p2 DIV p1, 0);
    checkI (p1 MOD p2, 2); checkI (p2 MOD p1, 5);

  msg ("  n1 := -12; n2 := -3");
  n1 := -12; n2 := -3;  
    checkI (n1 DIV n2, 4); checkI (n2 DIV n1, 0);
    checkI (n1 MOD n2, 0); checkI (n2 MOD n1, -3);
  msg ("  n1 := -12; n2 := -5");
  n1 := -12; n2 := -5;  
    checkI (n1 DIV n2, 2); checkI (n2 DIV n1, 0);
    checkI (n1 MOD n2, -2); checkI (n2 MOD n1, -5);

  msg ("  n1 := -12; p2 := 3");
  n1 := -12; p2 := 3;  
    checkI (n1 DIV p2, -4); checkI (p2 DIV n1, -1);
    checkI (n1 MOD p2, 0); checkI (p2 MOD n1, -9);
  msg ("  n1 := -12; p2 := 5");
  n1 := -12; p2 := 5;  
    checkI (n1 DIV p2, -3); checkI (p2 DIV n1, -1);
    checkI (n1 MOD p2, 3); checkI (p2 MOD n1, -7);

  msg ("  p1 := 12; n2 := -3");
  p1 := 12; n2 := -3;  
    checkI (p1 DIV n2, -4); checkI (n2 DIV p1, -1);
    checkI (p1 MOD n2, 0);  checkI (n2 MOD p1, 9);
  msg ("  p1 := 12; n2 := -5");
  p1 := 12; n2 := -5;  
    checkI (p1 DIV n2, -3); checkI (n2 DIV p1, -1);
    checkI (p1 MOD n2, -3); checkI (n2 MOD p1, +7);

  msg ("  x1 := 12; x2 := 3");
  x1 := 12; x2 := 3;  
    checkI (x1 DIV x2, 4); checkI (x2 DIV x1, 0);
    checkI (x1 MOD x2, 0); checkI (x2 MOD x1, 3);
  msg ("  x1 := 12; x2 := 5");
  x1 := 12; x2 := 5;  
    checkI (x1 DIV x2, 2); checkI (x2 DIV x1, 0);
    checkI (x1 MOD x2, 2); checkI (x2 MOD x1, 5);

  msg ("  x1 := -12; x2 := -3");
  x1 := -12; x2 := -3;  
    checkI (x1 DIV x2, 4); checkI (x2 DIV x1, 0);
    checkI (x1 MOD x2, 0); checkI (x2 MOD x1, -3);
  msg ("  x1 := -12; x2 := -5");
  x1 := -12; x2 := -5;  
    checkI (x1 DIV x2, 2); checkI (x2 DIV x1, 0);
    checkI (x1 MOD x2, -2); checkI (x2 MOD x1, -5);

  msg ("  x1 := -12; x2 := 3");
  x1 := -12; x2 := 3;  
    checkI (x1 DIV x2, -4); checkI (x2 DIV x1, -1);
    checkI (x1 MOD x2, 0); checkI (x2 MOD x1, -9);
  msg ("  x1 := -12; x2 := 5");
  x1 := -12; x2 := 5;  
    checkI (x1 DIV x2, -3); checkI (x2 DIV x1, -1);
    checkI (x1 MOD x2, 3); checkI (x2 MOD x1, -7);

  msg ("  x1 := 12; x2 := -3");
  x1 := 12; x2 := -3;  
    checkI (x1 DIV x2, -4); checkI (x2 DIV x1, -1);
    checkI (x1 MOD x2, 0);  checkI (x2 MOD x1, 9);
  msg ("  x1 := 12; x2 := -5");
  x1 := 12; x2 := -5;  
    checkI (x1 DIV x2, -3); checkI (x2 DIV x1, -1);
    checkI (x1 MOD x2, -3); checkI (x2 MOD x1, +7);

  FOR i := -10 TO 10 DO
    FOR j := -10 TO 10 DO
      IF j # 0 THEN
        d := i DIV j;
        m := i MOD j;
        IF i = 0 THEN
          check(d = 0);
          check(m = 0);
        ELSIF i = j THEN
          check(d = 1);
          check(m = 0);
        ELSE
          IF i < 0 AND j < 0 THEN check(d >= 0); END;
          IF i < 0 AND j > 0 THEN check(d <= 0); END;
          IF i > 0 AND j < 0 THEN check(d <= 0); END;
          IF i > 0 AND j > 0 THEN check(d >= 0); END;
        END;
        IF m # 0 THEN
          check((j < 0) = (m < 0));
          check((j > 0) = (m > 0));
        END;
        check(i = (j * d + m));
        IF i > 0 AND i < 10 THEN T(" "); END;
        I(i);
        T(" div ");
        IF j > 0 AND j < 10 THEN T(" "); END;
        I(j);
        T(" = ");
        I(d);
        T("\n");
        IF i > 0 AND i < 10 THEN T(" "); END;
        I(i);
        T(" mod ");
        IF j > 0 AND j < 10 THEN T(" "); END;
        I(j);
        T(" = ");
        I(m);
        T("\n");
      END;
    END;
  END;

  RTIO.Flush();
  done ();
  END Main.
