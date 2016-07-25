(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkI, done;

PROCEDURE app (h: PROCEDURE (i: INTEGER): INTEGER; i: INTEGER): INTEGER =
  BEGIN
    RETURN h (i);
  END app;

PROCEDURE appCond (h: PROCEDURE (i: INTEGER): INTEGER; i: INTEGER): INTEGER =
  BEGIN
    IF i < 0 THEN 
      RETURN h (i+14);
    ELSE
      RETURN h (i);
    END
  END appCond;

PROCEDURE appTwice (h: PROCEDURE (i: INTEGER): INTEGER; i: INTEGER): INTEGER =
  VAR s, t, u: INTEGER; 
  BEGIN
    s := h (i);
    t := i + 4; 
    u := h (t); 
    RETURN s + u;
  END appTwice;

PROCEDURE f (x: INTEGER): INTEGER = 

  PROCEDURE g (y: INTEGER): INTEGER =
    BEGIN
      RETURN x + y;
    END g;

  BEGIN
    RETURN app (g, 1);
  END f;

PROCEDURE f1 (x1: INTEGER): INTEGER = 

  PROCEDURE g (y1: INTEGER): INTEGER =
    BEGIN
      RETURN x1 + y1;
    END g;

  BEGIN
    RETURN appTwice (g, 3);
  END f1;

PROCEDURE f2 (x1: INTEGER): INTEGER = 

  PROCEDURE g (y1: INTEGER): INTEGER =
    BEGIN
      RETURN x1 + y1;
    END g;

  VAR a, b: INTEGER; 

  BEGIN
    a := appCond (g, 4);
    b := appCond (g, -2);
    RETURN a + b;
  END f2;

VAR
  i, j, k, m: INTEGER;

BEGIN
  i := f (7);
  checkI (i, 8);

  j := f1 (10);
  checkI (j, 30);

  k := f (8);
  checkI (k, 9);

  m := f2 (12);
  checkI (m, 40);

  done ();
END Main.

    
