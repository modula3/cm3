(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT msg, checkI, done;

VAR p1, p2 : [1 .. 20];
    n1, n2 : [-20 .. -1];
    x1, x2 : INTEGER;

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
  done ();
  END Main.

