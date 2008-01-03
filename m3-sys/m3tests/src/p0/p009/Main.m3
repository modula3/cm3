(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: Type operations: ORD, VAL, NUMBER, FIRST, LAST *)

(* Bug: ARRAY [45..12] OF generates "elt[0]" in C and the compiler complains *)

MODULE Main;

FROM Test IMPORT msg, check, done;
FROM Fmt IMPORT Int, Bool;

TYPE 
  E1 = {a, b, c, d, e, f, g};
  E2 = [E1.b .. E1.e];

  E3 = [12 .. 45];
  E4 = [45 .. 12];

  A1 = ARRAY E1 OF INTEGER;
  A2 = ARRAY E2 OF INTEGER;
  A3 = ARRAY E3 OF INTEGER;
  A4 = ARRAY E4 OF INTEGER;

  VAR 
    a : E1 := E1.a;
    c1: E1 := E1.c;
    c2: E2 := E1.d;
    x : INTEGER := 17;
    y : E3 := 17;
    z : INTEGER := 3;

    a1 : A1;
    a2 : A2;
    a3 : A3;
    a4 : A4;
    a5 : REF ARRAY OF INTEGER;


VAR b: BOOLEAN; v: INTEGER;

BEGIN

  msg ("---- Ord ----\n");

  msg ("Ord (E1.a) = " & Int (ORD (E1.a)) & " [0]");   check (ORD (E1.a) = 0);
  msg ("Ord (E1.c) = " & Int (ORD (E1.c)) & " [2]");   check (ORD (E1.c) = 2);
  msg ("Ord (17)   = " & Int (ORD (17))   & " [17]");  check (ORD (17) = 17);
  msg ("");
  msg ("Ord (a)  = " & Int (ORD (a))  & " [0]");       check (ORD (a) = 0);
  msg ("Ord (c1) = " & Int (ORD (c1)) & " [2]");       check (ORD (c1) = 2);
  msg ("Ord (c2) = " & Int (ORD (c2)) & " [3]");       check (ORD (c2) = 3);
  msg ("Ord (x)  = " & Int (ORD (x))  & " [17]");      check (ORD (x) = 17);
  msg ("Ord (y)  = " & Int (ORD (y))  & " [17]");      check (ORD (y) = 17);


  msg ("\n---- Val ----\n");

  b := (VAL (0, E1) = E1.a);
  msg ("(Val (0, E1)  = E1.a) = " & Bool (b) & " [true]");  check (b);
  b := (VAL (2, E1) = E1.c);
  msg ("(Val (2, E1)  = E1.c) = " & Bool (b) & " [true]");  check (b);
  b := (VAL (2, E2) = E1.c);
  msg ("(Val (2, E2)  = E1.c) = " & Bool (b) & " [true]");  check (b);

  v := VAL (17, E3);
  msg ("(Val (17, E3)        = " & Int (v)  & " [17]");   check (v = 17);
  v := VAL (17, INTEGER);
  msg ("(Val (17, INTEGER)   = " & Int (v)  & " [17]");   check (v = 17);
  msg ("");
  b := VAL (z, E1) = E1.d;
  msg ("(Val (z, E1) = E1.d) = " & Bool (b) & " [true]"); check (b);
  b := VAL (z, E2) = E1.d;
  msg ("(Val (z, E2) = E1.d) = " & Bool (b) & " [true]"); check (b);
  v := VAL (x, E3);
  msg ("(Val (x, E3)         = " & Int (v)  & " [17]");   check (v = 17);
  v := VAL (x, INTEGER);
  msg ("(Val (x, INTEGER)    = " & Int (v)  & " [17]");   check (v = 17);
  v := VAL (y, E3);
  msg ("(Val (y, E3)         = " & Int (v)  & " [17]");   check (v = 17);
  v := VAL (y, INTEGER);
  msg ("(Val (y, INTEGER)    = " & Int (v)  & " [17]");   check (v = 17);


  msg ("\n---- Number ----\n");

v := NUMBER (E1); msg ("NUMBER (E1) = " & Int (v) & " [7]");   check (v = 7);
v := NUMBER (E2); msg ("NUMBER (E2) = " & Int (v) & " [4]");   check (v = 4);
v := NUMBER (E3); msg ("NUMBER (E3) = " & Int (v) & " [34]");  check (v = 34);
v := NUMBER (E4); msg ("NUMBER (E4) = " & Int (v) & " [0]");   check (v = 0);

v := NUMBER (A1); msg ("NUMBER (A1) = " & Int (v) & " [7]");   check (v = 7);

v := NUMBER (a1); msg ("NUMBER (a1) = " & Int (v) & " [7]");   check (v = 7);

v := NUMBER (A2); msg ("NUMBER (A2) = " & Int (v) & " [4]");   check (v = 4);
v := NUMBER (a2); msg ("NUMBER (a2) = " & Int (v) & " [4]");   check (v = 4);

v := NUMBER (A3); msg ("NUMBER (A3) = " & Int (v) & " [34]");  check (v = 34);
v := NUMBER (a3); msg ("NUMBER (a3) = " & Int (v) & " [34]");  check (v = 34);

v := NUMBER (A4); msg ("NUMBER (A4) = " & Int (v) & " [0]");   check (v = 0);
v := NUMBER (a4); msg ("NUMBER (a4) = " & Int (v) & " [0]");   check (v = 0);

a5 := NEW (REF ARRAY OF INTEGER, 10);
v := NUMBER (a5^); msg ("NUMBER (a5^) = " & Int (v) & " [10]"); check (v = 10);


msg ("\n---- First ----\n");

b := FIRST (E1) = E1.a;
msg ("(FIRST (E1) = E1.a) = " & Bool (b) & " [true]"); check (b);
b := FIRST (E2) = E1.b;
msg ("(FIRST (E2) = E1.b) = " & Bool (b) & " [true]"); check (b);
v := FIRST (E3);
msg ("FIRST  (E3) = " & Int (v) & " [12]"); check (v = 12);
v := FIRST (E4);
msg ("FIRST  (E4) = " & Int (v) & " [0]"); check (v = 0);
b := FIRST (A1) = E1.a;
msg ("(FIRST (A1) = E1.a) = " & Bool (b) & " [true]"); check (b);
b := FIRST (a1) = E1.a;
msg ("(FIRST (a1) = E1.a) = " & Bool (b) & " [true]"); check (b);
b := FIRST (A2) = E1.b;
msg ("(FIRST (A2) = E1.b) = " & Bool (b) & " [true]"); check (b);
b := FIRST (a2) = E1.b;
msg ("(FIRST (a2) = E1.b) = " & Bool (b) & " [true]"); check (b);
v := FIRST (A3);
msg ("FIRST  (A3) = " & Int (v) & " [12]");  check (v = 12);
v := FIRST (a3);
msg ("FIRST  (a3) = " & Int (v) & " [12]"); check (v = 12);
msg ("FIRST  (A4) = " & Int (FIRST (A4)) & " [0]");
msg ("FIRST  (a4) = " & Int (FIRST (a4)) & " [0]");
v := FIRST (a5^);
msg ("FIRST  (a5^) = " & Int (v) & " [0]"); check (v = 0);
v := FIRST (INTEGER);
msg ("FIRST (INTEGER) = " & Int (v) & " [-2147483648]"); check (v+1 = -2147483647);

  msg ("\n---- Last ----\n");

b := LAST (E1) = E1.g;
  msg ("(LAST (E1) = E1.g) = " & Bool (b) & " [true]"); check (b);
b := LAST (E2) = E1.e;
  msg ("(LAST (E2) = E1.e) = " & Bool (b) & " [true]"); check (b);
v := LAST (E3);
  msg ("LAST  (E3) = " & Int (v) & " [45]"); check (v = 45);
v := LAST (E4);
  msg ("LAST  (E4) = " & Int (v) & " [-1]"); check (v = -1);
  msg ("");
b := LAST (A1) = E1.g;
  msg ("(LAST (A1) = E1.g) = " & Bool (b) & " [true]"); check (b);
b := LAST (a1) = E1.g;
  msg ("(LAST (a1) = E1.g) = " & Bool (b) & " [true]"); check (b);
b := LAST (A2) = E1.e;
  msg ("(LAST (A2) = E1.e) = " & Bool (b) & " [true]"); check (b);
b := LAST (a2) = E1.e;
  msg ("(LAST (a2) = E1.e) = " & Bool (b) & " [true]"); check (b);
v := LAST (A3);
  msg ("LAST  (A3) = " & Int (v) & " [45]"); check (v = 45);
v := LAST (a3);
  msg ("LAST  (a3) = " & Int (v) & " [45]"); check (v = 45);
  msg ("LAST  (A4) = " & Int (LAST (A4)) & " [-1]");
  msg ("LAST  (a4) = " & Int (LAST (a4)) & " [-1]");
v := LAST (a5^);
  msg ("LAST  (a5^) = " & Int (v) & " [9]"); check (v = 9);
v := LAST (INTEGER);
  msg ("LAST (INTEGER) = " & Int (v) & " [2147483647]"); check (v = 2147483647);

  done ();
END Main.
