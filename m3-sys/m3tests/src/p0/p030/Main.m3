(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: non-opaque OBJECTs *)

MODULE Main;

FROM Test IMPORT checkI, done;

TYPE
  obj1 = OBJECT
           a: INTEGER;
	 METHODS
	   p (i: INTEGER) := P1;
	 END;

  obj2 = obj1 OBJECT
           b: INTEGER;
         METHODS
	   q (r:REAL) := Q2;
         OVERRIDES
	   p := P2;
	 END;

  obj3 = obj2 OBJECT END;

PROCEDURE P1 (self: obj1;  x: INTEGER) =
  BEGIN
    checkI (self.a, x);
  END P1;

PROCEDURE P2 (self: obj2;  x: INTEGER) =
  BEGIN
    checkI (self.a + self.b, x);
  END P2;

PROCEDURE Q2 (self: obj2;  r: REAL) =
  BEGIN
    self.a := ROUND (r);
    self.p (59);
  END Q2;

BEGIN
  VAR o1: obj1;
      o2: obj2;
      o3: obj3;
  BEGIN
    o1 := NEW (obj1); o2 := NEW (obj2); o3 := NEW (obj3);
    o1 := o2;
    o1 := o3;
    o2 := o1;
    o2 := o3;
    o3 := o1;
    o3 := o2;

    o1.a := 7;
    o1.p (7);
    o2.a := 12;
    o2.b := 25;
    o2.p (37);

    o3.a := o2.a;
    o3.b := o2.b;
    o3.q (34.0);
    o3.p (59);
  END;

  done ();

END Main.
