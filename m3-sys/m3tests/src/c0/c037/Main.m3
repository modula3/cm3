(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: NEW of opaque OBJECTs with default and non-default fields & methods *)

MODULE Main;

TYPE
  obj1 = OBJECT
           a: INTEGER;
	 METHODS
	   p (i: INTEGER) RAISES ANY := P1;
	 END;

  obj2 <: obj1;

  obj3 = obj2 OBJECT
           b: INTEGER;
         METHODS
	   q (r: REAL) RAISES ANY := Q2;
         OVERRIDES
	   p := P2;
	 END;

PROCEDURE P1 (self: obj1;  x: INTEGER) RAISES ANY =
  BEGIN
    x := self.a;
    self.p (x);
  END P1;


PROCEDURE P2 (self: obj3;  x: INTEGER) RAISES ANY =
  BEGIN
    x := self.a + self.b;
    self.p (x);
  END P2;

PROCEDURE Q2 (self: obj3;  r: REAL) RAISES ANY =
  BEGIN
    self.a := 4;
    self.q (3.14159 + r);
  END Q2;

BEGIN
  VAR o1: obj1;
      o2: obj2;
      o3: obj3;
      i, j: INTEGER;
  BEGIN
    o1 := NEW (obj1);
    o2 := NEW (obj2);
    o3 := NEW (obj3);
    o1 := NEW (obj2, a := i+j);
    o2 := NEW (obj2, p := P1);
    o3 := NEW (obj3, q := Q2, a := 4);
  END;
END Main.
