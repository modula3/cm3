(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: opaque OBJECTS *)

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
  BEGIN
    o1 := o2;
    o1 := o3;
    o2 := o1;
    o2 := o3;
    o3 := o1;
    o3 := o2;
    o1.p (4);
    o2.p (o3.b);
    o3.q (4.5);
  END;
END Main.
