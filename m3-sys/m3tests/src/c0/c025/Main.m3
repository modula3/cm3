(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: RECORD types, variables & assignments *)

MODULE Main;

TYPE
  R1 = RECORD
    a, b: INTEGER;
    c:    CHAR;
    d:    INTEGER;
  END;

  R2 = RECORD
    x: INTEGER;
    y: R1;
    z: BOOLEAN;
  END;

VAR
  m, n: R1;
  p, q: R2;

BEGIN

  m.a := 1;
  m.b := 2;
  m.c := 'c';
  m.d := 4;

  m := n;
  p := q;

  p.y := m;

  p.z := BOOLEAN.TRUE;
  p.z := BOOLEAN.FALSE;
  p.z := TRUE;
  p.z := FALSE;
  p.y.c := 'd';
  p.x := 4;

END Main.
