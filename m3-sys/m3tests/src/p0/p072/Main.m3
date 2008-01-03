(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Nov 30 20:54:11 1990 by muller        *)


MODULE Main;

FROM Test IMPORT checkI, done;

TYPE
  foo = [0..255];

VAR
  a : ARRAY CHAR OF INTEGER;
  u,v : ARRAY foo OF INTEGER;
  x,y : foo;

BEGIN
  FOR c := '\000' TO '\377' DO 
    a[c] := 10; END;
  checkI (a['a'], 10);    


  x := FIRST (foo);
  y := LAST (foo);
  FOR i := x TO y DO
    u [i] := 20; END;
  checkI (u[10], 20);

  FOR i := FIRST (foo) TO LAST (foo) DO
    v [i] := 30; END;
  checkI (v[10], 30);

  done ();
END Main.
