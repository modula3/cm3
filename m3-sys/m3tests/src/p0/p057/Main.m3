(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkI, done;

TYPE  Array = ARRAY OF INTEGER;

PROCEDURE Open(READONLY a: Array) RAISES {}=
  VAR c := NEW(REF Array, NUMBER(a));
  BEGIN
     checkI (NUMBER (c^), 2);
  END Open;

BEGIN
   Open(ARRAY [1..2] OF INTEGER {1,2});
   done ();
END Main.
