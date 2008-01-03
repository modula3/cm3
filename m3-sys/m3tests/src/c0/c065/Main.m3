(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: ADDRESS arithmetic *)

UNSAFE MODULE Main;

VAR
  x, y : ADDRESS;
  i : INTEGER;

BEGIN

x := x + i;
x := x + 4;
INC (x, 5);

x := x - i;
x := x - 7;
DEC (x, 6);

i := x - y;

x := LOOPHOLE (234, ADDRESS);

x := LOOPHOLE (234, ADDRESS) + i;
x := LOOPHOLE (234, ADDRESS) + 8;

x := LOOPHOLE (234, ADDRESS) - i;
x := LOOPHOLE (234, ADDRESS) - 10;

i := LOOPHOLE (234, ADDRESS) - y;
i := x - LOOPHOLE (234, ADDRESS);
i := LOOPHOLE (234, ADDRESS) - LOOPHOLE (234, ADDRESS);

x := ADR (x);

END Main.
