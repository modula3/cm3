(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

PROCEDURE p (e := E.b) RAISES ANY =
BEGIN
CASE e OF
  | E.a =>
  | E.b =>
  ELSE END;
END p;

PROCEDURE q (e : E := E.b) RAISES ANY =
BEGIN
CASE e OF
  | E.a =>
  | E.b =>
  ELSE END;
END q;

PROCEDURE r (e : E := E.c) RAISES ANY =
BEGIN
CASE e OF
  | E.a =>
  | E.b =>
  ELSE END;
END r;

PROCEDURE s (e := E.c) RAISES ANY =
BEGIN
CASE e OF
  | E.a =>
  | E.b =>
  ELSE END;
END s;

BEGIN
END Main.


