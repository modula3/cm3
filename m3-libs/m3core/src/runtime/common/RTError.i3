(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* Runtime error reporting and crashes.... *)

INTERFACE RTError;

PROCEDURE Msg (file: TEXT; line: INTEGER; a, b, c: TEXT := NIL);
(* report an error *)

PROCEDURE MsgS (file: ADDRESS; line: INTEGER; a, b, c: TEXT := NIL);
(* report an error (file = C-style null-terminated string)  *)

PROCEDURE MsgI (msg: TEXT := NIL; i: INTEGER);
(* report an error with an integer argument *)

PROCEDURE MsgPC (pc: INTEGER; a, b, c: TEXT := NIL);
(* report an error at the given PC and crash *)

PROCEDURE ReportPC (pc: INTEGER; a, b, c: TEXT := NIL);
(* report an error at the given PC, but don't crash *)

END RTError.

