(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

(* A "WideChar.T" is an "WIDECHAR".  This interface is intended to be
   used as an actual interface for generic interfaces and modules such
   as "Table". *)

INTERFACE WideChar;

IMPORT Word;

TYPE T = WIDECHAR;

CONST Brand = "WideChar";

PROCEDURE Compare (a, b: T): [-1..1];
(* == RETURN (a - b) *)

PROCEDURE Equal (a, b: T): BOOLEAN;
(* == RETURN (a = b) *)

PROCEDURE Hash (a: T): Word.T;
(* == RETURN ORD (a) *)

END WideChar.
