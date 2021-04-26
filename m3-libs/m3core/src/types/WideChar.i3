(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* A "WideChar.T" is an "WIDECHAR".  This interface is intended to be
   used as an actual interface for generic interfaces and modules such
   as "Table". *)

INTERFACE WideChar;

IMPORT Word;

TYPE T = WIDECHAR;

CONST Brand = "WideChar";

CONST WideChar16Max = 16_FFFF;   (* from Target *)
CONST WideChar32Max = 16_10FFFF; (* from Target *)

PROCEDURE Compare (a, b: T): [-1..1];
(* == RETURN (a - b) *)

PROCEDURE Equal (a, b: T): BOOLEAN;
(* == RETURN (a = b) *)

PROCEDURE Hash (a: T): Word.T;
(* == RETURN ORD (a) *)

END WideChar.
