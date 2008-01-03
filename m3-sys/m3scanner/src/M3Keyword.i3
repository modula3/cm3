(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3Keyword.i3                                          *)
(* Last modified on Fri May 28 08:28:03 PDT 1993 by kalsow     *)

INTERFACE M3Keyword;

IMPORT M3Scanner;

PROCEDURE Classify (READONLY x: ARRAY OF CHAR): M3Scanner.TK;
(* If "x" is a Modula-3 keyword, return its token class,
   otherwise return M3Scanner.TK_Ident.  It is a checked runtime
   error if NUMBER(x) is less than one. *)

END M3Keyword.
