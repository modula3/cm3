(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3ID.i3                                               *)
(* Last modified on Fri May 28 08:28:03 PDT 1993 by kalsow     *)

INTERFACE M3ID;

IMPORT M3Token;

PROCEDURE Classify (READONLY x: ARRAY OF CHAR): M3Token.T;
(* If "x" is a Modula-3 keyword, return its token class,
   otherwise return M3Token.Ident.  It is a checked runtime
   error if NUMBER(x) is less than one. *)

END M3ID.
