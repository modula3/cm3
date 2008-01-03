(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Ident.i3                                              *)
(* Last Modified On Fri Jun 24 09:36:42 PDT 1994 By kalsow     *)

INTERFACE Ident;

IMPORT M3ID;

TYPE
  StringList  = REF ARRAY OF M3ID.T;
  IntegerList = REF ARRAY OF INTEGER;

VAR
  stack  : StringList  := NIL;
  offset : IntegerList := NIL;
  top    : INTEGER := 0; (* points to the first empty stack element *)

PROCEDURE ParseList (): INTEGER;
(* returns the number of ids in the list *)

PROCEDURE Reset ();

END Ident.
