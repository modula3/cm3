(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jul  6 11:09:28 PDT 1994 by kalsow     *)

INTERFACE Arg;

TYPE
  T = REF RECORD
    arg: TEXT;
    next: T;
  END;

TYPE
  List = REF RECORD
    head, tail: T := NIL;
    cnt := 0;
  END;

PROCEDURE NewList (): List;
(* return [ ] *)

PROCEDURE Append (list: List;  val: TEXT);
(* list := [ list  val ] *)

PROCEDURE Prepend (list: List;  val: TEXT);
(* list := [ val list ] *)

PROCEDURE AppendL (a, b: List);
(* a := [ a b ] *)

PROCEDURE Pop (list: List): TEXT;
(* list = [ a b ]  =>  list := [ b ]; return a *)

PROCEDURE Flatten (list: List;  other: TEXT): REF ARRAY OF TEXT;
(* return an array of text equal to "Append (list, other)" *)

END Arg.
