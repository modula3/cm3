(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Created by stolfi on Mon May  1 12:42:20 1989               *)
(* Last modified on Fri Sep 29 16:21:08 1989 by muller         *)
(*      modified on Fri Sep 29 15:49:09 1989 by kalsow         *)
(*      modified on Sun May  7 15:35:59 1989 by stolfi         *)

INTERFACE Refany;

(* Some standard operations on REFANY.

   This package defines some standard types and operations
   on REFANY.  When (if) Modula-3 has generics, this interface
   will be useful.

   Index: refanys, exported
*)

TYPE T = REFANY;

PROCEDURE New (value: T): REF T;
(* Allocates and initializes a new heap value *)

PROCEDURE NewArray (size: CARDINAL;  value: T := NIL): REF ARRAY OF T;
(* Allocates a new array from the heap
  and initializes all its elements with the given value *)

PROCEDURE Compare (a, b: T): INTEGER;
(* == RETURN (a - b) *)

PROCEDURE Lt (a, b: T): BOOLEAN;
(* == RETURN (a < b) *)

PROCEDURE Eq (a, b: T): BOOLEAN;
(* == RETURN (a = b) *)

PROCEDURE Hash (a: T): INTEGER;
(* returns a suitable hash value *)

END Refany.
  

