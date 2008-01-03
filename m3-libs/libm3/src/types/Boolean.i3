(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Sep 15 11:00:27 PDT 1994 by heydon         *)
(*      modified on Tue May 11 16:37:06 PDT 1993 by swart          *)
(*      modified on Thu Apr 22 09:57:00 PDT 1993 by mcjones        *)
(*      modified on Thu Nov  2 21:55:31 1989 by muller         *)
(*      modified on Fri Sep 29 15:44:47 1989 by kalsow         *)
(*      modified on Sun May  7 15:35:59 1989 by stolfi         *)

INTERFACE Boolean;

(* Some standard operations on BOOLEAN for use by Generic modules.

   Index: booleans, generics
*)

IMPORT Word;

TYPE T = BOOLEAN;

CONST Brand = "Boolean";

PROCEDURE Compare (a, b: T): [-1..1];
(* == RETURN (a - b) *)

PROCEDURE Equal (a, b: T): BOOLEAN;
(* == RETURN (a = b) *)

PROCEDURE Hash (a: T): Word.T;
(* == RETURN ORD (a) *)

END Boolean.












