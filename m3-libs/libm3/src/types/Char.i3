(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Sep 15 11:00:15 PDT 1994 by heydon         *)
(*      modified on Wed May 12 12:08:29 PDT 1993 by swart          *)
(*      modified on Thu Apr 22 09:56:55 PDT 1993 by mcjones        *)
(*      modified on Thu Nov  2 21:55:29 1989 by muller         *)
(*      modified on Fri Sep 29 15:46:46 1989 by kalsow         *)
(*      modified on Fri Jan 20 10:02:29 PST 1989 by glassman   *)
(*      modified on Wed May 27 23:11:56 1987 by mbrown         *)
(*      modified Mon May 13 20:11:50 1985 by Ellis             *)


(* A "Char.T" is an "CHAR".  This interface is intended to be
   used as an actual interface for generic interfaces and modules such
   as "Table". *)

INTERFACE Char;

IMPORT Word;

TYPE T = CHAR;

CONST Brand = "Char";

PROCEDURE Compare (a, b: T): [-1..1];
(* == RETURN (a - b) *)

PROCEDURE Equal (a, b: T): BOOLEAN;
(* == RETURN (a = b) *)

PROCEDURE Hash (a: T): Word.T;
(* == RETURN ORD (a) *)

END Char.
