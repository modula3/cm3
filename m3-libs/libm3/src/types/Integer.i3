(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Sep 15 10:59:45 PDT 1994 by heydon     *)
(*      modified on Tue Aug 31 15:21:37 PDT 1993 by mcjones    *)
(*      modified on Thu Nov  2 21:55:28 1989 by muller         *)
(*      modified on Fri Sep 29 15:44:47 1989 by kalsow         *)
(*      modified on Sun May  7 15:35:59 1989 by stolfi         *)

(* An "Integer.T" is an "INTEGER".  This interface is intended to be
   used to instantiate generic interfaces and modules such as "Table"
   and "List". *)

INTERFACE Integer;

IMPORT Word;

TYPE T = INTEGER;

CONST Brand = "Integer";

PROCEDURE Equal(a, b: T): BOOLEAN;
(* Return "a = b". *)

PROCEDURE Hash(a: T): Word.T;
(* Return "a". *)

PROCEDURE Compare(a, b: T): [-1..1];
(* Return "-1" if "a < b", "0" if "a = b", or "+1" if "a > b". *)

END Integer.

