(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Sep 15 10:59:22 PDT 1994 by heydon     *)
(*      modified on Tue Aug 31 15:21:33 PDT 1993 by mcjones    *)
(*      modified on Mon Dec 30 09:33:31 PST 1991 by kalsow     *)
(*      modified on Thu Nov  2 21:55:24 1989 by muller         *)
(*      modified on Sun May  7 15:35:59 1989 by stolfi         *)

(* A "Refany.T" is a "REFANY".  This interface is intended to be used
   to instantiate generic interfaces and modules such as "Table" and
   "List". *)

INTERFACE Refany;

IMPORT Word;

TYPE T = REFANY;

CONST Brand = "Refany";

PROCEDURE Equal(r1, r2: T): BOOLEAN;
(* Return "r1 = r2". *)

PROCEDURE Hash(r: T): Word.T;
(* Cause a checked runtime error. *)

PROCEDURE Compare(r1, r2: T): [-1..1];
(* Cause a checked runtime error. *)


END Refany.
