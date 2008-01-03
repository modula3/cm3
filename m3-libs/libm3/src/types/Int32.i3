(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Sep 15 11:00:01 PDT 1994 by heydon     *)
(*      modified on Fri Mar 25 10:07:00 PST 1994 by kalsow     *)
(*      modified on Tue Aug 31 15:21:37 PDT 1993 by mcjones    *)
(*      modified on Thu Nov  2 21:55:28 1989 by muller         *)
(*      modified on Sun May  7 15:35:59 1989 by stolfi         *)

(* An "Int32.T" is a 32-bit integer whether compiled on 32-bit or
   64-bit machines.  This interface can be used to instantiate
   generic interfaces and modules such as "Table" and "List". *)

INTERFACE Int32;

IMPORT Word;

TYPE T = BITS 32 FOR [ -16_7fffffff-1 .. 16_7fffffff ];

CONST Brand = "Int32";

PROCEDURE Equal(a, b: T): BOOLEAN;
(* Return "a = b". *)

PROCEDURE Hash(a: T): Word.T;
(* Return "a". *)

PROCEDURE Compare(a, b: T): [-1..1];
(* Return "-1" if "a < b", "0" if "a = b", or "+1" if "a > b". *)

END Int32.

