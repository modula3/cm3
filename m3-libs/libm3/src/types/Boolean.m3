(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Wed May 12 12:16:52 PDT 1993 by swart          *)
(*      modified on Thu Apr 22 09:57:07 PDT 1993 by mcjones        *)
(*      modified on Thu Nov  2 21:55:31 1989 by muller         *)
(*      modified on Fri Sep 29 15:30:35 1989 by kalsow         *)
(*      modified on Sun May  7 15:46:59 1989 by stolfi         *)

MODULE Boolean;

IMPORT Word;

PROCEDURE Compare (a, b: T): [-1..1] =
  BEGIN
    RETURN ORD (a) - ORD (b);
  END Compare;

PROCEDURE Equal (a, b: T): BOOLEAN =
  BEGIN
    RETURN (a = b);
  END Equal;

PROCEDURE Hash (a: T): Word.T =
  BEGIN
    RETURN ORD (a);
  END Hash;

BEGIN
END Boolean.

