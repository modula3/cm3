(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Wed May 12 12:18:54 PDT 1993 by swart          *)
(*      modified on Thu Nov  2 18:28:30 1989 by muller         *)
(*      modified on Mon Oct  2 09:13:49 1989 by kalsow         *)
(*      modified on Tue Mar  3 18:14:23 PST 1987 by luca       *)

MODULE Axis;

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
END Axis.
