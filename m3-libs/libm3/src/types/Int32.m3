(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Mar 25 10:07:28 PST 1994 by kalsow     *)
(*      modified on Tue Dec  1 08:38:28 PST 1992 by mcjones    *)
(*      modified on Thu Nov  2 21:55:27 1989 by muller         *)
(*      modified on Sun May  7 15:46:59 1989 by stolfi         *)

MODULE Int32;

IMPORT Word;

PROCEDURE Equal(a, b: T): BOOLEAN =
  BEGIN
    RETURN a = b
  END Equal;

PROCEDURE Hash(a: T): Word.T =
  BEGIN
    RETURN a
  END Hash;

PROCEDURE Compare(a, b: T): [-1..1] =
  BEGIN
    IF a < b THEN RETURN -1
    ELSIF a = b THEN RETURN 0
    ELSE RETURN 1
    END
  END Compare;

BEGIN
END Int32.

