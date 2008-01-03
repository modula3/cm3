(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue May 11 16:42:59 PDT 1993 by swart          *)
(*      modified on Thu Apr 22 09:56:56 PDT 1993 by mcjones        *)
(*      modified on Thu Nov  2 21:55:28 1989 by muller         *)
(*      modified on Fri Sep 29 15:43:49 1989 by kalsow         *)
(*      modified on Wed May 27 23:11:56 1987 by mbrown         *)

MODULE Char;

IMPORT Word;

PROCEDURE Compare (a, b: T): [-1..1] =
  BEGIN
    IF a < b THEN RETURN -1; END;
    IF a > b THEN RETURN 1; END;
    RETURN 0;
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
END Char.
