(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE WideChar;

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
END WideChar.
