(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Feb 28 20:21:20 PST 1992 by stolfi     *)
(*      modified on Wed Sep 25 00:05:24 1991 by kalsow         *)
(*      modified on Thu Nov  2 21:55:26 1989 by muller         *)

UNSAFE MODULE LongReal;

IMPORT Word, Int32;

PROCEDURE Equal(a, b: T): BOOLEAN =
  BEGIN RETURN a = b END Equal;


CONST Int32Sz = BYTESIZE(T) DIV BYTESIZE(Int32.T);

TYPE Int32Arr = ARRAY [0..Int32Sz-1] OF Int32.T;
  
PROCEDURE Hash(a: T): Word.T =
  VAR arr := LOOPHOLE(a, Int32Arr); res := 0; BEGIN
    FOR i := 0 TO LAST(arr) DO
      res := Word.Xor(res, Int32.Hash(arr[i]))
    END (* FOR *);
    RETURN res
  END Hash;

PROCEDURE Compare(a, b: T): [-1..1] =
  BEGIN
    IF a < b THEN RETURN -1
    ELSIF a > b THEN RETURN 1
    ELSE RETURN 0
    END (* IF *)
  END Compare;

BEGIN
END LongReal.
