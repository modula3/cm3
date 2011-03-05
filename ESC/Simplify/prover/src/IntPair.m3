(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Apr 17 16:19:28 PDT 1996 by detlefs                  *)

MODULE IntPair;

IMPORT Integer, Word;

PROCEDURE Compare(p1, p2: T): [-1..1] =
  BEGIN RETURN Integer.Compare(p1.i, p2.i) END Compare;

PROCEDURE CompareJ(p1, p2: T): [-1..1] =
  BEGIN RETURN Integer.Compare(p1.j, p2.j) END CompareJ;

PROCEDURE Equal(ip1, ip2: T): BOOLEAN =
  BEGIN RETURN ip1.i = ip2.i AND ip1.j = ip2.j
  END Equal;

PROCEDURE Hash(ip: T): Word.T =
  BEGIN RETURN Word.Xor(Word.Not(ip.i), ip.j)
  END Hash;


BEGIN
END IntPair.

