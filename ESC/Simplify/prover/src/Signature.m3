(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Feb 18 11:18:16 PST 1993 by detlefs                  *)

MODULE Signature;

IMPORT Word;

PROCEDURE Hash(READONLY k: T): Word.T =
  BEGIN RETURN k.n + k.m END Hash;

PROCEDURE Equal(READONLY k1, k2: T): BOOLEAN =
  BEGIN RETURN k1.n = k2.n AND k1.m = k2.m END Equal;

BEGIN
END Signature.

