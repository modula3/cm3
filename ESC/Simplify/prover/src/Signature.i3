(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Oct 30 21:25:27 PST 1994 by detlefs                  *)

INTERFACE Signature;

(* Exists to be used as a generic argument. *)

IMPORT Word;

TYPE T = RECORD n, m: INTEGER END (* RECORD *);

CONST Brand = "Signature";

PROCEDURE Hash(READONLY k: T): Word.T;

PROCEDURE Equal(READONLY k1, k2: T): BOOLEAN;

END Signature.
