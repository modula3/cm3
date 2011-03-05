(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Apr 17 16:17:53 PDT 1996 by detlefs                  *)

INTERFACE IntPair;

(* A pair of integers.  Useful as the element type of an array, for sorting
   purposes, or as a generic argument. *)

IMPORT Word;

TYPE T = RECORD i, j: INTEGER END (* RECORD *);

CONST Brand = "IntPair";

PROCEDURE Compare(p1, p2: T): [-1..1];
(* Returns "Integer.Compare(p1.i, p2.i)" *)

PROCEDURE CompareJ(p1, p2: T): [-1..1];
(* Returns "Integer.Compare(p1.j, p2.j)" *)

PROCEDURE Equal(ip1, ip2: T): BOOLEAN;
(* Return whether "ip1" and "ip2" are the same. *)

PROCEDURE Hash(ip: T): Word.T;
(* Return a hash function of "ip". *)

END IntPair.

