(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue May 23 07:40:05 PDT 1995 by kalsow     *)


INTERFACE CharMap;

PROCEDURE CmpText (t, u: TEXT): [-1 .. +1];
(* Returns "SortOrder[t] - SortOrder[u]". *)

PROCEDURE Substr (a, b: TEXT): BOOLEAN;
(* Returns "TRUE" if "a" contains "b" as a substring when case is ignored. *)

PROCEDURE PrefixMatch (a, b: TEXT;  len: INTEGER): BOOLEAN;
(* Returns "TRUE" if the first "len" characters of "a" and "b" are
   equal when case is ignored. *)

END CharMap.
