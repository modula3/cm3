(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3PathDefaults;

IMPORT M3PathElem, M3PathElemList;

(* This interface provides support for prepending the current directory
to an "M3PathElemList.T" *) 

PROCEDURE EnsureCurrentFirst(l: M3PathElemList.T): M3PathElemList.T;
(* Ensure that the current working directory appears first in the
returned list. If it already occurs in "l" and is not at the front
it is moved to the front. *)

PROCEDURE Add(l: M3PathElemList.T; d: M3PathElem.T): M3PathElemList.T;
(* If "d" does not occur in "l", where the comparison is via 
"M3PathElem.Equal", add it to the end of "l" and return the new list. *)

END M3PathDefaults.


