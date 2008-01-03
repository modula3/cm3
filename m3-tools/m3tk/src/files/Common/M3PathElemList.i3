(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* "M3PathElemList" is an instantiation of the "List" interface
with an "M3PathElem.T". It provides the abstraction of a 
{\it search path} as a list of directories. *)

INTERFACE M3PathElemList = List(M3PathElem) 
END M3PathElemList.
