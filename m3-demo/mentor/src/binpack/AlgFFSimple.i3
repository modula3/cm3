(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Mon May 23 21:13:53 PDT 1994 by najork *)
(*      modified on Tue Jul 21 05:33:15 1992 by mhb *)

INTERFACE AlgFFSimple;

IMPORT BinpackAlgClass;

(* First-first binpacking. Doesn't respond to any feedback events. *)

TYPE T <: BinpackAlgClass.T;

END AlgFFSimple.

