(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Thu Jan  5 23:18:02 PST 1995 by najork                   *)
(*      modified on Fri Jul 30 13:57:17 PDT 1993 by heydon                   *)

INTERFACE MyReal;

CONST Brand = "MyReal";

TYPE
  T = REAL;

PROCEDURE Compare(a, b: T): [-1..1];
(* Returns "-1" if "a < b", "0" if "a = b", and "+1" if "a > b". *)

END MyReal.
