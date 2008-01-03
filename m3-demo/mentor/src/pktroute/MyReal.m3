(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Fri Jul 30 14:05:23 PDT 1993 by heydon                   *)

MODULE MyReal;

PROCEDURE Compare(a, b: T): [-1..1] =
  BEGIN
    IF a < b THEN RETURN -1
    ELSIF a > b THEN RETURN 1
    END;
    RETURN 0
  END Compare;

BEGIN
END MyReal.
