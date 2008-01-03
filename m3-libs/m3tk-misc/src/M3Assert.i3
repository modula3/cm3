(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3Assert;

(* The "Check" procedure is similar to the <*ASSERT*> pragma, except
that, unlike the latter, it cannot be ignored by a compiler. "Fail"
cause a program to output the message "Assertion failed" and then
terminate. *)

PROCEDURE Check(assertion: BOOLEAN);
(* = IF NOT assertion THEN Fail() END *)

PROCEDURE Fail();

END M3Assert.
