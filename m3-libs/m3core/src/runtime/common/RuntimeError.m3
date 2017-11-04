(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE RuntimeError;

IMPORT RT0, Compiler;

VAR self: RT0.ExceptionPtr := NIL;

PROCEDURE Self (): RT0.ExceptionPtr =
  BEGIN
    IF (self = NIL) THEN
      TRY
        RAISE E (T.Unknown);
      EXCEPT E =>
        self := LOOPHOLE (Compiler.ThisException(), RT0.ActivationPtr).exception;
      END;
    END;
    RETURN self;
  END Self;

CONST
  Msg = ARRAY T OF TEXT {
    "<*ASSERT*> failed.",
    "An enumeration or subrange value was out of range.",
    "An array subscript was out of range.",
    "An open array had the wrong shape.",
    "Attempt to reference an illegal memory location.",
    "An explicit or implicit NARROW() operation failed.",
    "A function failed to return a value.",
    "No handler specified for the current CASE value.",
    "No handler specified for the current type in a TYPECASE statement.",
    "A thread stack overflowed.",
    "NEW() was unable to allocate more memory.",
    "Attempted to compute the address of an unaligned value.",
    "An exception was raised, but not handled.",
    "An exception was blocked by a RAISES clause.",
    "Integer result too large to represent.",
    "Attempt to DIV or MOD by zero.",
    "Attempted floating-point division by zero.",
    "Floating-point result is too large to represent.",
    "Floating-point result is too small to represent.",
    "Floating-point result is inexact.",
    "Invalid floating-point number.",
    "Two types were defined with the same brand.",
    "A compile-time type is missing.",
    "The supertypes of an object form a cycle.",
    "Multiple full revelations of an opaque type.",
    "Partial revelations don't match type declarations.",
    "A NIL-valued method was invoked.",
    "A privileged machine instruction was attempted.",
    "A low-level OS or machine error occurred.",
    "A runtime error occurred."
  };

PROCEDURE Tag (t: T): TEXT =
  BEGIN
    RETURN Msg[t];
  END Tag;

PROCEDURE Raise (t: T) =
  BEGIN
    RAISE E(t);
  END Raise;

BEGIN
END RuntimeError.
