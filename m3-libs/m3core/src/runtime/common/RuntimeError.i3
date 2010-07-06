(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved.  *)
(* See file COPYRIGHT-CMASS for details. *)

(*
   "RuntimeError" defines the exception raised by the compiler and
   runtime system for the various checked runtime errors defined by
   the language.
*)

INTERFACE RuntimeError;

IMPORT RT0;

<*IMPLICIT*>
EXCEPTION E (T);
(* Because "E" is implicit, the compiler does not warn about
   where it might occur and the runtime allows it to escape
   from procedures that don't explicitly list it in their
   RAISES{} clauses. *)

TYPE
  T = {
    AssertFailed,           (* 0 <*ASSERT*> condition was FALSE. *)
    ValueOutOfRange,        (* 1 An enumeration or subrange value was out of range. *)
    SubscriptOutOfRange,    (* 2 An array subscript was out of range. *)
    IncompatibleArrayShape, (* 3 open array had the wrong shape *)
    BadMemoryReference,     (* 4 attempted to reference a bad location in memory. *)
    NarrowFailed,           (* 5 An explicit or implicit NARROW() operation failed. *)
    NoReturnValue,          (* 6 A function failed to return a value. *)
    UnhandledCase,          (* 7 no handler for the current CASE value. *)
    UnhandledTypecase,      (* 8 no handler for the given type in a TYPECASE stmt. *)
    StackOverflow,          (* 9 A thread stack overflowed *)
    OutOfMemory,            (* 10 NEW() was unable to allocate more memory. *)
    UnalignedAddress,       (* 11 attempted to get the address of an unaligned value *)
    UnhandledException,     (* 12 An exception was raised, but not handled. *)
    BlockedException,       (* 13 An exception was blocked by a RAISES clause. *)
    IntegerOverflow,        (* 14 integer result too large to represent *)
    IntegerDivideByZero,    (* 15 Attempt to DIV or MOD by zero. *)
    FloatDivideByZero,      (* 16 Attempted floating-point division by zero. *)
    FloatOverflow,          (* 17 floating-point result too large to represent *)
    FloatUnderflow,         (* 18 floating-point result too small to represent *)
    FloatInexact,           (* 19 floating-point result is inexact *)
    FloatInvalid,           (* 20 invalid floating-point argument *)
    DuplicateBrand,         (* 21 two types with the same brand exist *)
    MissingType,            (* 22 a compile-time type is missing at link time *)
    SupertypeCycle,         (* 23 supertypes of an object form a cycle *)
    OpaqueTypeRedefined,    (* 24 multiple full revelations of an opaque type *)
    InconsistentRevelation, (* 25 partial revelations don't match type declarations *)
    UndefinedMethod,        (* 26 a NIL-valued method was invoked *)
    PrivilegedInstruction,  (* 27 a privileged machine instruction was attempted *)
    SystemError,            (* 28 a low-level OS or machine error occurred *)
    Unknown                 (* 29 *)
  (* NOTE: going past 31 requires backend and runtime change *)
  (* NOTE: This enumeration must be kept in synch with the version
     of M3CG.RuntimeError used by the compiler. *)
  };

PROCEDURE Self (): RT0.ExceptionPtr;
(* Returns the runtime representation of "E" *)

PROCEDURE Tag (t: T): TEXT;
(* Returns a message describing "t". *)

PROCEDURE Raise (t: T);
(* RAISE(t). *)

END RuntimeError.
