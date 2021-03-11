(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved.  *)
(* See file COPYRIGHT-CMASS for details. *)

(* hack for PM3, not actually used, but makes things compile *)

(*
   "RuntimeError" defines the exception raised by the compiler and
   runtime system for the various checked runtime errors defined by
   the language.
*)

INTERFACE RuntimeError;

IMPORT RT0;

EXCEPTION E (T);
(* Because "E" is implicit, the compiler does not warn about
   where it might occur and the runtime allows it to escape
   from procedures that don't explicitly list it in their
   RAISES{} clauses. *)

TYPE
  T = {
    AssertFailed,           (* <*ASSERT*> condition was FALSE. *)
    ValueOutOfRange,        (* An enumeration or subrange value was out of range. *)
    SubscriptOutOfRange,    (* An array subscript was out of range. *)
    IncompatibleArrayShape, (* open array had the wrong shape *)
    BadMemoryReference,     (* attempted to reference a bad location in memory. *)
    NarrowFailed,           (* An explicit or implicit NARROW() operation failed. *)
    NoReturnValue,          (* A function failed to return a value. *)
    UnhandledCase,          (* no handler for the current CASE value. *)
    UnhandledTypecase,      (* no handler for the given type in a TYPECASE stmt. *)
    StackOverflow,          (* A thread stack overflowed *)
    OutOfMemory,            (* NEW() was unable to allocate more memory. *)
    UnalignedAddress,       (* attempted to get the address of an unaligned value *)
    UnhandledException,     (* An exception was raised, but not handled. *)
    BlockedException,       (* An exception was blocked by a RAISES clause. *)
    IntegerOverflow,        (* integer result too large to represent *)
    IntegerDivideByZero,    (* Attempt to DIV or MOD by zero. *)
    FloatDivideByZero,      (* Attempted floating-point division by zero. *)
    FloatOverflow,          (* floating-point result too large to represent *)
    FloatUnderflow,         (* floating-point result too small to represent *)
    FloatInexact,           (* floating-point result is inexact *)
    FloatInvalid,           (* invalid floating-point argument *)
    DuplicateBrand,         (* two types with the same brand exist *)
    MissingType,            (* a compile-time type is missing at link time *)
    SupertypeCycle,         (* supertypes of an object form a cycle *)
    OpaqueTypeRedefined,    (* multiple full revelations of an opaque type *)
    InconsistentRevelation, (* partial revelations don't match type declarations *)
    UndefinedMethod,        (* a NIL-valued method was invoked *)
    PrivilegedInstruction,  (* a privileged machine instruction was attempted *)
    SystemError,            (* a low-level OS or machine error occurred *)
    Unknown
  };
  (* NOTE: This enumeration must be kept in synch with the version
     of M3CG.RuntimeError used by the compiler. *)

CONST Tag : PROCEDURE (t: T): TEXT = NIL;
(* Returns a message describing "t". *)

END RuntimeError.




