(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Portions Copyright 1996-2000, Critical Mass, Inc.           *)
(* See file COPYRIGHT-CMASS for details.                       *)

(*  Modula-3 code generator interface

This interface in conjunction with M3CG_Ops provides an intermediate
language for compiler front ends to generate Modula-3 procedures
and object modules.   Its primary client is the Modula-3 compiler.
The bulk of this interface was derived (i.e. stolen) from "M2CG" by
John Ellis and John DeTreville.

The intermediate language (this interface) allows for different actual
code generators, providing two degrees of freedom:

    - multiple target architectures
    - code generators optimized for different goals (code quality
      versus fast turn-around)

The interface to the intermediate language is procedural: clients call
methods to construct intermediate-language programs.  Thus a
fast-turn-around code generator can compile the intermediate language
on the fly without much consing, while a high-quality slower code
generator may construct an entire intermediate-language module before
compiling it.

This interface defines a single object type, M3CG.T.  The operations
available on an M3CG.T are defined in M3CG_Ops.
*)

INTERFACE M3CG;

IMPORT Target, M3ID;

TYPE T <: ROOT; (* a code generator *)

TYPE
  Type = Target.CGType;
  MType = [ Type.Word8  .. Type.Addr  ];  (* "memory" types *)
  ZType = [ Type.Word32 .. Type.Addr  ];  (* "operator" types *)
  AType = [ Type.Word32 .. Type.XReel ];  (* "arithmetic" types *)
  IType = [ Type.Word32 .. Type.Int64 ];  (* "integer" operator types *)
  RType = [ Type.Reel   .. Type.XReel ];  (* "real" types *)
  (* The code generator manipulates scalar values of the types
     listed above.  The notation "sN.B" denotes the stack value
     that is "N" elements from the top of stack and has the type
     whose first letter is "B".  Only loads and stores manipulate
     MTypes, when loaded values are sign-extended or zero-extended
     as necessary to produce ZTypes.  *)

TYPE
  Sign = { Positive, Negative, Unknown };
  (* extra compile-time information for DIV and MOD *)

TYPE
  CompareOp = { EQ, NE, GT, GE, LT, LE };
  ConvertOp = { Round, Trunc, Floor, Ceiling };

CONST (*  A op B  ===  B SwappedCompare[op] A  *)
  SwappedCompare = ARRAY CompareOp OF CompareOp {
                     CompareOp.EQ, CompareOp.NE, CompareOp.LT,
                     CompareOp.LE, CompareOp.GT, CompareOp.GE };

CONST (*  A op B  ===  NOT (A NotCompare[op] B)  *)
  NotCompare = ARRAY CompareOp OF CompareOp {
                     CompareOp.NE, CompareOp.EQ, CompareOp.LE,
                     CompareOp.LT, CompareOp.GE, CompareOp.GT };

TYPE
  Name = M3ID.T;

TYPE
  Var    = BRANDED "M3CG.Var"  OBJECT END; (* represents a variable *)
  Proc   = BRANDED "M3CG.Proc" OBJECT END; (* represents a procedure *)

TYPE
  BitOffset  = INTEGER;  (* bit offset of a field *)
  BitSize    = CARDINAL; (* bit size of a memory reference or variable *)
  ByteOffset = INTEGER;  (* byte offset of a field *)
  ByteSize   = CARDINAL; (* byte size of a memory reference or variable *)
  Alignment  = CARDINAL; (* minimum byte alignment *)

TYPE
  Frequency = [0..100];
  (* estimated frequency that a branch will be taken or variable referenced. *)

CONST
  Never  : Frequency = FIRST (Frequency);
  Maybe  : Frequency = (Never + Always) DIV 2;
  Likely : Frequency = (Never + 8 * Always) DIV 10;
  Always : Frequency = LAST (Frequency);

TYPE
  TypeUID = BITS 32 FOR [-16_7fffffff-1 .. 16_7fffffff];
  (* a 32-bit unique id (fingerprint) for each type.  *)

TYPE
  Label = INTEGER;
  (* a unique value for each label.  The client is responsible for
     allocating these id's with the 'next_label' field declared below. *)

CONST
  No_label: Label = -1;

TYPE
  CallingConvention = Target.CallingConvention;

TYPE
  RuntimeError = {
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
     of RuntimeError.T used by the runtime system. *)

END M3CG.



