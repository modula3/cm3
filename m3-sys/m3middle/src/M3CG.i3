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

TYPE UnitKind = { Interface, Module };

(* Changes to the compiler intermediate representation have to be
   consistified in *many* places.  To help with compatibility problems,
   here is a versioning system.  Through CIV_WC_size, there was no
   explicit versioning.  Starting with CIV_ext1, the version can 
   optionally be placed into the IR.
*) 
   
TYPE CM3_IR_Version = INTEGER;
CONST
  CIV_SRC = 0;
  CIV_PM3 = 1;
  CIV_CM3 = 2;        (* The earliest to be vetted. From Critical Mass? *) 
  CIV_AtomicOps1 = 3; (* Jun 22, 2007. d099f0b. *) 
  CIV_AtomicOps2 = 4; (* Feb 17, 2009. e66950a. *) 
  CIV_AtomicOps3 = 5; (* Feb 3,  2010. 56e677d. *) 
  CIV_extract = 6;    (* Mar 10, 2010. b352ffc. *) 
  CIV_pop_struct = 7; (* Aug 27, 2010. 6ee95af. *) 
  CIV_RT_hook = 8;    (* Aug 27, 2010. dda2a77. *) 
  CIV_WC_size = 9;    (* Feb 26, 2014. 655eeac. *) 
  CIV_ext1 = 10;      (*  *) 

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
     abbreviated by "B".  Only loads and stores manipulate
     MTypes.  When loaded, values are sign-extended or zero-extended
     as necessary to produce ZTypes.  *)

TYPE
  Sign = { Positive, Negative, Unknown };
  (* extra compile-time information for DIV and MOD *)

TYPE
  CompareOp = { EQ, NE, GT, GE, LT, LE };
  ConvertOp = { Round, Trunc, Floor, Ceiling };

TYPE
  MemoryOrder = { Relaxed, Release, Acquire, AcquireRelease, Sequential };
  AtomicOp = { Add, Sub, Or, And, Xor };

CONST (*  A op B  ===  B SwappedCompare[op] A  *)
  SwappedCompare = ARRAY CompareOp OF CompareOp {
                     CompareOp.EQ, CompareOp.NE, CompareOp.LT,
                     CompareOp.LE, CompareOp.GT, CompareOp.GE };

CONST (*  A op B  ===  NOT (A NotCompare[op] B)  *)
  NotCompare = ARRAY CompareOp OF CompareOp {
                     CompareOp.NE, CompareOp.EQ, CompareOp.LE,
                     CompareOp.LT, CompareOp.GE, CompareOp.GT };

TYPE
  Name = M3ID.T; (* Numbering of a simple identifier. *) 

  (* an optionally module qualified name (qualified identifier) *)
  QID = RECORD
    module := M3ID.NoID;
    item := M3ID.NoID;
  END;

  CONST NoQID = QID {M3ID.NoID, M3ID.NoID};

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
  TypeUID = (* BITS 32 FOR *) [-16_7fffffff-1 .. 16_7fffffff];
    (* ^A 32-bit unique id: a fingerprint or hash of the type's structure.  *)
  TypeUIDBits = BITS 32 FOR TypeUID;

CONST NO_UID : TypeUID = -1;
(* TODO: There are duplicate declarations of NO_UID in m3front/src/type/TypeRep.i3,
         m3front/src/misc/M3String.m3, and m3front/src/misc/M3WString.m3. 
         Also, m3cc/gcc*/gcc/m3cg/parse.c contain C declarations, and
         cm3ide/src/nodes/Type.i3 contains a declaration with zero as value. 
         Consistify and centralize this.
*)  

PROCEDURE FormatUID(tUID: TypeUID) : TEXT;

TYPE
  Label = INTEGER;
  (* A unique numbering for each label.  The client is responsible for
     allocating these id's with the 'next_label' method of M3CG_OpsPublic. *)

CONST
  No_label: Label = -1;

TYPE
  CallingConvention = Target.CallingConvention;

TYPE
  RuntimeError = {
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
     of RuntimeError.T used by the runtime system. *)
  };

END M3CG.
