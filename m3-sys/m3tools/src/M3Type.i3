(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

INTERFACE M3Type;

IMPORT M3ID, M3AST, Target;

TYPE
  T <: ROOT;

  Info = RECORD
    size      : INTEGER;  (* preferred bit size  (< 0 for open arrays) *)
    min_size  : INTEGER;  (* minimum size that can hold the type *)
    alignment : INTEGER;  (* alignment in bits *)
    class     : Class;    (* top-level classification *)
    is_traced : BOOLEAN;  (* TRUE => is or contains a traced reference *)
    is_empty  : BOOLEAN;  (* TRUE => no legal values *)
    is_solid  : BOOLEAN;  (* TRUE => no padding or non-legal values *)
    err_msg   : TEXT;     (* class = Unknown => possible error *)
  END;

  Class = { Unknown, Integer, Real, Longreal, Extended,
            Array, Enum, Object, Opaque, OpenArray, Packed,
            Procedure, Record, Ref, Set, Subrange };

TYPE
  Array <: T OBJECT
    index   : T;
    element : T;
  END;

  Enum <: T OBJECT
    elements : REF ARRAY OF M3ID.T;
  END;

  Object <: T OBJECT
    brand     : TEXT;
    super     : T;
    fields    : REF ARRAY OF FieldDesc;
    methods   : REF ARRAY OF MethodDesc;
    overrides : REF ARRAY OF MethodDesc;
  END;

  Opaque <: T OBJECT
    super : T;
  END;

  OpenArray <: T OBJECT
    element : T;
  END;

  Packed <: T OBJECT
    bits    : INTEGER;
    element : T;
  END;

  Procedure <: T OBJECT
    formals     : REF ARRAY OF FormalDesc;
    return      : T;
    raises      : REF ARRAY OF ExceptDesc;
    callingConv : Target.CallingConvention;
  END;

  Record <: T OBJECT
    fields : REF ARRAY OF FieldDesc;
  END;

  Ref <: T OBJECT
    brand  : TEXT;
    target : T;
    traced : BOOLEAN;
  END;

  Set <: T OBJECT
    domain : T;
  END;

  Subrange <: T OBJECT
    min   : Target.Int;
    max   : Target.Int;
    super : T;
  END;

VAR(*READONLY*)  (* builtin types *)
  Integer, Cardinal         : T;
  Real, LongReal, Extended  : T;
  Root, UntracedRoot        : T;
  Refany, Address, Null     : T;
  Boolean, Char             : T;
  Mutex, Txt                : T;

TYPE
  FieldDesc = RECORD
    name    : M3ID.T;
    type    : T;
    default : Constant;
  END;

  Mode = { Value, Var, Readonly };

  MethodDesc = RECORD
    name      : M3ID.T;
    signature : Procedure;
    default   : Constant;
  END;

  FormalDesc = RECORD
    name    : M3ID.T;
    type    : T;
    mode    : Mode;
    default : Constant;
  END;

  ExceptDesc = RECORD
    ast  : M3AST.T;
    decl : M3AST.NodeIndex;
  END; (* { NIL, 0 } == ANY *)

TYPE
  Constant = RECORD
    (* not needed yet... *)
  END;

(*------------------------------------------------------ public methods ---*)

PROCEDURE GetInfo (t: T;  VAR(*OUT*) x: Info);
(* return the various info values for 't'. *)

PROCEDURE Base (t: T): T;
(* return the base type of 't' (strip renaming, packing & subranges) *)

PROCEDURE IsOrdinal (t: T): BOOLEAN;
(* return TRUE if the type is an ordinal (Integer, Enum, Subrange) *)

PROCEDURE Number (t: T): Target.Int;
(* return the number of values of the type;  -1 if t is not an ordinal type *)

PROCEDURE GetBounds (t: T;  VAR min, max: Target.Int): BOOLEAN;
(* return the bounds and true for ordinal types, 
   [0,-1] and FALSE for non-ordinal types *)

PROCEDURE IsEqual (a, b: T): BOOLEAN;
(* TRUE iff (a == b)  !!! NOTE: only does trivial comparisons so far. !!! *)

(************************* NOT IMPLEMENTED *******************************

PROCEDURE IsSubtype (a, b: T): BOOLEAN;
(* TRUE iff (a <: b) *)

PROCEDURE IsAssignable (a, b: T;  safe: BOOLEAN): BOOLEAN;
(* TRUE iff (a := b) typechecks in a module with safety "safe". *)

*************************************************************************)

END M3Type.
