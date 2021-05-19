(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Type.i3                                               *)
(* Last Modified On Tue May 23 15:17:25 PDT 1995 by kalsow     *)
(*      Modified On Fri Dec 21 00:57:36 1990 by muller         *)

INTERFACE Type;

IMPORT M3, CG, Target;
FROM M3CG IMPORT QID, NoQID;

TYPE
  T          = M3.Type;
  Assumption = M3.EqAssumption;
  ModuleInfo <: REFANY;
  BitAlignT = [0 .. 255]; 

TYPE
  Class = { Error, Named, Integer, Longint, Real, Longreal, Extended,
            Array, Enum, Object, Opaque, OpenArray, Packed,
            Procedure, Record, Ref, Set, Subrange };

VAR
  recursionDepth: INTEGER := 0;
  (* incremented(decremented) every time the type checker enters(leaves)
     one of the types that's allowed to introduce recursions.
     (ie. REF, OBJECT, PROC) *)

TYPE
  Info = RECORD
    size      : INTEGER;  (* size in bits, -1 if variable sized *)
    min_size  : INTEGER;  (* minimum size in bits. *)
    alignment : INTEGER;  (* minimum alignment in bits *)
    addr_align: INTEGER := Target.Word8.align;
    (* ^When stk_type = CG.Type.Addr, the alignment of dereferenced location. *)
    hash      : INTEGER;  (* internal hash code *)
    name      := NoQID;   (* usually just one M3ID.T suffices and second could be in derived NamedType,
                           * but this form is easier *)
    stk_type  : CG.Type;  (* code generator representation on operator stack *)
    mem_type  : CG.Type;  (* code generator representation as a variable *)
    class     : Class;
    isTraced  : M3.Flag;
    isEmpty   : M3.Flag;
    isSolid   : M3.Flag;
    lazyAligned: M3.Flag;
  END;

(*** phase 0 ***)

PROCEDURE Initialize ();
PROCEDURE Reset ();
(* initializes the module and all other type modules. *)

(*** phase 1 ***)

PROCEDURE Parse (): T;
(* parse a type expression *)

PROCEDURE SetModule (x: ModuleInfo): ModuleInfo;
(* sets the current module's type info to 'x' and returns
   the previous module's type info.  This routine is only called
   when the "current" module changes. *)

(*** phase 2 ***)

PROCEDURE Check (t: T): T;
(* type check type 't'.   Return the underlying constructed
   (ie. class # Class.Named) type node. *)

PROCEDURE CheckInfo (t: T;  VAR(*OUT*) x: Info): T;
(* type check type 't'.  Return the underlying constructed
   (ie. class # Class.Named) type node and in 'x' its info. *)

PROCEDURE StraddleFreeScalars
  (t: T;  offs: INTEGER; IsEltOrField: BOOLEAN): BOOLEAN;
(* Returns TRUE iff no scalars within a value of type 't', located at
   a bit offset of 'offs' from a word boundary, cross word boundaries.  *)

PROCEDURE Strip (t: T): T;
(* return the constructed type of 't' (ie. strip renaming) *)

PROCEDURE StripPacked (t: T): T;
(* return the unpacked type of 't' (i.e., strip renaming and packing.) *)

PROCEDURE Base (t: T): T;
(* return the base type of 't' (strip renaming, packing & subranges) *)

PROCEDURE CGType (t: T;  in_memory: BOOLEAN := FALSE): CG.Type;
(* returns the code generator's representation for 't', either
   as a variable in memory or as an operand on the evaluation stack
   depending on 'in_memory' *)

PROCEDURE IsStructured (t: T): BOOLEAN;
(* Always represented as an address on the CG stack (record, array, or large set) *)
(* PRE: t need not be checked. *) 

PROCEDURE LoadScalar (t: T);
(* If 't' is not a structured type, generate code to load the scalar
   pointed to by the address on the CG stack *)

PROCEDURE IsLazyAligned (t: T): BOOLEAN;

PROCEDURE SetLazyAlignment (t: T; on: BOOLEAN);

PROCEDURE Typename (t: T; VAR typename: QID);

(*** phase 3 ***)

PROCEDURE BeginSetGlobals ();
(* Prepares the types of the current module for calls to SetGlobals *)

PROCEDURE SetGlobals (origin: INTEGER);
(* assign offsets to any needed global data for any types in the
   current module that occured lexically at or before 'origin'. *)

PROCEDURE IsOrdinal (t: T): BOOLEAN;
(* return TRUE if the type is an ordinal (Integer, Enum, Subrange) *)

PROCEDURE Number (t: T): Target.Int;
(* return the number of values of the type;  -1 if t is not an ordinal type *)

PROCEDURE GetBounds (t: T;  VAR min, max: Target.Int): BOOLEAN;
(* return the bounds and true for ordinal types, 
   [0,-1] and FALSE for non-ordinal types *)

PROCEDURE IsEqual (a, b: T;  x: Assumption): BOOLEAN;
(* TRUE iff (a == b) given the assumptions x *)

PROCEDURE IsSubtype (a, b: T): BOOLEAN;
(* TRUE iff (a <: b) *)

PROCEDURE IsAssignable (a, b: T): BOOLEAN;
(* TRUE iff (a := b) typechecks *)

PROCEDURE Name (t: T): TEXT;
PROCEDURE GlobalUID (t: T): INTEGER;
(* return the unique id for the type in the generated code *)

(*** phase 4 ***)

PROCEDURE CompileAll ();
(* compile all the local types for the current module *)

PROCEDURE Compile (t: T);
(* generates the debugging declarations for 't' *)

PROCEDURE AddCell (t: T);
(* Creates a typecell for t.  Normally, this done after Check
   time by Module.SetGlobals, but that only gets types created during
   the Check phase.  This is for types that can be only created later.
   In particular, see ArrayExpr.InnerPrep. *)

PROCEDURE LoadInfo (t: T;  offset: INTEGER;  addr := FALSE);
(* loads the specified field of 't's typecell.  If 'offset' is less than
   zero, 'LoadInfo' loads the address of the typecell.  *)

PROCEDURE InitValue (t: T;  zeroed: BOOLEAN);
(* initialize the variable addressed by s0.A to an arbitrary value of type 't'.
   If 'zeroed' the variable is assumed to already have all bits set to zero. *)

PROCEDURE Zero (t: T);
(* initialize the variable of type 't' addressed by s0.A to zeros. *)

PROCEDURE InitCost (t: T;  ifZeroed: BOOLEAN): INTEGER;
(* the cost of initializing a 't'.  (0 IFF no init required) *)

PROCEDURE GenMap (t: T;  offset, size: INTEGER;  refs_only: BOOLEAN);
(* emits the type map for type 't' occupying 'size' bits at 'offset'. *)

PROCEDURE GenDesc (t: T);
(* generate the runtime description for type 't' *)

PROCEDURE GenTag (t: T;  tag: TEXT;  offset: INTEGER);
(* generate a comment with 'tag' and 't's name *)

PROCEDURE LinkName (t: T;  tag: TEXT): TEXT;
(* Return a name for "t" that includes "tag" and will be unique
   to this compilation unit. *)

(*** phase 5 ***)

PROCEDURE GenCells (): INTEGER;
(* generate the current module's linked list of typecells
   and return its offset in the module global data. *)

PROCEDURE GenCellPtrs (): INTEGER;
(* generate the current module's linked list of pointers to typecells
   and return its offset in the module global data. *)

END Type.

(*
  The following sets of procedures may be called during the
  various phases of the compilation:

   initialization:
     { Initialize* }
   parsing:
     { Parse, NoteDeclaration }
   type checking:
     { Check, Number, GetBounds, Base, IsEqual,
       IsSubtype, IsAssignable }
   code generation:
     { Name, Number, GetBounds, Base, IsEqual,
       IsSubtype, IsAssignable }

   ( * => may only be called once )
*)
