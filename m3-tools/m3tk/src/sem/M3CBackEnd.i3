INTERFACE M3CBackEnd;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

IMPORT Text;

IMPORT M3AST_AS, M3AST_SM;

IMPORT M3CStdProcs, M3CWordProcs;

(* This module defines the interface for the information required by
the compiler front-end from a back-end . In order to support multiple
back-ends simultaneously, the interface is in terms of procedure
variables, which must be assigned by an appropriate implementation. *)

TYPE
  NumStatus = {Valid,         (* computed an ok value *)
               Unknown,       (* input error, no computation possible *)
               Overflow};     (* overflow on computation *)

(* The implementor of these procedures can usually assume that appropriate
error checks/computations have already been performed before a procedure is
called. The big exception to this rule is that type checking has not usually
been done; type checking can require constants to be evaluated so the back end
is called to evaluate them before type checking is done.
  Any other checks or computations which are left to these back end procedures
are given in the comments describing the procedure. *)

TYPE
  LiteralValueProc =
    PROCEDURE (lit: M3AST_AS.EXP;
        VAR (*out*) er: M3AST_SM.Exp_value): NumStatus RAISES {};
  (* Compute the value of the given literal. *)

  ConstructorValueProc = 
    PROCEDURE (cons: M3AST_AS.Constructor;
        VAR (*out*) er: M3AST_SM.Exp_value): NumStatus RAISES {};
  (* Ditto for constructors.  The value of 'sm_actual_s' can be relied upon, as
  can the fact that all values of the components have been computed (which
  implies that no types are unset).
    The back end is expected to do all the work for set constructors and
  operations - i.e. it must provide a 'ConstructorValueProc' which will build
  something representing the set and must be prepared to do set operations e.g.
  '+' and 'IN' on such values. The set type is guaranteed to have valid bounds
  and all elements on the 'sm_actual_s' list are guaranteed to be ordinal and
  in range.
    Array and record constructors are easier; most of the work can be done in
  a portable way by the front-end. The only operation the front end will ever
  require on an array or record constructor value is the one specified by the
  'ConstructorOriginalProc' given below. *)

  ConstructorOriginalProc =
      PROCEDURE(e: M3AST_SM.Exp_value): M3AST_AS.Constructor RAISES {};
  (* 'e' is guaranteed to be an array or record constructor value; returns the
  original constructor from which the value was derived.
    The front end uses this constructor and its 'sm_actuals_s' list to do
  selection, indexing and testing for (in)equality which are the only
  operations allowed on constructors in constant expressions. *)

  IsOrdinalProc = PROCEDURE(e: M3AST_SM.Exp_value): BOOLEAN RAISES {};
  (* Returns TRUE if 'e' represents an ordinal value, FALSE otherwise. 'e' may
  be NIL in which case FALSE should be returned. An 'IsOrdinalProc' is
  permitted to be lax and classify 'e' as ordinal even if it is not; a result
  of TRUE indicates that 'e' looks ordinal to the back end and that the back
  end will be happy to do ordinal style operations upon it *)

  ValProc =
    PROCEDURE (n: INTEGER; ts: M3AST_AS.TYPE_SPEC;
        VAR (*out*) er: M3AST_SM.Exp_value): NumStatus RAISES {};
  (* Return an 'Exp_value' representing the member of an ordinal type
  corresponding to the integer 'n'.
    This is not always the 'n'th member of the ordinal type; for example if
  'ts' represented the subrange [2..3] and 'n' was 2 the 'Exp_value' returned
  would represent the first member of the subrange; 'n' is absolute, not
  relative to the first value of the given type.
    Enumerations are considered to be numbered from 0 upward e.g. if 'ts'
  represented the enumeration Animal = {Pig,Cow,Sheep} and 'n' was 1 the
  'Exp_value' returned would represent the member 'Cow'. If 'n' was 1 and 'ts'
  was the subrange [Animal.Cow..Animal.Sheep] the 'Exp_value' returned would
  still represent 'Cow'.
    'ts' is guaranteed to be ordinal and 'n' is guaranteed to be in the range
  of integer values which correspond to members of 'ts'. 'er' should not be
  updated unless 'NumStatus.Valid' is returned. *)

  OrdProc =
    PROCEDURE(e: M3AST_SM.Exp_value;
        VAR (*out*) i: INTEGER): NumStatus RAISES {};
  (* Return the integer value corresponding to 'e'. Barring overflow 'OrdProc'
  is the inverse of 'ValProc' e.g. given 'Ord' of type 'OrdProc' and 'Val' of
  type 'ValProc':
    IF Val(n, someType, e) = NumStatus.Valid AND
        Ord(e, nn) = NumStatus.Valid THEN
      (* Assert: n = nn *)
    END;
  'e' is guaranteed to be an ordinal value. 'i' should be updated only if
  'NumStatus.Valid' is returned. *)

  ConvertOrdinalProc = PROCEDURE(e: M3AST_SM.Exp_value; ts: M3AST_AS.TYPE_SPEC;
      VAR (* out *) er: M3AST_SM.Exp_value): NumStatus RAISES {};
  (* Convert the given ordinal expression 'e' into an ordinal expression with
  the same value but the given type, 'ts'. 'e' is guaranteed to be ordinal and
  'ts' is guaranteed to be an ordinal type.
    This has a similar effect to using 'OrdProc' then 'ValProc' but it avoids
  the conversion to an intermediate integer *)

  BinaryOpProc = 
    PROCEDURE (op: M3AST_AS.BINARY; 
        el: M3AST_SM.Exp_value; e2: M3AST_SM.Exp_value;
        VAR (*out*) er: M3AST_SM.Exp_value): NumStatus RAISES {};
  (* Set 'er' to the value of 'e1' 'op' 'e2'.
   Assert: op # In (below).
   Assert: e1 is not a record or array constructor value
   WARNING: No type checking has been done at the stage (because type checking
  may depend on expression values which are not yet computed) hence there is
  no guarantee that 'e1' and 'e2' will be compatible and suitable for the
  given operation. The only guarantee is that both will be non NIL.
    Typically the start of the 'BinaryOpProc' will be a check that the values
  are at least compatible enough for the back end to work with. The check is
  free to relax the type rules (indeed it will have to as they cannot be
  enforced at this point). The only constraint is that the check must accept
  anything that is legal in the language.
     'er' should not be updated unless 'NumStatus.Valid' is returned. *)

  InOpProc =
    PROCEDURE (e1, e2: M3AST_SM.Exp_value;
        VAR (*out*) er: M3AST_SM.Exp_value): NumStatus RAISES {};
  (* 'e1' can be relied on to be ordinal and to be within the bounds of the set
   base type. 'e2' can be relied upon to be a set. 'er' should not be updated
   unless 'NumStatus.Valid' is returned. *)

  UnaryOpProc =
    PROCEDURE (op: M3AST_AS.UNARY; e: M3AST_SM.Exp_value;
        VAR (*out*) er: M3AST_SM.Exp_value): NumStatus RAISES {};
  (* As 'BinaryOpProc' but for unary ops. Assert 'op # Deref' *)

  StdUnaryOpProc =
    PROCEDURE (f: M3CStdProcs.Func; 
        e: M3AST_SM.Exp_value;
        VAR (*out*) er: M3AST_SM.Exp_value;
        floatType: M3AST_AS.FLOAT_TYPE := NIL): NumStatus  RAISES {};
  (* As UnaryOp, but standard function identified by 'f'. 'f' will be one of:
    M3CStdProcs.T.{Abs,Float,Floor,Ceiling,Round,Trunc}. Iff 'f=Float'
    then 'floatType' indicates, the expected type of the result. *)

  StdBinaryOpProc =
    PROCEDURE (f: M3CStdProcs.Func; 
        el: M3AST_SM.Exp_value; e2: M3AST_SM.Exp_value;
        VAR (*out*) er: M3AST_SM.Exp_value): NumStatus RAISES {};
  (* As BinaryOp, but standard function identified by 'f'. 'f' is limited to
   be M3CStdProcs.T.{Min,Max}. The arguments may not be compatible. *)

  StdUnaryTypeOpProc =
    PROCEDURE (
        f: M3CStdProcs.Func;
        ts: M3AST_SM.TYPE_SPEC_UNSET;
        VAR (*out*) er: M3AST_SM.Exp_value): NumStatus RAISES {};
  (* M3CStdProcs.T.{BitSize,ByteSize,AdrSize,First,Last}.
  For First/Last, Assert(TypeOf(ts) = Integer_type). The standard function
  Number is handled elsewhere. *)

  WordOpProc = PROCEDURE(
      w: M3CWordProcs.T;
      READONLY args: ARRAY OF M3AST_SM.Exp_value;
      VAR (* out *) er: M3AST_SM.Exp_value)
      : NumStatus
      RAISES {};
  (* Set 'er' to the result of the word operation given by 'w'. 'args'
  contains the correct number of arguments, which are guaranteed to be all
  integer. They may, however, be out of range e.g. the 'i' and 'n' arguments
  to 'Insert' might be non CARDINAL or out of range (in which case
  'NumStatus.Unknown' is returned). 'er' should only be updated if the result
  is 'NumStatus.Valid'.  *)

  CompareProc =
    PROCEDURE (el: M3AST_SM.Exp_value; 
               e2: M3AST_SM.Exp_value): INTEGER RAISES {};
  (* Return 0 if values are of same general type and equal; else non-zero.  
  Iff they are both ordinal, then return  -1 if e1 < e2, +1 if e1 > e2; this
  behaviour must be maintained even if 'e1' and 'e2' are not compatible
  ordinals according to the M3 type rules.
    Note that no check for strict type equivalence is required; the back end
  will typically do minimal type checking. Hence to do strict equality checking
  a use of a 'CompareProc' must be guarded by a further check that the types
  of the two corresponding expressions are compatible.
    'e1' and 'e2' are guaranteed to be non NIL. 'CompareProc' does not handle
  array or record constructors (use 'M3CExpValue.Equal' to check these for
  equality. *)

  LoopholeOKProc =  
    PROCEDURE (e: M3AST_AS.EXP; 
        ts: M3AST_AS.TYPE_SPEC): BOOLEAN RAISES {};
  (* Returns TRUE if 'LOOPHOLE(e, ts)' is valid. *)

  BitsOKProc =
    PROCEDURE (e: M3AST_AS.EXP;
        ts: M3AST_AS.TYPE_SPEC): BOOLEAN RAISES {};
  (* Returns TRUE if 'BITS e [ALIGN a] FOR ts' is valid. 'e' is guaranteed to
  be an integer expression and 'e.sm_exp_value' is guaranteed to be non NIL. *)

  VarParamOKProc =
    PROCEDURE (ts: M3AST_AS.TYPE_SPEC): BOOLEAN RAISES {};
  (* Returns TRUE if this type is a legal VAR parameter, i.e. can be
  addressed. (packed types) *) 

  BitSizeAndAlignProc =
    PROCEDURE (ts: M3AST_AS.TYPE_SPEC) RAISES {};
  (* Given that all components of 'ts' have already had their size
  and alignment computed, compute and set the sm_bitsize, sm_align
  attributes of 'ts'.
    If 'ts' is a packed type its 'as_exp' is guaranteed to be integer (or
  subrange of integer) and it will have a non NIL 'sm_exp_value'.
    If 'ts' is a subrange type its bounding expressions are both guaranteed
  to be ordinal with non NIL 'sm_exp_value' fields; the ordinals are not
  guaranteed to be related by the subtype relation *)

  ExpValueToTextProc =
    PROCEDURE(e: M3AST_SM.Exp_value): Text.T RAISES {};
  TextToExpValueProc =
    PROCEDURE(t: Text.T): M3AST_SM.Exp_value RAISES {};
  (* Because types can contain constants these procedures are needed when
  writing or reading type texts. They provide a way of encoding an exp value
  as a text and then recreating it from that text. When converting to a text
  'e' is guaranteed not to be a record or array constructor. When converting
  from a text 't' is guaranteed to have been produced by a call of an
  'ExpValueToTextProc' *)


(* Violation of naming conventions here - to avoid editing all the code
   that used to call these procs before they were made variable.
*)
VAR
  LiteralValue: LiteralValueProc;
  ConstructorValue: ConstructorValueProc;
  ConstructorOriginal: ConstructorOriginalProc;
  IsOrdinal: IsOrdinalProc;
  Val: ValProc;
  Ord: OrdProc;
  ConvertOrdinal: ConvertOrdinalProc;
  BinaryOp: BinaryOpProc;
  InOp: InOpProc;
  UnaryOp: UnaryOpProc;
  StdUnaryOp: StdUnaryOpProc;
  StdBinaryOp: StdBinaryOpProc;
  StdUnaryTypeOp: StdUnaryTypeOpProc;
  WordOp: WordOpProc;
  Compare: CompareProc;
  LoopholeOK: LoopholeOKProc;
  BitsOK: BitsOKProc;
  VarParamOK: VarParamOKProc;
  BitSizeAndAlign: BitSizeAndAlignProc;
  ExpValueToText: ExpValueToTextProc;
  TextToExpValue: TextToExpValueProc;

END M3CBackEnd.
