(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Expr.i3                                               *)
(* Last Modified On Tue Jun 20 15:17:31 PDT 1995 By kalsow     *)

INTERFACE Expr;

IMPORT M3, M3Buf, CG, Target, Type;

TYPE
  T    = M3.Expr;
  List = REF ARRAY OF T;
  CheckState = M3.CheckState;

(*** phase 1 ***)

PROCEDURE Parse (): T;
(* parses an expression *)

(*** phase 2 ***)

PROCEDURE TypeOf (t: T): M3.Type;
(* returns the type of the expression *)

PROCEDURE SemTypeOf (t: T): M3.Type;
(* returns the semantic type of the expression, as in source code. *)

PROCEDURE RepTypeOf (t: T): M3.Type;
(* returns the representation type of the expression, as in memory. *)
(* PRE: t is Checked. *)

PROCEDURE TypeCheck (t: T;  VAR cs: CheckState);
(* typechecks the expression. *)

TYPE lengthTyp = INTEGER;
(* NOTE: Modula3 uses "length" to mean an array element count. *)
CONST lengthNonStatic = -1;
CONST lengthNonArray = -2;
CONST lengthInvalid = -3;

PROCEDURE StaticLength (t: T): lengthTyp;

PROCEDURE UsesAssignProtocol (rhs: T): BOOLEAN;
(* See ArrayExpr.m3 for explanation of protocols. *)
(*** phase 3 ***)

PROCEDURE ConstValue (t: T): T;
(* Returns NIL if t is not a constant, otherwise returns a simplified
   expression that denotes t.  May be called before the expression
   is typechecked. *)

PROCEDURE GetBounds (t: T;  VAR min, max: Target.Int);
(* returns upper and lower bounds for the value of the expression. *)

PROCEDURE IsDesignator (t: T): BOOLEAN;
(* TRUE iff t is a designator *)

PROCEDURE IsWritable (t: T; traced: BOOLEAN): BOOLEAN;
(* TRUE iff t is a writable designator *)

PROCEDURE IsZeroes (t: T): BOOLEAN;
(* PRE: t is checked. *)
(* TRUE if t's binary representation is all zeroes *)

PROCEDURE GetSign (t: T): CG.Sign;
(* returns the best guess of t's sign *)

PROCEDURE NeedsAddress (t: T);
(* marks t as needing a memory address *)

PROCEDURE SupportsDirectAssignment (t: T): BOOLEAN;
(* returns "TRUE" if "t" supports direct assignments. *)

PROCEDURE MarkForDirectAssignment (t: T);
(* If called, must be before Prep(t). *)
(* mark "t" so that when compiled it will assume the code generator
   stack contains the address of a LHS that is to be assigned-to, and
   it will perform the assignment. *)

PROCEDURE IsMarkedForDirectAssignment (t: T): BOOLEAN;
(* returns "TRUE" if "t" is marked for direct assignment *)

PROCEDURE Alignment (t: T): Type.BitAlignT;
(* A bit alignment that t is guaranteed to have.  Hopefully maximum, or
   nearly so.  Always a true alignment, possibly as small as 1 bit. 
   Expression alignments are more precise than type alignments in that they
   can take into account properties of an expression that the expression's
   type does not necessarily have in general.  Particularly, if a value is
   a field or element, they can depend on its containing record, object,
   or array.  For an open array expression, this is the alignment of the
   elements, not the dope.
   Compare to Type.T.info.alignment. 
*)


(*** phase 4 ****)

(* Expressions are compiled in two steps:
     Prep: emit any code that includes branches or stores
     Compile: emit the rest of the code
*)

PROCEDURE Prep (t: T);
PROCEDURE Compile (t: T);
(* Emit code to evaluate the expression onto the top of stack. For some
   types, this could be an address (arrays, records, bit sets). *)

PROCEDURE PrepLValue (t: T; traced: BOOLEAN);
PROCEDURE CompileLValue (t: T; traced: BOOLEAN);
(* Emit code to evaluate 't's L-value into s0.A. 't' must be a designator. *)

PROCEDURE CompileAddress (t: T; traced: BOOLEAN);
(* emits code to evaluate 't's byte address onto the top of stack.
   Equivalent to CompileLValue, followed by a runtime check that it
   is byte aligned.
   Use PrepLValue to prep these expressions. *)

PROCEDURE PrepBranch (t: T;  true, false: CG.Label;  freq: CG.Frequency);
PROCEDURE CompileBranch (t: T;  true, false: CG.Label;  freq: CG.Frequency);
(* emits code to evaluate the expression and conditionally branch to 'true'
   or 'false' depending on its boolean value.  'freq' is the estimated
   frequency that the specified branch will be taken. *)

PROCEDURE NoteWrite (t: T);
(* generates any tracing implied by a write to 't' *)

PROCEDURE IsEqual (a, b: T;  x: M3.EqAssumption): BOOLEAN;
(* TRUE iff (value(a) = value(b)), assuming a constant global store
   and the type equalities represented by 'x'.  *)

PROCEDURE PrepLiteral (t: T;  type: M3.Type;  is_const: BOOLEAN);
(* prepares constant values for GenLiteral *)

PROCEDURE GenLiteral (t: T;  offset: INTEGER;  type: M3.Type;  is_const: BOOLEAN);
(* initializes the global storage at ADR(x)+offset in the global data segment
   or constant pool to the constant value t.  For any expression t, PrepLiteral(t)
   must be called before GenLiteral (t).  *)

PROCEDURE GenFPLiteral (t: T;  mbuf: M3Buf.T);
(* add the string denoting the literal value of 't' to 'mbuf' *)

PROCEDURE BadOperands (op: TEXT;  a, b: M3.Type := NIL): M3.Type;
(* generate an "illegal operands" error message if neither 'a' nor 'b'
   is the contagious error type and return the error type *)

PROCEDURE Use (t: T): BOOLEAN;
(* Generate runtime actions prior to a use of t that does not call Compile.
   Return TRUE IFF following code is reachable. *)

END Expr.
