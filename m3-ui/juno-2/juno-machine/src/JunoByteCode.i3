(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 28 10:40:35 PST 1997 by heydon                   *)

INTERFACE JunoByteCode;

(* This interface contains the constants which represent each of the
   bytecodes in the bytecode interpreter.  After each bytecode constant is
   a representation of the number and size of the arguments expected in the
   bytecode stream. These arguments have the following meanings:

|    m, n    8-bit unsigned offsets [0..255]
|    e       8-bit unsigned error code
|    v       32-bit unsigned index into the global variable_tbl
|    p       32-bit unsigned index into the global code_tbl
|    P       32-bit unsigned index into the global ext_code_tbl
|    s       signed 16-bit offset
|    u       unsigned 16-bit offset
|    r       JunoValue.Real number

   "PUSHL s": Push the local at offset "s" from "fp" onto the stack. A
   negative index represents a procedure argument; a positive index represents
   a procedure local variable (or temporary). See the description of the
   procedure call protocol below for a more detailed description of local
   variable indices.

   "POPL s": Pop the top stack value into the local at offset "s" from "fp".

   "PUSHG v", "POPG v": Push or pop global variables at location "v" in the
   global "variable_tbl".

   "INCSP n", "DECSP n": Increment, decrement the stack pointer by "n".

   "PUSHM3NIL n": Push Modula-3 "NIL" onto the stack "n" times. Modula-3 NIL
   on the stack corresponds to an uninitialized variable or an unhinted solve
   variable.

   "PUSHNIL": Push Juno "NIL" onto the stack.

   "PUSHNUM r": Push the real Juno value "r" onto the stack.

   "C_OFF", "C_ON": Set the condition variable to FALSE or TRUE, respectively.

   "JUMP s": Jump to the location specified by the relative offset "s". The
   offset is "s" bytes relative to the address of the instruction following
   the "JUMP".

   "TJUMP s", "FJUMP s": Jump to the location specified by the relative offset
   "s" iff the condition variable is TRUE or FALSE, respectively. The offset
   is "s" bytes relative to the address of the instruction following the
   "TJUMP" or "FJUMP".

   "UJUMP s": To be used only on return from a call to a user-defined function
   or a functional external procedure compiled as an expression. "UJUMP" is
   like "FJUMP", but it also has the side-effect in the case that the
   condition bit is FALSE of decrementing the stack pointer (to skip past the
   single out parameter to the function) and setting the internal "undefined
   term" address.

   "CALL p": Call the bytestream procedure at index "p" in the global
   "code_tbl". It is the caller's responsibility to make space for the OUT
   parameters on the stack, to then push the INOUT and IN parameters onto the
   stack (in left-to-right order), and to remove the IN parameters from the
   stack on return from the procedure. See "Procedure Call Protocol" below.

   "CALLEXT P": Call the external (Modula-3) procedure at index "P" in the
   global "ext_code_tbl". The caller's responsibilities are the same as for
   the "CALL" instruction above. In the event that the procedure failed on its
   arguments, single a "Failed External Procedure" runtime error.

   "RET": Return from a bytestream procedure.

   "ERROR e", "FERROR e": "ERROR" causes a run-time error.  The global error
   code is set to "e", and the machine traps with a "JunoRT.TrapCode.Error".
   See the "JunoRT" interface for a description of the other fields of the
   execution result in the event of an error. See the "JunoRTError" interface
   for the possible error codes. "FERROR" causes a run-time error iff the
   condition variable is FALSE.

   "ADD s", "SUBTRACT s", "MULTIPLY s", "DIVIDE s", "DIV_ s", "MOD_ s",
   "MAX_ s", "MIN_ s", "ATAN s": Perform the specified arithmetic operation on
   the top two stack entries, where the top entry is the right operand. The
   result replaces them on the stack unless the result is undefined, in which
   case the arguments are removed and a branch is made to the specified offset
   "s".

   "NEGATE s", "ABS_ s", "FLOOR_ s", "CEILING_ s", "ROUND_ s", "SIN s",
   "COS s", "LN s", "EXP s": Perform the specified arithmetic operation on
   the top stack entry, replacing it with the result. If the operation is
   undefined, the argument is removed from the stack, and a branch is made to
   the specified offset "s".

   "REL s": Pop the top two items from the stack and perform a REL on them.
   If "a" is the top stack item and "(b,c)" is the next item, "a REL (b,c)"
   is computed and pushed onto the stack. If the types of the arguments are
   wrong, a branch is performed to offset "s".

   "CAR s", "CDR s": Replace the top stack entry with its CAR or CDR,
   respectively. If the operation is undefined, the argument is removed from
   the stack, and a branch is made to the specified offset "s".

   "CAR_CDR s": Replace the top stack entry with its CDR and then its CAR (so
   the new top entry of the stack will be the CAR). If the operation is
   undefined, the argument is removed from the stack, and a branch is made to
   the specified offset "s".

   "CONS": Replace the top two stack entries by a new pair whose CDR was the
   top stack entry and whose CAR was the second top-most stack entry. No error
   can occur.

   "LIST u": Replace the top "u" elements from the stack with an "u"-element
   list formed from them; the top-most stack element becomes the last element
   of the resulting list.

   "CONCAT s": Replace the top two stack entries by their concatenation; the
   top-most stack element is the right argument.

   "IS_REAL", "IS_INT", "IS_TEXT", "IS_PAIR": Test the type of the top stack
   entry without popping it. Set the machine flag "cond" iff that element has
   the proper type.

   "EQUAL", "LESS", "AT_MOST": Remove and compare the top two stack entries,
   where the top entry is the right operand, and set the machine flag "cond"
   accordingly. If the values have incomparable types, then "cond" is reset.

   "CONG", "PARA": Pop the top two items from the stack and perform a CONG or
   PARA, respectively, on them. If the two elements are the segments "(a, b)"
   and "(c, d)", where "a", "b", "c", and "d" are points, then set the machine
   flag "cond" iff the two segments are congruent or parallel, respectively. 

   "HOR", "VER": Pop the top two items from the stack and perform a HOR or
   VER, respectively, on them. If the two elements are the points "(a, b)"
   and "(c, d)", where "a", "b", "c", and "d" are reals, then set the machine
   flag "cond" iff the two segments are horizontal or vertical, respectively. 

   "NEWCL p": Pushes a new closure value for the procedure in slot "p"
   taking 0 arguments.

   "NEWEXTCL P": Pushes a new closure value for the procedure in slot "P"
   taking 0 arguments.

   "CLOSE u, s": Requires that the top "u+1" elements on the stack (from bottom
   to top) are a closure value "C" and "u" values "v_1, ..., v_u" (pushed in
   left-to-right order). These "u+1" values are replaced by a new closure
   formed by extending "C" by the values "v_1, ..., v_u". If "C" is not a
   closure, then may branch to the code at relative offset "s".

   "APPLY uOuts, uInOuts, uIns, s": Pop the stack, which is required to
   contain a closure value. If this value is not a closure, or if the
   signature of the closure's procedure is incompatible with this call, the
   instruction branches to the relative offset "s".
     This bytecode invokes the closure like a procedure call instruction.
   Hence, before invoking apply, space should have been made for the OUT
   parameters, and the INOUT and IN parameters should have been pushed on the
   stack. If the closure on the top of the stack names the procedure "P" and
   the closure has "k" values, then this bytecode checks that "P" has "uOuts"
   OUT parameters, "uInOuts" INOUT parameters, and "k + uIns" IN parameters.
     "APPLY" stores the number of values in the closure in the "clIns" field
   of the current internal state before it transfers control.
     If the procedure in the closure is external, then this instruction
   executes the procedure; if the procedure fails on its argument, a "Failed
   External Procedure" run-time error is signaled. If the procedure in the
   closure is internal, this instruction prepares the stack frame for it an
   branches to its first instruction.

   "CLDECSP": Decrement the stack pointer by the "clIns" stored in the
   current internal state.  Should follow every "APPLY" instruction.

   "SOLVE u1, u2, u3, constraints": This bytecode models a procedure call with
   "u1" INOUT parameters and "u2" IN parameters. Thus, a total of "u1 + u2"
   parameters are assumed to be on the stack when this bytecode executes. When
   the bytecode completes, it automatically removes the IN parameters from the
   stack, but leaves the INOUT parameters on the stack. The machine flag
   "cond" is set to TRUE iff the solve was successful.

   "u3" is the total number of constraints to solve for; it must be positive. 
   Each constraint in the "constraints" bytes takes the following form:

|    n, uX, [uY, [ uZ ]]

   where "n" represents the type of constraint; and "uX", "uY", and
   "uZ" are unsigned offsets in the range "[0..u1+u2-1]" in the INOUT
   and IN parameters on the stack. Offset 0 corresponds to the first
   INOUT parameter, offset "u1" corresponds to the first IN parameter,
   and offset "u1+u2-1" corresponds to the last IN parameter at the top
   of the stack. "uZ" is only present if "n <= 3"; "uY" is present if "n <= 8".
   The value for "n" must be in the range "ConRange". The equations for the
   values of "n" are:

|    (1) x = CONS(y, z)
|    (2) x = y + z
|    (3) x = y * z
|    (4) x = ATAN(y, z)
|    (5) x = y
|    (6) x = SIN(y)
|    (7) x = COS(y)
|    (8) x = EXP(y)
|    (9) IS-REAL(x)
|   (10) IS-TEXT(x)

*)

(* Procedure Call Protocol

   It is the caller's responsibility to make space for the OUT parameters
   before pushing the INOUT and IN parameters on the stack. On return from the
   procedure call, the stack pointer will be the same as at the time of the
   call. Hence, it is the caller's responsibility to decrement the stack
   pointer to skip the IN parameters, which will reside on the top of the
   stack. The caller can than pop the INOUT and OUT results into local
   variables.

   Variable Indices

   Inside the call, OUT, INOUT, and IN parameters are "local" variables
   at negative offsets from the frame pointer. Hence, they have negative
   indices in the frame. The parameters are organized on the stack from bottom
   to top in the order OUT's, INOUT's, and IN's. If there are a total of "x"
   IN parameters, "y" INOUT parameters, and "z" OUT parameters, then parameter
   indices are given by the following table:

|     IN      [-x..-1]
|     INOUT   [-x-y..-x-1]
|     OUT     [-x-y-z..-x-y-1]

   The order of the parameters on the stack is governed by the following rule:

|     The parameters have decreasing indices starting with -1 when read from
|     *right* to *left* in the order they appear in a PROC, PRED, or FUNC
|     declaration.

   For example, for the signature:

|  PROC x, y := (s, t, u)P(a, b, c, d)

   the variables have the following indices:

|  x = -9, y = -8, s = -7, t = -6, u = -5, a = -4, b = -3, c = -2, d = -1

   The temporary (or projected) variables are also "local" variables, but
   their indices are strictly positive, starting at 1. Thus, the first
   temporary variable has index 1. It is the callee's responsibility to make
   space on the stack for the temporary frame variables on entry to the
   procedure. This space is "automatically" deallocated on return from the
   procedure when the stack frame for the procedure is destroyed.

   Condition Bit

   There are four types of targets of a procedure call, and on return from
   each, the machine's internal condition bit is interpretted differently. The
   four types are: predicates whose bodies are compiled as procedures,
   functions whose bodies are compiled as procedures, user-defined procedures,
   and external procedures. Here is the meaning of the condition bit on return
   from the call in each case:

   1. Predicates whose bodies are compiled as procedures

   The condition bit is true or false as the body of the compiled predicate
   halted or produced a guard failure. Hence, the bit indicates if the
   predicate is true or false on the supplied arguments.

   2. Functions whose bodies are compiled as procedures

   The condition bit is true or false as the body of the compiled function
   halted or produced a guard failure. Hence, the bit indicates if the
   function is defined or undefined on the supplied arguments.

   3. User-defined procedures

   Since user-defined procedures are total, the condition bit must always be
   true on return from a call to a user-defined procedure.

   4. External procedures

   The condition bit is true or false as the external procedure succeeded or
   failed on its arguments. The interpretation of success and failure depends
   on whether the external procedure was used functionally or not. If used
   functionally, failure results in an "undefined term" runtime error. If not
   used functionally, failure results in an "external procedure failed"
   runtime error.
*)

CONST

  (* Stack operations *)
  PUSHL    = 1;                 (* s *)
  PUSHG    = 2;                 (* v *)
  POPL     = 3;                 (* s *)
  POPG     = 4;                 (* v *)
  INCSP    = 5;                 (* n *)
  DECSP    = 6;                 (* n *)
  PUSHM3NIL= 7;                 (* n *)
  PUSHNIL  = 8;
  PUSHNUM  = 9;                 (* r *)

  (* Condition flag *)
  C_OFF    = 10;
  C_ON     = 11;

  (* Jumps *)
  JUMP     = 15;                (* s *)
  TJUMP    = 16;                (* s *)
  FJUMP    = 17;                (* s *)
  UJUMP    = 18;                (* s *)

  (* Procedure call *)
  CALL     = 20;                (* p *)
  RET      = 21;
  ERROR    = 22;                (* e *)
  FERROR   = 23;                (* e *)
  CALLEXT  = 24;                (* P *)

  (* Arithmetic *)
  ADD      = 25;                (* s *)
  SUBTRACT = 26;                (* s *)
  MULTIPLY = 27;                (* s *)
  DIVIDE   = 28;                (* s *)
  DIV_     = 29;                (* s *)
  MOD_     = 30;                (* s *)
  NEGATE   = 31;                (* s *)
  ABS_     = 32;                (* s *)
  FLOOR_   = 33;                (* s *)
  CEILING_ = 34;                (* s *)
  ROUND_   = 35;                (* s *)
  MAX_     = 36;                (* s *)
  MIN_     = 37;                (* s *)
  ATAN     = 38;                (* s *)
  SIN      = 39;                (* s *)
  COS      = 40;                (* s *)
  LN       = 41;                (* s *)
  EXP      = 42;                (* s *)
  REL      = 43;                (* s *)

  (* Cons cells and lists *)
  CAR      = 45;                (* s *)
  CDR      = 46;                (* s *)
  CAR_CDR  = 47;                (* s *)
  CONS     = 48;
  LIST     = 49;                (* u *)

  (* Text *)
  CONCAT   = 50;                (* s *)

  (* Type predicates *)
  IS_REAL  = 55;
  IS_INT   = 56;
  IS_TEXT  = 57;
  IS_PAIR  = 58;

  (* Comparison predicates *)
  EQUAL    = 60;
  LESS     = 61;
  AT_MOST  = 62;

  (* Built-in geometry predicates *)
  CONG     = 65;
  PARA     = 66;
  HOR      = 67;
  VER      = 68;

  (* Closures *)
  NEWCL    = 70;
  NEWEXTCL = 71;
  CLOSE    = 72;
  APPLY    = 73;
  CLDECSP  = 74;

  (* Solver *)
  SOLVE    = 75;                (* u1, u2, u3, constraints ... *)

(* Constraints for SOLVE *)
  CONS_C  = 1;
  SUM_C   = 2;
  PROD_C  = 3;
  ATAN_C  = 4;
  EQUAL_C = 5;
  SIN_C   = 6;
  COS_C   = 7;
  EXP_C   = 8;
  REAL_C  = 9;
  TEXT_C  = 10;

TYPE ConRange = [CONS_C..TEXT_C];

VAR names: ARRAY [PUSHL..SOLVE] OF TEXT;

(* "names[i]" is the text version of the bytecode instruction with value
   "i"; if "i" does not correspond to a bytecode, "names[i]" is "INVALID".
   The names in the range of this array are the same as the constant names
   above except that trailing underscore characters in the constant names are
   not included in the instruction names (e.g. the "FLOOR_" constant name
   corresponds to the instruction named "FLOOR"), and all other underscores in
   the constant names are hyphens in the instruction names (e.g., the "IS_INT"
   constant name corresponds to the instruction named "IS-INT". *)

END JunoByteCode.
