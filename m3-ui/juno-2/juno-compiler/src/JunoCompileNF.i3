(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 27 13:48:58 PST 1997 by heydon                   *)

INTERFACE JunoCompileNF;

(* Procedures for compiling a constraint in normal form. *)

IMPORT JunoAST, JunoScope, JunoCompileErr, StackTbl;

PROCEDURE Normalize(p: JunoAST.Formula; tbl: StackTbl.T): JunoAST.NormalForm;
(* Returns a formula equivalent to "p" that has been ``normalized''. A
   constraint is in ``normal form'' if it is an (optional) "E" quantification
   (all of whose variables are unhinted) over (optional) conjunctions of
   ``normal simple formulas''. A normal simple formula takes one of the
   following forms:

|    1) x ~= y,
|    2) P(v_1,...,v_n)
|    3) x ~= F(v_1,...,v_n)
|    4) CF(nsf_1,...,nsf_n)

   where "~=" denotes "~" (near) or "=" (equality), "x", "y", and "v_1"
   through "v_n" denote either literals or variables, "nsf_1" through "nsf_n"
   denote normal simple formulas, "P" is a predicate, "F" is a function, and
   "CF" is a compound formula other than "AND" (i.e., "NOT" or "OR").
   Moreover, compound formulas (4) are guaranteed to be original top-level
   conjuncts of the input formula "p". Note that a normal simple formula does
   not contain any grouped (parenthesized) expressions.

   This is a purely syntactic transformation. The "tbl" is used to annotate
   new variables as they are introduced in "E" quantifications. New variables
   are not added to "tbl". The variables in the result are all unhinted, and
   they all have their "evar" bits set. 

   Requires that all existentially quantified variables are distinct, and that
   every use of any of the existentially quantified variables occurs within
   the scope of its quantification. *)

PROCEDURE ToCmd(
    nf: JunoAST.NormalForm;
    scp: JunoScope.T;
    stack_tbl: StackTbl.T;
    xtra_vars: JunoAST.NearVarList := NIL):
  JunoAST.Cmd RAISES {JunoCompileErr.Error};
(* Return a command that solves the unknown variables in "nf.var" \union
   "xtra_vars" for the conjunction "nf.conj" if there is a solution, and fails
   otherwise. The resulting command contains only simple equality queries,
   assignments, and queries to be passed to the Juno solver (i.e.,
   "JunoAST.ConjQuery's"). The queries passed to the Juno solver must be a
   conjunction of normal simple formulas that contain only primitive
   predicates (REAL, TEXT, PAIR) and functions (+, *, CONS, SIN, COS, ATAN,
   EXP, LN).

   Requires all "evar" fields of the variables in "nf.var" to be true, all
   "evar" fields of the variables in "xtra_vars" to be false, and all
   variables in "nf.var" \union "xtra_vars" to be unhinted. The "frozen" bits
   in "nf.var" must all be reset, and those of "xtra_vars" are set iff the
   corresponding variable's value has been initialized. *)

END JunoCompileNF.
