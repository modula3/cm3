(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct 31 09:37:44 PST 1994 by heydon                   *)

INTERFACE IndexedNF;

IMPORT JunoAST;

TYPE
  T = JunoAST.NormalForm BRANDED "IndexedNF.T" OBJECT
    v_cnt, c_cnt: CARDINAL := 0
  END;

(* An "IndexNF.T" is like a "JunoAST.NormalForm", except that the "var" and
   "conj" fields may be arrays that are larger than the valid number of
   existential variables and conjuncts stored in it.

   In particular, if "nf" is a "T" (indexed normal form), then the valid
   existential variables are stored in "nf.var[0..nf.v_cnt-1]", and the valid
   conjuncts are stored in "nf.conj[0..nf.c_cnt-1]". *)

PROCEDURE New(var_cnt, conj_cnt: CARDINAL := 10): T;
(* Return a new "T" from a global avail list. The values "var_cnt" and
   "conj_cnt" are hints for the sizes of the existential variable and conjunct
   arrays; the arrays of the result may be smaller or larger than the hints. *)

PROCEDURE Dispose(inf: T);
(* Return "inf" to the global avail list. *)

PROCEDURE ToNF(inf: T): JunoAST.NormalForm;
(* Return a "JunoAST.NormalForm" equivalent to "inf". *)

PROCEDURE FromNF(nf: JunoAST.NormalForm; VAR (*INOUT*) res: T);
(* Set "res" to an indexed normal form equivalent to "nf". *)

PROCEDURE AddVar(VAR (*INOUT*) res: T; var: JunoAST.NearVarLink);
(* Append the variable "var" to the variable list of "res". *)

PROCEDURE AddConj(VAR (*INOUT*) res: T; conj: JunoAST.Formula);
(* Append the formula "conj" to the conjunction list of "res". *)

PROCEDURE AddVarList(VAR (*INOUT*) res: T; vars: JunoAST.NearVarList);
(* Append the variables "vars" to the variable list of "res". *)

PROCEDURE AddConjList(VAR (*INOUT*) res: T; forms: JunoAST.ExprList);
(* Append the formulas "forms" to the conjunction list of "res". *)

PROCEDURE AddVarArray(VAR (*INOUT*) res: T; READONLY vars: JunoAST.Vars);
(* Append the variables "vars" to the variable list of "res". *)

PROCEDURE AddConjArray(VAR (*INOUT*) res: T; READONLY forms: JunoAST.Formulas);
(* Append the formulas "forms" to the conjunction list of "res". *)

END IndexedNF.
