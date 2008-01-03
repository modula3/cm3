(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Oct 26 11:45:38 PST 1997 by heydon                   *)

(* Miscellaneous transformations on JunoAST's. *)

INTERFACE JunoASTUtils;

IMPORT JunoValue, JunoAST;

(* ====================== Id/QId/NearVar Conversions ======================= *)

PROCEDURE QIdFromNearVar(v: JunoAST.NearVarLink): JunoAST.QId;
(* Returns a new unqualified "JunoAST.QId" with the same identifier/index
   value as "v". The "hint" and "frozen" information in "v" is lost. Requires
   "v # NIL". *)

PROCEDURE QIdFromIds(mod, id: JunoAST.Id): JunoAST.QId;
(* Create and return a qualified ("mod # JunoAST.NilId") or unqualified ("mod
   = JunoAST.NilId") "JunoAST.QId" for "<mod>.<id>" or "<id>", respectively.
   The new "QId" has default "type" and "index". *)

PROCEDURE QIdFromTexts(mod, id: TEXT): JunoAST.QId;
(* Equivalent to "QIdFromIds(Atom.FromText(mod), Atom.FromText(id))". *)

PROCEDURE QIdFromId(id: JunoAST.Id): JunoAST.QId;
(* Create and return an unqualified "JunoAST.QId" "id". The new "QId" has
   default "type" and "index". *)

PROCEDURE QIdFromText(t: TEXT): JunoAST.QId;
(* Equivalent to "QIdFromId(Atom.FromText(t))". *)

(* ====================== IdList Conversions =============================== *)

PROCEDURE IdListToNearVarList(l: JunoAST.IdList): JunoAST.NearVarList;
(* Return a new "JunoAST.NearVarList" containing the same identifier/index
   pairs as the list "l". The "hint" in each element of the result is set to
   "JunoAST.NilExpr". The order of the elements in the resulting list is the
   reverse of those in "l". *)

PROCEDURE IdListToQIdList(l: JunoAST.IdList): JunoAST.QIdList;
(* Return a new "JunoAST.QIdList" containing the same (unqualified) names in
   "l" (in the same order). *)

(* ==================== Create New 1-Item Lists ============================ *)

PROCEDURE NewExprList(e: JunoAST.Expr; bp: JunoAST.T := NIL): JunoAST.ExprList;
(* Return a new, 1-item "JunoAST.ExprList" containing "e" with back pointer
   "bp". *)

PROCEDURE NewQIdList(qid: JunoAST.QId; bp: JunoAST.T := NIL): JunoAST.QIdList;
(* Return a new, 1-item "JunoAST.QIdList" containing "qid" with back pointer
   "bp". *)

PROCEDURE NewIdList(id: JunoAST.Id; index: INTEGER := 0): JunoAST.IdList;
(* Return a new, 1-item "JunoAST.IdList" containing "id", with index "index".
*)

(* ======================== Membership Tests =============================== *)

PROCEDURE MemIdList(id: JunoAST.Id; l: JunoAST.IdList): BOOLEAN;
(* Return TRUE iff "id" is a member of the list "l". *)

PROCEDURE MemNearVarList(id: JunoAST.Id; l: JunoAST.NearVarList):
  JunoAST.NearVarLink;
(* If "id" is the name of one of the variables in "l", return the
   corresponding "NearVarLink" of "l". Otherwise, return NIL. *)

(* ======================= Operations on IdList's ========================== *)

PROCEDURE CopyIdList(l: JunoAST.IdList): JunoAST.IdList;
(* Return a copy of the list "l". This procedure is non-destructive. *)

PROCEDURE ConcatIdLists(l1, l2: JunoAST.IdList): JunoAST.IdList;
(* Return the result of concatenating the lists "l1" and "l2". This procedure
   is non-destructive. *)

(* ===================== Operations on NearVarList's ======================= *)

PROCEDURE NearVarListUnion(l1, l2: JunoAST.NearVarList): JunoAST.NearVarList;
(* Return a new "JunoAST.NearVarList" containing the union of the elements in
   "l1" and "l2". The order of the resulting list is the reverse of "l1"
   concatenated with "l2". *)

PROCEDURE NearVarListCopy(l: JunoAST.NearVarList): JunoAST.NearVarList;
(* Return a copy of "l". Requires "l # NIL". *)

PROCEDURE ExtractHints(vars: JunoAST.NearVarList): JunoAST.Formula;
(* Returns a conjunction of "Equals" and "Near" predicates containing one such
   predicate for each frozen or hinted variable in "vars", respectively.
   Returns "TRUE" if all the "vars" are unhinted. Requires that
   "vars" is non-empty. *)

PROCEDURE StripHints(vars: JunoAST.NearVarList): JunoAST.NearVarList;
(* Returns a list containing the same variables as "vars", but all variables
   in the resulting list are unhinted. The "evar" fields of the result
   variables are all set to "FALSE". *)

(* ============================= MapArgs =================================== *)

TYPE Mappee = PROCEDURE(e: JunoAST.Expr): JunoAST.Expr;

PROCEDURE MapArgs(expr: JunoAST.Expr; p: Mappee): JunoAST.Expr;
(* If "expr" is of the form "f(t1,...,tn)", where "f" is one of the predicates
   or functions described below, and the "t_i" are terms, then return a new
   expression "f(p(t1),...,p(tn))". If "expr" is "TRUE" or "FALSE" (i.e., a
   "LitPred"), return "expr". Signal a run-time error otherwise.

   The legal forms for "f" are:
|    o a user-defined predicate or function (i.e., a "JunoAST.Call");
|    o predicates "JunoAST.BuiltInUnaryPred" or "JunoAST.Relation" (i.e., all
|      "JunoAST.BuiltInPred"'s except "And", "Or", "Not", and "Exists"); or
|    o functions "JunoAST.BuiltInFunc"

   Hence, of all "JunoAST.Expr"'s, the only types that cause "MapArgs" to
   signal a run-time error are:
|    o the compound formulas ("And", "Or", "Not", "Exists")
|    o a grouped expression (i.e., a "JunoAST.GroupedExpr");
|    o "AtomicExpr" (i.e., "LitValue" and "QId")
|    o "NormalForm"
*)

(* ======================= Operations on JunoAST.Vars ====================== *)

PROCEDURE MemVars(qid: JunoAST.QId; READONLY vars: JunoAST.Vars): INTEGER;
(* Return the index of "qid" -- which must be an unqualified local variable --
   in "vars", or -1 if it does not occur. "qid" appears in "vars" if there is
   a "JunoAST.NearVarLink" in "vars" with the same "index" value. *)

(* ==================== Create New Special-Purpose AST's =================== *)

PROCEDURE NewNumber(x: JunoValue.Real): JunoAST.Expr;
(* Return a new expression that is a new "JunoAST.Number" if "x" is
   non-negative, or a new "JunoAST.UMinus" containing a new "JunoAST.Number"
   if "x" is negative. *)

PROCEDURE NewPoint(x, y: JunoValue.Real): JunoAST.Pair;
(* Return a new AST that unparses to the expression "(xVal, yVal)", where
   "xVal" and "yVal" are the expressions with values "x" and "y",
   respectively, as created by "NewNumber" above. *)

PROCEDURE NewASTFromValue(v: JunoValue.T): JunoAST.T;
(* Return a new AST that unparses to the run-time value "v". *)

PROCEDURE NewAssign(v: JunoAST.QId; e: JunoAST.Expr): JunoAST.Assign;
(* Return the assignment "v := e". *)

(* ============================ Miscellaneous ============================== *)

PROCEDURE Ungroup(ast: JunoAST.T): JunoAST.T;
(* Return the largest subtree of "ast" that is not a grouped command or
   grouped expression. *)

PROCEDURE EqualQIds(qid1, qid2: JunoAST.QId): BOOLEAN;
(* Return TRUE iff "qid1" and "qid2" have the same module name and identifier
   name. *)

PROCEDURE FirstProcCall(cmd: JunoAST.Cmd; qid: JunoAST.QId): JunoAST.ProcCall;
(* Return the first procedure call command to the procedure named "qid" in the
   command "cmd", or NIL if such a procedure call does not occur in "cmd". *)

PROCEDURE AlwaysDefined(e: JunoAST.Expr): BOOLEAN;
(* Return TRUE iff "e" is an expression that is always defined. The basic
   expressions that are always defined are literals and qualified identifiers.
   In addition to these are: grouped expressions whose (body) expression is
   always defined, list expressions all of whose elements are always defined,
   pair expressions both of whose values are always defined, and call
   expressions to user-defined or external procedures all of whose IN
   parameters are always defined (except for the built-in user-defined
   "CLOSE" and "APPLY" procedures).

   Note: If this procedure is called before "e"'s atoms have been annotated by
   "JunoCompile.AnnotateAtoms", it will return FALSE in some cases where it
   would return TRUE once the atoms had been annotated. *)

END JunoASTUtils.
