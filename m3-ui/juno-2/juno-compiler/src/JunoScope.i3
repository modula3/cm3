(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Nov 12 18:54:18 PST 1994 by heydon                   *)
(*      modified on Fri Aug  7 21:53:58 PDT 1992 by myers                    *)

INTERFACE JunoScope;

(* A Scope.T is a mapping that associates names with constants, variables,
   predicates, functions, procedures, interfaces, and modules (that is, with
   any of the things that a name can denote in Juno). When an abstract syntax
   tree (or JunoAST.T) is decorated by the type checker, each node of the AST 
   that introduces a scope is labelled with a Scope.T that records the
   bindings of the names introduced at that scope level.
   
   Scopes are arranged in a tree structure that reflects their nesting in the
   program text; if a look-up on an identifier fails in a scope, the
   implementation continues by looking up the identifier in the parent scope,
   the parent's parent, and so on, until the lookup succeeds or finally fails
   on the root scope.

   This interface also defines the type "Entity". In a scope, each name
   (represented by an Atom.T) is bound to an "Entity". "Entity"'s represent
   Juno local variables, procedure arguments, constants, globals, predicates,
   functions, procedures, interface scopes, and module scopes.

   Scopes are typically used in four different ways. There are "root"
   scopes (which name modules), "unit" scopes (which name top-level module
   elements), "code" scopes (which name predicate, function, and procedure
   arguments), and "proj" scopes (which name projected variables). The four
   uses are distinguished by the types of their parent scopes and by the types
   of "Entity"'s they contain. In summary:

|  Scope Type  Parent Type    Contains        Contained Entity Type's
|  ----------  -------------  --------------  ----------------------------
|  "Root"      NIL            Module names    Mod
|  "Unit"      "Root"         Block names     Const, Var, Pred, Func, Proc
|  "Code"      "Unit"         Argument names  Arg
|  "Proj"      "Code"/"Proj"  Local vars      Temp

*)

IMPORT JunoAST, StackTbl;
FROM JunoCompileErr IMPORT Error;
IMPORT Atom, Wr;

TYPE
  T <: ROOT;

PROCEDURE New(p: T; size: CARDINAL := 1): T;
(* Return a new scope with parent scope "p" in which no names are bound. 
   If p is NIL, the new scope is a root scope. Initially, the scope is created
   with size "size", but it will grow dynamically to accommodate any number of
   bindings. *)

PROCEDURE Parent(scp: T): T;
(* Return the parent of "scp", or NIL if "scp" is a root scope. *)

PROCEDURE SetParent(scp, parent: T);
(* Set the parent scope of "scp" to "parent". *)

PROCEDURE Lookup(scp: T; id: Atom.T; localOnly := FALSE): Entity;
(* Return the entity associated with the name id in the scope scp.  
   Returns NIL if id is unbound. If localOnly is FALSE, then all 
   scopes on the path from scp to scp's root are searched in order; 
   otherwise only "scp" is searched. *)

PROCEDURE LookupQId(
    scp: T; qid: JunoAST.QId; VAR (*OUT*) unit: Entity): Entity;
(* If "qid" is unqualified, then set "unit" to NIL and return the result of
   Lookup(scp, qid.id0). Otherwise, set "unit" to "Lookup(scp, qid.id0)". If
   that is a non-"NIL" "Mod", then return "Lookup(mod.public_scp, qid.id1)";
   else return "NIL". *)

PROCEDURE Names(scp: T): REF ARRAY OF Atom.T;
(* Return an array containing the names bound in "scp" (not including names
   bound in any of its ancestor scopes). *)

PROCEDURE LocalArgs(scp: T; kinds: SET OF ArgKind): JunoAST.IdList;
(* Return a list of identifiers corresponding to those "Arg" entities in "scp"
   with a "kind" that is a member of the set "kinds". *)

EXCEPTION NameClash; NotFound;

PROCEDURE Bind(scp: T; id: Atom.T; e: Entity) RAISES { NameClash };
(* Bind "id" to "e" in "scp".  This creates a new binding in "scp"; it never 
   affects "scp"'s ancestors.  Bind raises the exception "NameClash" if "id"
   is already bound to something in "scp". *)

PROCEDURE Rebind(scp: T; id: Atom.T; e: Entity);
(* Bind "id" to "e" in "scp".  If "id" is already bound in "scp" this changes
   the binding. Otherwise, it creates a new binding. This procedure never
   affects "scp"'s ancestors. *)

PROCEDURE Unbind(scp: T; id: Atom.T): Entity RAISES { NotFound };
(* If "id" is bound in "scp", then return the entity it is bound to and remove
   the binding from "scp". Otherwise, raise "NotFound". *)

PROCEDURE Debug(scp: T; level: CARDINAL := 0);
(* Equivalent to "Print(Stdio.stderr, scp, level, 2)". *)

PROCEDURE Print(wr: Wr.T; scp: T; level, indent: CARDINAL := 0);
(* Print a description of "scp" to "wr" at logical indentation level "indent".
   Nested scopes deeper than "level" are not shown. Hence, when "level = 0",
   only the top-level entities of "scp" are shown. *)

PROCEDURE PrintEntity(wr: Wr.T; ent: Entity; level, indent: CARDINAL);
(* Print a description of "ent" to "wr" at logical indentation level "indent".
   Nested scopes deeper than "level" are not shown. Hence, if "ent" is an
   entity with a scope field, the scope is elided. *)

TYPE
  (* Types introduced as classes for the purposes of sub-typing only. *)

  Entity <: ROOT;			 (* LocalValue | Value | Code | Unit *)

  (* LocalParam | Temp *)
  LocalValue = Entity BRANDED "JunoScope.LocalValue" OBJECT
    offset: INTEGER;			 (* offset in current frame from fp *)
  END;

  ArgKind = { Out, InOut, In };

  (* Arg *)
  LocalParam = LocalValue BRANDED "JunoScope.LocalParam" OBJECT
    kind: ArgKind;			 (* kind of parameter *)
  END;

  (* Const | Var *)
  Value = Entity BRANDED "JunoScope.Value" OBJECT
    init: JunoAST.Expr;			 (* may be JunoAST.NilExpr for Var *)
    index: CARDINAL;			 (* index into JunoRT.value_tbl *)
  END;

  (* PredCode | ProcCode *)
  Code = Entity BRANDED "JunoScope.Code" OBJECT
    formals: T;				 (* pred/func/proc formal args *)
    tbl: StackTbl.T;			 (* local variable table *)
    index: CARDINAL;			 (* index into JunoRT.[ext_]code_tbl *)
    in_cnt: CARDINAL;			 (* # of IN parameters *)
  END;

  (* Pred | Func *)
  PredCode = Code BRANDED "JunoScope.PredCode" OBJECT
    body: JunoAST.Formula;		 (* predicate/function body *)
    normal_form: JunoAST.NormalForm;	 (* normal form of constraint *)
  END;

  (* Proc *)
  ProcCode = Code BRANDED "JunoScope.Proc" OBJECT
    out_cnt: CARDINAL;			 (* # of OUT parameters *)
    inout_cnt: CARDINAL;		 (* # of INOUT parameters *)
    body: JunoAST.Cmd;			 (* original body of the procedure *)
    external := FALSE			 (* Modula-3 external procedure? *)
  END;
  (* The "body" field is relevant iff the "external" field is "FALSE". *)

  (* Mod *)
  Unit = Entity BRANDED "JunoScope.Unit" OBJECT
    public_scp: T;                       (* scope for public declarations *)
    scp: T;				 (* scope for all declarations *)
  END;

  (* Clients should create instances of the following types. *)

  Temp <: LocalValue;			 (* projected local variable *)
  Arg <: LocalParam;			 (* proc/pred/func arg *)

  Const <: Value;			 (* top-level CONST *)
  Var <: Value;				 (* top-level VAR *)

  Pred <: PredCode;			 (* top-level PRED *)
  Func <: PredCode;			 (* top-level FUNC *)

  Proc <: ProcCode;			 (* top-level PROC *)

  Mod <: Unit;				 (* MODULE *)

PROCEDURE NewPred(pred: JunoAST.PredDecl; mod: JunoAST.Id): Pred
    RAISES {Error};
(* Return a new, complete predicate entity for "pred" in the module named
   "mod". Raises "JunoCompile.Error" if two or more formals in "proc" have the
   same name. *)

PROCEDURE NewFunc(func: JunoAST.FuncDecl; mod: JunoAST.Id): Func
    RAISES {Error};
(* Return a new, complete function entity for "func" in the module named
   "mod". Raises "JunoCompile.Error" if two or more formals in "proc" have the
   same name. *)

PROCEDURE NewProc(proc: JunoAST.ProcDecl; mod: JunoAST.Id): Proc
    RAISES {Error};
(* Return a new, complete procedure entity for "proc" in the module named
   "mod". Raises "JunoCompile.Error" if two or more formals in "proc" have the
   same name. *)

END JunoScope.
