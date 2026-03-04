(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* ESC specification AST types.

   These represent the parsed forms of <*SPEC ...*> and <*LOOPINV ...*>
   annotations attached to Modula-3 declarations and statements. *)

INTERFACE ESCSpec;

IMPORT Atom, RefList;

TYPE
  (* A raw pragma record, before spec parsing.  ESCPragma produces these;
     ESCSpecParse consumes them. *)
  RawPragma = OBJECT
    kind: PragmaKind;
    text: TEXT;               (* the pragma body after the keyword *)
    line, col: INTEGER;       (* source position *)
    unitName: Atom.T;         (* containing interface/module name *)
  END;

  PragmaKind = {Spec, LoopInv, PragmaSpec};

  (* --- Parsed specification forms (Phase 1+) --- *)

  Spec = OBJECT END;         (* abstract base for all spec forms *)

  ProcSpec = Spec OBJECT
    name: Atom.T;             (* procedure name from SPEC line *)
    formals: RefList.T;       (* list of Atom.T formal names *)
    requires: RefList.T;      (* list of Expr *)
    ensures: RefList.T;       (* list of Expr *)
    modifies: RefList.T;      (* list of ModifiesClause *)
  END;

  GhostVar = Spec OBJECT
    name: Atom.T;
    kind: GhostVarKind;
    domainType: Atom.T;       (* for MAP/SEQ: the domain type *)
    rangeType: Atom.T;        (* for MAP: range type; SEQ: element type *)
  END;

  GhostVarKind = {Plain, Map, Seq};

  AbstractDef = Spec OBJECT
    name: Atom.T;
    body: Expr;
  END;

  DependDecl = Spec OBJECT
    abstractVar: Atom.T;
    concreteFields: RefList.T;  (* list of Atom.T *)
  END;

  SpecFunc = Spec OBJECT
    name: Atom.T;
    formals: RefList.T;
    body: Expr;
  END;

  SpecAxiom = Spec OBJECT
    body: Expr;
  END;

  Invariant = Spec OBJECT
    kind: InvariantKind;
    body: Expr;
  END;

  InvariantKind = {Invariant, Rep};

  LoopInv = Spec OBJECT
    body: Expr;
  END;

  (* --- Expression AST for spec sub-language --- *)

  Expr = OBJECT END;         (* abstract base *)

  EIdent = Expr OBJECT
    name: Atom.T;
  END;

  EPrimed = Expr OBJECT      (* x' -- post-state *)
    base: Expr;
  END;

  ESelect = Expr OBJECT      (* a[i] or a.f *)
    base, index: Expr;
  END;

  EField = Expr OBJECT       (* x.field *)
    base: Expr;
    field: Atom.T;
  END;

  ECall = Expr OBJECT        (* f(args) *)
    func: Atom.T;
    args: RefList.T;          (* list of Expr *)
  END;

  EBinOp = Expr OBJECT
    op: BinOp;
    left, right: Expr;
  END;

  BinOp = {And, Or, Implies, Eq, Neq, Lt, Le, Gt, Ge, Add, Sub, Mul};

  EUnOp = Expr OBJECT
    op: UnOp;
    operand: Expr;
  END;

  UnOp = {Not, Neg};

  EQuant = Expr OBJECT       (* ALL [x: T] P  or  EX [x: T] P *)
    kind: QuantKind;
    var: Atom.T;
    type: Atom.T;
    body: Expr;
  END;

  QuantKind = {All, Exists};

  EConst = Expr OBJECT
    val: INTEGER;
  END;

  ENil = Expr OBJECT END;    (* NIL literal *)
  ERes = Expr OBJECT END;    (* RES -- return value *)
  EFresh = Expr OBJECT       (* FRESH(x) *)
    arg: Expr;
  END;

  ENumber = Expr OBJECT      (* NUMBER(x) *)
    arg: Expr;
  END;

  (* Modifies clause *)
  ModifiesClause = OBJECT
    target: Expr;             (* e.g., Data[t] or Data[t][i] *)
  END;

END ESCSpec.
