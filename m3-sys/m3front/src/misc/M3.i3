(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: M3.i3                                                 *)
(* Last modified on Fri Feb 24 16:22:09 PST 1995 by kalsow     *)
(*      modified on Wed Nov 21 01:28:24 1990 by muller         *)

(* This interface defines some base object types so that circular
   dependencies between other interfaces may be avoided. *)

INTERFACE M3;

IMPORT M3ID, M3Buf, Jmpbufs;

TYPE
  Flag = BITS 1 FOR BOOLEAN;

  QID  = RECORD module, item: M3ID.T; END;

(*------------------------------------------------------------- AST nodes ---*)

TYPE
  Node      = OBJECT origin: INTEGER END;
  ValueNode = Node OBJECT next: Value := NIL END;

TYPE
  Scope     <: Node;       (* == Scope.T *)
  Stmt      <: Node;       (* == Stmt.T  *)
  Expr      <: Node;       (* == Expr.T  *)
  Value     <: ValueNode;  (* == Value.T *)
  Type      <: Node;       (* == Type.T  *)
  ExSet     <: Node;       (* == ESet.T  *)
  ExSetList <: REFANY;     (* == list of ExSet *)
  EqAssumption <: ADDRESS; (* == Type.Assumption *)

(*--------------------------------------------------------- type checking ---*)

TYPE (* the "global state" that is passed around during type checking *)
  CheckState = RECORD
    raises_others : BOOLEAN;
    ok_to_raise   : ExSetList;
    no_error      : ExSetList;
    jmpbufs       := Jmpbufs.CheckState { };
  END;

CONST
  OuterCheckState = CheckState {
    raises_others := FALSE,
    ok_to_raise   := NIL,
    no_error      := NIL,
    jmpbufs       := Jmpbufs.CheckState { }
  };

(*-------------------------------------------------------- fingerprinting ---*)

TYPE
  FPInfo = RECORD
    tag     : TEXT;
    buf     : M3Buf.T;
    n_nodes : INTEGER;
    nodes   : ARRAY [0..5] OF Type;
    others  : REF ARRAY OF Type;
  END;
  (* "fp" methods must either assign a non-NIL value to "tag"
     or build a string in "buf".  They must also set "n_nodes" and
     either "nodes" or "others".  If "n_nodes <= NUMBER (nodes)",
     "nodes" must be used, otherwise "others" is used. *)

END M3.
