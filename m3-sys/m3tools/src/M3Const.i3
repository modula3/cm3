(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

INTERFACE M3Const;

IMPORT M3ID, M3AST, M3Type, Target;

TYPE
  T = RECORD
    class : Class;
    info  : INTEGER;
    int   : Target.Int;
    float : Target.Float;
    type  : M3Type.T;
    ref   : REFANY;
  END;

  Class = {     (* other fields... *)
    Integer,    (* t.int == value,             t.type == TYPEOF(value) *)
    Float,      (* t.float == value,           t.type == TYPEOF(value) *)
    Enum,       (* t.info == ORD (value),      t.type == TYPEOF(value) *)
    Text,       (* t.ref == value:TEXT,        t.type == TYPEOF(value) *)
    Type,       (* t.type == value                                     *)
    Addr,       (* t.info == value,            t.type == TYPEOF(value) *)
    Set,        (* t.ref == value:M3SetVal.T,  t.type == TYPEOF(value) *)
    Record,     (* t.ref == value:M3RecVal.T,  t.type == TYPEOF(value) *)
    Array,      (* t.ref == value:M3ArrVal.T,  t.type == TYPEOF(value) *)
    Exception,  (* t.{info,ref} == {NodeIndex,M3AST} of decl           *)
    Proc,       (* t.{info,ref} == {NodeIndex,M3AST} of decl           *)
    Var,        (* t.{info,ref} == {NodeIndex,M3AST} of decl           *)
    GenericArg, (* t.{info,ref} == {NodeIndex,M3AST} of decl           *)
    Formal,     (* t.{info,ref} == {NodeIndex,M3AST} of decl           *)
    Module,     (* t.{info,ref} == {NodeIndex,M3AST} of decl           *)
    Builtin     (* t.info == ORD (M3Builtin.Proc)                      *)
  };

TYPE
  ImportOracle = OBJECT METHODS
    find (interface: M3ID.T): M3AST.T;
  END;

PROCEDURE Eval (ast: M3AST.T;  loc: M3AST.NodeIndex;  env: ImportOracle;
                VAR(*OUT*) val: T) RAISES {Error};
(* Evaluate and return in "val" the constant at node "loc" in "ast" using "env"
   to resolve any imported symbols. *)

PROCEDURE IsEQ (READONLY a, b: T): BOOLEAN;
(* Return "a = b". *)

EXCEPTION Error (TEXT);
   
END M3Const.
