(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ValueRep.i3                                           *)
(* Last modified on Wed Mar  1 08:47:26 PST 1995 by kalsow     *)
(*      modified on Fri Feb 23 03:41:40 1990 by muller         *)

INTERFACE ValueRep;

IMPORT M3, M3ID, Value;

REVEAL
  M3.Value = M3.ValueNode BRANDED "Value.T" OBJECT
    name       : M3ID.T;
    extName    : M3ID.T;
    scope      : M3.Scope;
    vnext      : M3.Value; (* linked list of all values in the same module *)
    checkDepth : BITS 12 FOR [-2048..2047];
    class      : BITS 4 FOR Value.Class;
    checked    : M3.Flag;
    readonly   : M3.Flag;
    external   : M3.Flag;
    unused     : M3.Flag;
    obsolete   : M3.Flag;
    up_level   : M3.Flag;
    error      : M3.Flag; (* reset in each compilation *)
    used       : M3.Flag; (* reset in each compilation *)
    declared   : M3.Flag; (* reset in each compilation *)
    inited     : M3.Flag; (* reset in each compilation *)
    compiled   : M3.Flag; (* reset in each compilation *)
    imported   : M3.Flag; (* reset in each compilation *)
    exported   : M3.Flag; (* reset in each compilation *)
    exportable : M3.Flag; (* reset in each compilation *)
    inTypeOf   : M3.Flag; (* reset in each compilation *)
    inToExpr   : M3.Flag; (* reset in each compilation *)
    inToType   : M3.Flag; (* reset in each compilation *)
  METHODS
    typeCheck   (VAR cs: M3.CheckState);
    set_globals ()                        := NoInit;
    load        ()                        := NoLoader;
    declare     (): BOOLEAN               := Never;
    const_init  ()                        := NoInit;
    need_init   (): BOOLEAN               := Never;
    lang_init   ()                        := NoInit;
    user_init   ()                        := NoInit;
    toExpr      (): M3.Expr               := NoExpr;
    toType      (): M3.Type               := NoType;
    typeOf      (): M3.Type               := NoType;
    base        (): M3.Value              := Self;
    add_fp_tag  (VAR x: M3.FPInfo): CARDINAL;
    fp_type     (): M3.Type;
  END;

PROCEDURE Init (v: M3.Value;  name: M3ID.T;  c: Value.Class);
(* initializes a Value.T *)

PROCEDURE FPStart (v: M3.Value;  VAR x: M3.FPInfo; tag: TEXT;
                   offset: INTEGER;  global: BOOLEAN);
(* add "tag", the name, external name, and offset of 'v' to 'x's buffer *)

PROCEDURE NoExpr (v: M3.Value): M3.Expr;
(* == ASSERT (FALSE) *)

PROCEDURE NoType (v: M3.Value): M3.Type;
(* == ASSERT (FALSE) *)

PROCEDURE NoLoader (v: M3.Value);
(* == ASSERT (FALSE) *)

PROCEDURE Never (v: M3.Value): BOOLEAN;
(* returns FALSE *)

PROCEDURE NoInit (v: M3.Value);
(* == noop *)

PROCEDURE Always (v: M3.Value): BOOLEAN;
(* returns TRUE *)

PROCEDURE TypeVoid (v: M3.Value): M3.Type;
(* == RETURN Void.T *)

PROCEDURE Self (v: M3.Value): M3.Value;
(* == RETURN v *)

END ValueRep.
