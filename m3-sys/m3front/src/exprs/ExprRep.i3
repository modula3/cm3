(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ExprRep.i3                                            *)
(* Last Modified On Tue Jun 20 15:20:23 PDT 1995 By kalsow     *)
(*      Modified On Thu Jun 15 12:45:56 PDT 1995 By ericv      *)
(*      Modified On Thu Nov 29 03:45:22 1990 By muller         *)

INTERFACE ExprRep;

IMPORT M3, M3Buf, CG, Target, Type, Expr;

REVEAL
  M3.Expr = M3.Node BRANDED "Expr.T" OBJECT
    type                 : M3.Type;
    repType              : M3.Type;
    align                : INTEGER; 
    checked              : BOOLEAN;
    directAssignableType : BOOLEAN;
    doDirectAssign       : BOOLEAN;
    isNamedConst         : BOOLEAN;
  METHODS
    typeOf       (): M3.Type                       := NoType;
    repTypeOf    (): M3.Type                       := NoType;
    check        (VAR cs: M3.CheckState)           := NoCheck;
    isEqual      (e: M3.Expr; x: M3.EqAssumption): BOOLEAN := NeverEq;
    evaluate     (): M3.Expr                       := NoValue;
    getBounds    (VAR min, max: Target.Int)        := NoBounds;
    isWritable   (lhs: BOOLEAN): BOOLEAN           := IsNever;
    isDesignator (<*UNUSED*> lhs := FALSE): BOOLEAN:= IsNever;
    isZeroes     (<*UNUSED*> lhs := FALSE): BOOLEAN:= IsNever;
    need_addr    ()                                := NotAddressable;
    genFPLiteral (mbuf: M3Buf.T)                   := NoFPLiteral;
    prepLiteral  (type: M3.Type;  is_const: BOOLEAN) := NoPrepLiteral;
    genLiteral   (offset: INTEGER;  type: M3.Type;  is_const: BOOLEAN) := NoLiteral;
    prep         ()                                := NoCompile;
    compile      ()                                := NoCompile;
    prepLV       (traced: BOOLEAN)                 := NotLValue;
    compileLV    (traced: BOOLEAN)                 := NotLValue;
    prepBR       (true, false: CG.Label;  freq: CG.Frequency) := NotBoolean;
    compileBR    (true, false: CG.Label;  freq: CG.Frequency) := NotBoolean;
    note_write   ()                                := NotWritable;
    exprAlign    (): Type.BitAlignT                := ExprAlignDefault;
    staticLength (): Expr.lengthTyp                := StaticLengthDefault;
    usesAssignProtocol (): BOOLEAN                 := UsesAssignProtocolDefault;
    use (): BOOLEAN                                := DefaultUse
  END;

TYPE Ta   = M3.Expr OBJECT a: M3.Expr     OVERRIDES isEqual := EqCheckA  END;
TYPE Tab  = M3.Expr OBJECT a, b: M3.Expr  OVERRIDES isEqual := EqCheckAB END;
TYPE Tabc = Tab     OBJECT class: INTEGER OVERRIDES isEqual := EqCheckAB END;

PROCEDURE Init (e: M3.Expr);
(* initializes the common part of an Expr.T *)

(* misc. useful methods *)
PROCEDURE NotAddressable (e: M3.Expr);
PROCEDURE NoType         (e: M3.Expr): M3.Type;
PROCEDURE NoCheck        (e: M3.Expr;  VAR cs: M3.CheckState);
PROCEDURE NoValue        (e: M3.Expr): M3.Expr;
PROCEDURE Self           (e: M3.Expr): M3.Expr;
PROCEDURE NoBounds       (e: M3.Expr;  VAR min, max: Target.Int);
PROCEDURE IsNever        (e: M3.Expr;  lhs: BOOLEAN): BOOLEAN;
PROCEDURE IsAlways       (e: M3.Expr;  lhs: BOOLEAN): BOOLEAN;
PROCEDURE NeverEq        (e: M3.Expr; x: M3.Expr; z: M3.EqAssumption): BOOLEAN;
PROCEDURE NoFPLiteral    (e: M3.Expr;  mbuf: M3Buf.T);
PROCEDURE NoPrepLiteral  (e: M3.Expr;  type: M3.Type;  is_const: BOOLEAN);
PROCEDURE NoLiteral      (e: M3.Expr;  offset: INTEGER;  type: M3.Type;  is_const: BOOLEAN);
PROCEDURE NoPrep         (e: M3.Expr);
PROCEDURE NoCompile      (e: M3.Expr);
PROCEDURE NotLValue      (e: M3.Expr; traced: BOOLEAN);
PROCEDURE NotBoolean     (e: M3.Expr; t,f: CG.Label; freq: CG.Frequency);
PROCEDURE PrepNoBranch   (e: M3.Expr; t,f: CG.Label; freq: CG.Frequency);
PROCEDURE NoBranch       (e: M3.Expr; t,f: CG.Label; freq: CG.Frequency);
PROCEDURE NotWritable    (e: M3.Expr);

(* Multi-use overrides for exprAlign:  *)
PROCEDURE ExprAlignDefault (e: M3.Expr): Type.BitAlignT;
  (* ^Take it from the type.  Strip packed. *) 
PROCEDURE ExprAddrAlign    (e: M3.Expr): Type.BitAlignT; 
PROCEDURE ExprBoolAlign    (e: M3.Expr): Type.BitAlignT;
PROCEDURE ExprIntAlign     (e: M3.Expr): Type.BitAlignT;
PROCEDURE ExprAlignArg0    (e: Ta): Type.BitAlignT;
  (* ^Inherit alignment from argument zero. *)  

PROCEDURE StaticLengthDefault (e: M3.Expr): Expr.lengthTyp;
PROCEDURE UsesAssignProtocolDefault (e: M3.Expr): BOOLEAN;
PROCEDURE DefaultUse (e: M3.Expr): BOOLEAN;

PROCEDURE EqCheckA  (e: Ta;  x: M3.Expr;  z: M3.EqAssumption): BOOLEAN;
PROCEDURE EqCheckAB (e: Tab; x: M3.Expr;  z: M3.EqAssumption): BOOLEAN;


END ExprRep.

