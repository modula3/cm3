(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ExprRep.i3                                            *)
(* Last Modified On Tue Jun 20 15:20:23 PDT 1995 By kalsow     *)
(*      Modified On Thu Jun 15 12:45:56 PDT 1995 By ericv      *)
(*      Modified On Thu Nov 29 03:45:22 1990 By muller         *)

INTERFACE ExprRep;

IMPORT M3, M3Buf, CG, Target, Type, Expr, MSIR, CaptureAnalysis;

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
    (* ^Puts literal value in global area of current unit, if it's constant and
       structured.  Does not leave address on CG stack.  *)
    prep         ()                                := NoPrep;
    compile      (StaticOnly: BOOLEAN)             := NoCompile;
    prepLV       (traced: BOOLEAN)                 := NotLValue;
    compileLV    (traced, StaticOnly: BOOLEAN)     := NotLValueBool;
    prepBR       (true, false: CG.Label;  freq: CG.Frequency) := NotBoolean;
    compileBR    (true, false: CG.Label;  freq: CG.Frequency) := NotBoolean;
    note_write   ()                                := NotWritable;
    capture         (ca: CaptureAnalysis.T)           := ExprCaptureDefault;
    (* Walk sub-expressions recording up-level variable reads in ca.
       The default is a no-op (correct for leaf exprs with no children). *)
    captureLV       (ca: CaptureAnalysis.T)           := ExprCaptureLVDefault;
    (* Like scan, but this expression appears in an lvalue context:
       a VarExpr/NamedExpr at the top of the tree marks its variable
       as written.  Default: delegates to scan (correct for non-designators
       and for designators whose top-level variable is not directly assigned,
       e.g. deref, field-of-heap-object). *)
    compileMSIR  (): MSIR.Value                    := MSIRDefault;
    (* Emit MSIR for this expression. Returns NIL on unsupported,
       in which case MSIRBuilder.Abandon has been called and the
       enclosing procedure will be dropped. *)
    compileLValueMSIR (): MSIR.Value               := LValueMSIRDefault;
    (* Emit MSIR address of this lvalue. Returns NIL on unsupported. *)
    exprAlign    (): Type.BitAlignT                := ExprAlignDefault;
    staticLength (): Expr.lengthTyp                := StaticLengthDefault;
    usesAssignProtocol (): BOOLEAN                 := UsesAssignProtocolDefault;
    checkUseFailure (): BOOLEAN                    := DefaultCheckUseFailure
  END;

TYPE Ta   = M3.Expr OBJECT a: M3.Expr     OVERRIDES isEqual := EqCheckA;
                                                    capture  := TaCapture;
                                                    captureLV := TaCaptureLV  END;
(* Ta.capture  recurses into a.  Ta.captureLV  also recurses into a (unary exprs
   are not designators, so the lvalue context does not propagate further). *)

TYPE Tab  = M3.Expr OBJECT a, b: M3.Expr  OVERRIDES isEqual := EqCheckAB;
                                                     capture  := TabCapture;
                                                     captureLV := TabCaptureLV END;
(* Tab.capture  recurses into a and b.  Tab.captureLV  recurses into a and b
   (binary exprs are not designators at the top level, except for
   SubscriptExpr which overrides scanLV to propagate into a). *)

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
PROCEDURE NoCompile      (e: M3.Expr; StaticOnly: BOOLEAN);
PROCEDURE NotLValue      (e: M3.Expr; traced: BOOLEAN);
PROCEDURE NotLValueBool  (e: M3.Expr; traced, StaticOnly: BOOLEAN);
PROCEDURE NotBoolean     (e: M3.Expr; t,f: CG.Label; freq: CG.Frequency);
PROCEDURE PrepNoBranch   (e: M3.Expr; t,f: CG.Label; freq: CG.Frequency);
PROCEDURE NoBranch       (e: M3.Expr; t,f: CG.Label; freq: CG.Frequency);
PROCEDURE NotWritable    (e: M3.Expr);
PROCEDURE MSIRDefault    (e: M3.Expr): MSIR.Value;
(* default: signals "not yet supported" by calling MSIRBuilder.Abandon
   and returning NIL. Subclasses override when they have a translation. *)
PROCEDURE LValueMSIRDefault (e: M3.Expr): MSIR.Value;
(* default lvalue: calls Abandon. Override in designator expressions. *)

PROCEDURE ExprCaptureDefault   (e: M3.Expr;  ca: CaptureAnalysis.T);
(* no-op: correct for leaf expressions with no sub-expressions *)
PROCEDURE ExprCaptureLVDefault (e: M3.Expr;  ca: CaptureAnalysis.T);
(* delegates to scan: correct when the expression is not itself a
   directly-assigned variable (deref, heap-field-qualify, etc.) *)
PROCEDURE TaCapture    (e: Ta;  ca: CaptureAnalysis.T);
PROCEDURE TaCaptureLV  (e: Ta;  ca: CaptureAnalysis.T);
PROCEDURE TabCapture   (e: Tab; ca: CaptureAnalysis.T);
PROCEDURE TabCaptureLV (e: Tab; ca: CaptureAnalysis.T);

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
PROCEDURE DefaultCheckUseFailure (e: M3.Expr): BOOLEAN;

PROCEDURE EqCheckA  (e: Ta;  x: M3.Expr;  z: M3.EqAssumption): BOOLEAN;
PROCEDURE EqCheckAB (e: Tab; x: M3.Expr;  z: M3.EqAssumption): BOOLEAN;


END ExprRep.

