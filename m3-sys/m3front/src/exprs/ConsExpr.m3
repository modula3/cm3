(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ConsExpr.m3                                           *)
(* Last modified on Tue Jun 20 15:46:56 PDT 1995 by kalsow     *)
(*      modified on Thu Jun 15 14:17:19 PDT 1995 by ericv      *)
(*      modified on Fri Dec 14 21:41:11 1990 by muller         *)

MODULE ConsExpr;
(* A value constructor.  Ascertaining whether it is an array, record,
   or set constructor requires semantic analysis, so this placeholder
   node is built during parsing.  It stays in the tree, but is given
   an ArrayExpr.T, RecordExpr.T, or SetExpr.T as a child (in field
   'base'), by Seal, later when  its type can be looked up and checked. *)

IMPORT M3, Expr, ExprRep, Error, ErrType, Type;
IMPORT TypeExpr, SetExpr, RecordExpr, ArrayExpr;

TYPE Kind = {Unknown, NonConstr, Record, Set, Array};

TYPE
  P = Expr.T BRANDED "ConsExpr.P" OBJECT
        typeExpr : Expr.T; (* An *expression* for the type being constructed. *)
        args     : Expr.List;
        base     : Expr.T;
        kind     : Kind;
        dots     : BOOLEAN;
        isNestedArrayConstr: BOOLEAN := FALSE;
        (* We're gonna want this someday. *)
      OVERRIDES
        typeOf       := TypeOf;
        repTypeOf    := RepTypeOf;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
        evaluate     := Evaluate;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := IsZeroes;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
        staticLength := StaticLength;
        usesAssignProtocol := UsesAssignProtocol;
        use          := Use;
      END;

(* EXPORTED: *)
PROCEDURE New (typeExpr: Expr.T;  args: Expr.List;  dots: BOOLEAN): Expr.T =
  VAR p := NEW (P);
  BEGIN
    ExprRep.Init (p);
    p.typeExpr  := typeExpr;
    p.args      := args;
    p.dots      := dots;
    p.base      := NIL;
    p.kind      := Kind.Unknown;
    p.directAssignableType := TRUE;
    RETURN p;
  END New;

(* EXPORTED: *)
PROCEDURE Is (e: Expr.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P    => RETURN TRUE;
    ELSE      RETURN FALSE;
    END;   
  END Is;

(* EXPORTED: *)
PROCEDURE Base (e: Expr.T): Expr.T =
(* PRE: Seal (e) or Expr.TypeCheck (e) has been called. *)
  BEGIN
    TYPECASE e OF
    | NULL => RETURN NIL;
    | P(p) => RETURN p.base;
    ELSE      RETURN NIL;
    END;   
  END Base;

(* Externally dispatched-to: *)
PROCEDURE TypeOf (p: P): Type.T =
  VAR ta: Type.T;
  BEGIN
    IF TypeExpr.Split (p.typeExpr, ta)
    THEN RETURN ta;
    ELSE RETURN Expr.TypeOf (p.typeExpr);
    END;
  END TypeOf;

(* EXPORTED: *)
PROCEDURE Seal (e: Expr.T) =
(* POST: Base will now return a valid result. *)
  BEGIN
    TYPECASE e OF
    | NULL =>
    | P(p) => InnerSeal (p)
    ELSE
    END
  END Seal;

PROCEDURE InnerSeal (p: P) =
(* POST: Base will now return a valid result. *)
  VAR consType: Type.T;  consTypeInfo: Type.Info;
  BEGIN
    IF (p.base # NIL) THEN RETURN END;
    IF NOT TypeExpr.Split (p.typeExpr, consType) THEN RETURN END;
    consType := Type.StripPacked (consType);  (* strip named and BITS FOR. *)
    consType := Type.CheckInfo (consType, consTypeInfo);
    IF (consType = NIL) THEN (* Prior error *)
    ELSE
      CASE consTypeInfo.class OF
      | Type.Class.Record =>
        p.base := RecordExpr.New (consType, p.args);
        p.kind := Kind.Record;
      | Type.Class.Set =>
        p.base := SetExpr.New (consType, p.args);
        p.kind := Kind.Set;
      | Type.Class.Array, Type.Class.OpenArray =>
        p.base := ArrayExpr.New (consType, p.args, p.dots);
        p.kind := Kind.Array;
      ELSE p.kind := Kind.Unknown
      END (*CASE*);
    END
  END InnerSeal;

PROCEDURE CheckRecurse
(* Recurses only on a Cons/Array pair directly inside a Cons/Array pair. *)
  (consExpr: P; parentKind: Kind; VAR cs: Expr.CheckState) =
  BEGIN
    InnerSeal (consExpr);
    IF consExpr.kind = Kind.Unknown THEN
      Error.Msg ("constructor type must be array, record, or set type");
      consExpr.type := ErrType.T;
    ELSE
      IF parentKind = Kind.Array AND consExpr.kind = Kind.Array THEN
        consExpr.isNestedArrayConstr := TRUE;
        ArrayExpr.NoteNested (consExpr.base);
      END;
      Expr.TypeCheck (consExpr.typeExpr, cs);
      consExpr.type := TypeOf (consExpr);
      IF consExpr.dots AND consExpr.kind # Kind.Array THEN
        Error.Msg ("trailing \'..\' in constructor, ignored");
      END;

      FOR i := 0 TO LAST (consExpr.args^) DO
        WITH argExpr = consExpr.args^[i] DO
          TYPECASE argExpr OF
          | NULL =>
          | P (argCons) =>
            (* Does not include named CONST w/ array constructor as its value. *)
            CheckRecurse (argCons, consExpr.kind, cs);
          ELSE Expr.TypeCheck (argExpr, cs)
          END
        END
      END
    END;
    Expr.TypeCheck (consExpr.base, cs);
    consExpr.checked := TRUE;
  END CheckRecurse;

(* Externally dispatched-to: *)
PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  BEGIN
    CheckRecurse (p, Kind.NonConstr, cs);
  END Check;

(* Externally dispatched-to: *)
PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    InnerSeal (a);
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => InnerSeal (b);  RETURN Expr.IsEqual (a.base, b.base, x);
    ELSE      RETURN Expr.IsEqual (a.base, e, x);
    END;
  END EqCheck;

(* Externally dispatched-to: *)
PROCEDURE NeedsAddress (p: P) =
  BEGIN
    InnerSeal (p);
    Expr.NeedsAddress (p.base);
  END NeedsAddress;

(* Externally dispatched-to: *)
PROCEDURE Prep (p: P) =
  VAR t: Type.T;
  BEGIN
    InnerSeal (p);
    IF TypeExpr.Split (p.typeExpr, t) THEN Type.Compile (t) END;
    Expr.Prep (p.base);
  END Prep;

(* Externally dispatched-to: *)
PROCEDURE Compile (p: P) =
  BEGIN
    Expr.Compile (p.base);
  END Compile;

(* Externally dispatched-to: *)
PROCEDURE Evaluate (p: P): Expr.T =
  BEGIN
    InnerSeal (p);
    RETURN Expr.ConstValue (p.base);
  END Evaluate;

(* Externally dispatched-to: *)
PROCEDURE IsZeroes (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    InnerSeal (p);
    RETURN Expr.IsZeroes (p.base);
  END IsZeroes;

(* Externally dispatched-to: *)
PROCEDURE RepTypeOf (p: P): Type.T =
  BEGIN
    IF p = NIL THEN RETURN ErrType.T END;
    InnerSeal (p);
    RETURN p.base.repTypeOf () (* Delegate.*);
  END RepTypeOf;

(* Externally dispatched-to: *)
PROCEDURE StaticLength (p: P): Expr.lengthTyp =
  BEGIN
    IF p = NIL THEN RETURN Expr.lengthInvalid END;
    InnerSeal (p);
    RETURN p.base.staticLength () (* Delegate.*);
  END StaticLength;

(* Externally dispatched-to: *)
PROCEDURE UsesAssignProtocol (p: P): BOOLEAN =
  BEGIN
    IF p = NIL THEN RETURN FALSE END;
    InnerSeal (p);
    RETURN p.base.usesAssignProtocol () (* Delegate.*);
  END UsesAssignProtocol;

(* Externally dispatched-to: *)
PROCEDURE Use (p: P): BOOLEAN =
  BEGIN
    IF p = NIL THEN RETURN TRUE END;
    InnerSeal (p);
    RETURN p.base.use () (* Delegate.*);
  END Use;

BEGIN
END ConsExpr.
