(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: PackedType.m3                                         *)
(* Last modified on Tue May 23 15:25:39 PDT 1995 by kalsow     *)
(*      modified on Fri Dec 21 01:25:20 1990 by muller         *)

MODULE PackedType;

IMPORT M3, CG, Word, Type, TypeRep, Error, Expr, Target, TInt;
IMPORT M3Buf, Token, IntegerExpr, Scanner, TipeDesc;

CONST
  NO_SIZE = -1;

TYPE
  P = Type.T OBJECT
        sizeE      : Expr.T;
        newSize    : INTEGER;
        baseType   : Type.T;
      OVERRIDES
        check      := Check;
        check_align:= CheckAlign;
        isEqual    := EqualChk;
        isSubtype  := Subtyper;
        compile    := Compiler;
        initCost   := InitCoster;
        initValue  := GenInit;
        mapper     := GenMap;
        gen_desc   := GenDesc;
        fprint     := FPrinter;
      END;

PROCEDURE Parse (): Type.T =
  TYPE TK = Token.T;
  VAR p: P := New (NO_SIZE, NIL);
  BEGIN
    Scanner.Match (TK.tBITS);
    p.sizeE := Expr.Parse ();
    Scanner.Match (TK.tFOR);
    p.baseType := Type.Parse ();
    RETURN p;
  END Parse;

PROCEDURE New (size: INTEGER;  base: Type.T): Type.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    TypeRep.Init (p, Type.Class.Packed);
    p.sizeE    := NIL;
    p.newSize  := size;
    p.baseType := base;
    RETURN p;
  END New;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.info.class = Type.Class.Named) THEN t := Type.Strip (t) END;
    IF (t.info.class # Type.Class.Packed) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

PROCEDURE GetSize (p: P): INTEGER =
  VAR new_sz: INTEGER;  new_size: Target.Int;  e: Expr.T;
  BEGIN
    IF (p.newSize = NO_SIZE) AND (p.sizeE # NIL) THEN
      e := Expr.ConstValue (p.sizeE);
      IF (e = NIL) OR NOT IntegerExpr.Split (e, new_size)
        OR NOT TInt.ToInt (new_size, new_sz)
        THEN Error.Msg ("BITS FOR size must be a constant integer");
        ELSE p.sizeE := e;  p.newSize := new_sz;
      END;
    END;
    RETURN p.newSize;
  END GetSize;

PROCEDURE Split (t: Type.T;  VAR size: INTEGER;  VAR base: Type.T) =
  VAR p := Reduce (t);
  BEGIN
    size := GetSize (p);
    base := p.baseType;
  END Split;

PROCEDURE Base (t: Type.T): Type.T =
  VAR p: P := t;
  BEGIN
    RETURN p.baseType;
  END Base;

PROCEDURE Check (p: P) =
  VAR
    new_sz, old_min: INTEGER;
    cs := M3.OuterCheckState;
    info: Type.Info;
  BEGIN
    p.baseType := Type.CheckInfo (p.baseType, info);
    old_min := info.min_size;
    new_sz  := info.size;

    IF (p.sizeE # NIL) THEN
      Expr.TypeCheck (p.sizeE, cs);
      new_sz := GetSize (p);
      IF (new_sz = NO_SIZE) THEN new_sz := info.size; END;
    END;

    IF (new_sz < old_min) THEN
      Error.Int (old_min, "BITS FOR size too small, must be at least");
    END;

    p.info.size      := new_sz;
    p.info.min_size  := new_sz;
    p.info.alignment := info.alignment;
    p.info.mem_type  := info.mem_type;
    p.info.stk_type  := info.stk_type;
    p.info.class     := Type.Class.Packed;
    p.info.isTraced  := info.isTraced;
    p.info.isEmpty   := info.isEmpty;
    p.info.isSolid   := info.isSolid;
    p.info.hash      := Word.Plus (Word.Times (61, info.hash), new_sz);
  END Check;

PROCEDURE CheckAlign (p: P;  offset: INTEGER): BOOLEAN =
  VAR z0: INTEGER;  info: Type.Info;  sz: INTEGER;
  BEGIN
    EVAL Type.CheckInfo (p.baseType, info);
    sz := GetSize (p);
    IF (info.size = sz) THEN
      RETURN Type.IsAlignedOk (p.baseType, offset);
    ELSIF Type.IsStructured (p.baseType) THEN
      (* the scalar crossing can't be any worse than in the full structure *)
      RETURN Type.IsAlignedOk (p.baseType, offset);
    ELSE
      z0 := offset DIV Target.Integer.align * Target.Integer.align;
      RETURN (offset + sz) <= (z0 + Target.Integer.size);
    END;
  END CheckAlign;

PROCEDURE Compiler (p: P) =
  BEGIN
    Type.Compile (p.baseType);
    CG.Declare_packed (Type.GlobalUID (p), p.newSize,
                       Type.GlobalUID (p.baseType));
  END Compiler;

PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN GetSize (a) = GetSize (b)
       AND Type.IsEqual (a.baseType, b.baseType, x);
  END EqualChk;

PROCEDURE Subtyper (a: P;  b: Type.T): BOOLEAN =
  BEGIN
    RETURN Type.IsEqual (b, a.baseType, NIL);
  END Subtyper;

PROCEDURE InitCoster (p: P;  zeroed: BOOLEAN): INTEGER =
  BEGIN
    RETURN Type.InitCost (p.baseType, zeroed);
  END InitCoster;

PROCEDURE GenInit (p: P;  zeroed: BOOLEAN) =
  BEGIN
    Type.InitValue (p.baseType, zeroed);  (* BUG!! *)
  END GenInit;

PROCEDURE GenMap (p: P;  offset, size: INTEGER;  refs_only: BOOLEAN) =
  BEGIN
    <*ASSERT size <= p.newSize*>
    Type.GenMap (p.baseType, offset, size, refs_only);
  END GenMap;

PROCEDURE GenDesc (p: P) =
  BEGIN
    IF TipeDesc.AddO (TipeDesc.Op.Packed, p) THEN
      TipeDesc.AddI (p.newSize);
      Type.GenDesc (p.baseType);
    END;
  END GenDesc;

PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  BEGIN
    M3Buf.PutText (x.buf, "BITS-FOR ");
    M3Buf.PutInt  (x.buf, p.newSize);
    x.n_nodes  := 1;
    x.nodes[0] := p.baseType;
  END FPrinter;

BEGIN
END PackedType.
