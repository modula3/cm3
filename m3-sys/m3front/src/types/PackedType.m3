(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: PackedType.m3                                         *)
(* Last modified on Tue May 23 15:25:39 PDT 1995 by kalsow     *)
(*      modified on Fri Dec 21 01:25:20 1990 by muller         *)

MODULE PackedType;

IMPORT M3, CG, Word, Type, TypeRep, Error, Expr, Target;
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
        no_straddle:= IsStraddleFree;
        isEqual    := EqualChk;
        isSubtype  := Subtyper;
        compile    := Compiler;
        initCost   := InitCoster;
        initValue  := GenInit;
        mapper     := GenMap;
        gen_desc   := GenDesc;
        fprint     := FPrinter;
      END;

(* EXPORTED: *)
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

PROCEDURE GetPackedSize (p: P): INTEGER =
(* This mostly duplicates code in Check, but doesn't depend on the
   base type's having been checked, won't adjust for some errors, and
   may leave p.newSize as NO_SIZE (and return NO_SIZE).  If Check has
   happened, its result will prevail. *) 
  VAR newSize: INTEGER;  e: Expr.T;
  BEGIN
    IF (p.newSize = NO_SIZE) AND (p.sizeE # NIL) THEN
      e := Expr.ConstValue (p.sizeE);
      IF (e = NIL) OR NOT IntegerExpr.ToInt (e, newSize)
        THEN Error.Msg ("BITS FOR size must be a constant integer");
        ELSE p.sizeE := e;  p.newSize := newSize;
      END;
    END;
    RETURN p.newSize;
  END GetPackedSize;

(* EXPORTED: *)
PROCEDURE Split (t: Type.T;  VAR size: INTEGER;  VAR base: Type.T) =
  VAR p := Reduce (t);
  BEGIN
    size := GetPackedSize (p);
    base := p.baseType;
  END Split;

(* EXPORTED: *)
PROCEDURE Base (t: Type.T): Type.T =
  VAR p: P := t;
  BEGIN
    RETURN p.baseType;
  END Base;

(* Externally dispatched-to: *) 
PROCEDURE Check (p: P) =
  VAR
    cs := M3.OuterCheckState;
    baseInfo: Type.Info;
  VAR e: Expr.T;
  BEGIN
    p.baseType := Type.CheckInfo (p.baseType, baseInfo);
    
    IF (p.sizeE # NIL) THEN
      Expr.TypeCheck (p.sizeE, cs);
      e := Expr.ConstValue (p.sizeE);
      IF (e = NIL) OR NOT IntegerExpr.ToInt (e, p.newSize)
      THEN
        Error.Msg ("BITS FOR size must be a constant integer");
        p.newSize := baseInfo.size;
      ELSE
        p.sizeE := e;
        IF p.newSize < baseInfo.min_size THEN
          Error.Int
            (baseInfo.min_size, "BITS FOR size too small, must be at least");
          p.newSize := baseInfo.min_size; 
        END;
      END; 
    END;

    p.info.size      := p.newSize;
    p.info.min_size  := p.newSize;
    p.info.alignment := baseInfo.alignment; (* Inherit from base type. *) 
    p.info.mem_type  := baseInfo.mem_type;
    p.info.stk_type  := baseInfo.stk_type;
    p.info.class     := Type.Class.Packed;
    p.info.isTraced  := baseInfo.isTraced;
    p.info.isEmpty   := baseInfo.isEmpty;
    p.info.isSolid   := baseInfo.isSolid;
    p.info.hash      := Word.Plus (Word.Times (61, baseInfo.hash), p.newSize);
  END Check;

(* Externally dispatched-to: *) 
PROCEDURE IsStraddleFree
  (p: P;  offset: INTEGER; IsEltOrField: BOOLEAN): BOOLEAN =
  VAR z0: INTEGER;
      sz: INTEGER;
  BEGIN
    IF IsEltOrField THEN (* The BITS FOR affects allocation. *)  
      IF p.info.lazyAligned THEN z0 := offset DIV Target.Byte * Target.Byte;
      ELSE z0 := offset DIV Target.Word.align * Target.Word.align;
      END;
      sz := GetPackedSize (p);
      IF (offset + sz) <= (z0 + Target.Word.size)
      THEN (* The entire thing fits in a word. *) 
        RETURN TRUE; 
      ELSE (* It crosses a word boundary... *)
        RETURN FALSE;
        (* ^Insist that entire structured, packed component fit in a word. *) 
        IF Type.IsStructured (p) 
        THEN (* but maybe none of its scalar components do. *)  
          RETURN Type.StraddleFreeScalars (p.baseType, offset, IsEltOrField);
        ELSE RETURN FALSE
        END; 
      END;
    ELSE (* Not an element or field, just delegate to base type. *) 
      RETURN Type.StraddleFreeScalars (p.baseType, offset, FALSE);
    END; 
  END IsStraddleFree;

(* Externally dispatched-to: *) 
PROCEDURE Compiler (p: P) =
  BEGIN
    Type.Compile (p.baseType);
    CG.Declare_packed (Type.GlobalUID (p), p.newSize,
                       Type.GlobalUID (p.baseType));
  END Compiler;

(* Externally dispatched-to: *) 
PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN GetPackedSize (a) = GetPackedSize (b)
       AND Type.IsEqual (a.baseType, b.baseType, x);
  END EqualChk;

(* Externally dispatched-to: *) 
PROCEDURE Subtyper (a: P;  b: Type.T): BOOLEAN =
  BEGIN
    RETURN Type.IsEqual (b, a.baseType, NIL);
  END Subtyper;

(* Externally dispatched-to: *) 
PROCEDURE InitCoster (p: P;  zeroed: BOOLEAN): INTEGER =
  BEGIN
    RETURN Type.InitCost (p.baseType, zeroed);
  END InitCoster;

(* Externally dispatched-to: *) 
PROCEDURE GenInit (p: P;  zeroed: BOOLEAN) =
  BEGIN
    Type.InitValue (p.baseType, zeroed);  (* BUG!! *)
  END GenInit;

(* Externally dispatched-to: *) 
PROCEDURE GenMap (p: P;  offset, size: INTEGER;  refs_only: BOOLEAN) =
  BEGIN
    <*ASSERT size <= p.newSize*>
    Type.GenMap (p.baseType, offset, size, refs_only);
  END GenMap;

(* Externally dispatched-to: *) 
PROCEDURE GenDesc (p: P) =
  BEGIN
    IF TipeDesc.AddO (TipeDesc.Op.Packed, p) THEN
      TipeDesc.AddI (p.newSize);
      Type.GenDesc (p.baseType);
    END;
  END GenDesc;

(* Externally dispatched-to: *) 
PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  BEGIN
    M3Buf.PutText (x.buf, "BITS-FOR ");
    M3Buf.PutInt  (x.buf, p.newSize);
    x.n_nodes  := 1;
    x.nodes[0] := p.baseType;
  END FPrinter;

BEGIN
END PackedType.
