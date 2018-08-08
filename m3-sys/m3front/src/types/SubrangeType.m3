(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: SubrangeType.m3                                       *)
(* Last modified on Tue May 23 15:28:34 PDT 1995 by kalsow     *)
(*      modified on Thu Jan 31 23:22:08 1991 by muller         *)

MODULE SubrangeType;

IMPORT M3, CG, Type, TypeRep, Int, LInt, Expr, Token, Card, M3Buf;
IMPORT Error, IntegerExpr, EnumExpr, Word, TipeMap, TipeDesc;
IMPORT Target, TInt, TWord, TargetMap, LCard;
IMPORT Charr, WCharr; 
FROM Scanner IMPORT Match;

TYPE 
  P = Type.T BRANDED "SubrangeType.T" OBJECT
        baseType   : Type.T;
        minE, maxE : Expr.T;
        min,  max  : Target.Int;
        rep        : CG.Type;
        builtin    : BOOLEAN;  (* => CARDINAL *)
        sealed     : BOOLEAN;
      OVERRIDES
        check      := Check;
        no_straddle:= NoStraddle;
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
  VAR p: P := New (TInt.Zero, TInt.MOne, NIL, FALSE);
  BEGIN
    Match (TK.tLBRACKET);
    p.minE := Expr.Parse ();
    Match (TK.tDOTDOT);
    p.maxE := Expr.Parse ();
    Match (TK.tRBRACKET);
    RETURN p;
  END Parse;

PROCEDURE New (READONLY min, max: Target.Int;
               base: Type.T;  builtin: BOOLEAN): Type.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    TypeRep.Init (p, Type.Class.Subrange);
    p.baseType := base;
    p.min      := min;
    p.max      := max;
    p.builtin  := builtin;
    RETURN p;
  END New;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.info.class = Type.Class.Named) THEN t := Type.Strip (t) END;
    IF (t.info.class # Type.Class.Subrange) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

PROCEDURE Split (t: Type.T;  VAR min, max: Target.Int): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    min := p.min;
    max := p.max;
    RETURN TRUE;
  END Split;

PROCEDURE SetRep (p: P) =
  BEGIN
    IF TInt.LT (p.max, p.min) THEN
      p.min := TInt.Zero;
      p.max := TInt.MOne;
      IF Type.IsSubtype (p.baseType, LInt.T)
        THEN p.rep := Target.Longint.cg_type;
        ELSE p.rep := Target.Integer.cg_type;
      END;
      RETURN;
    END;

    IF TInt.LE (TInt.Zero, p.min) THEN
      (* look for an unsigned type *)
      FOR i := FIRST (TargetMap.Word_types) TO LAST (TargetMap.Word_types) DO
        WITH z = TargetMap.Word_types[i] DO
          IF TWord.LE (p.max, z.max) THEN
            p.rep := z.cg_type; RETURN;
          END;
        END;
      END;
    ELSE
      (* look for a signed type *)
      FOR i := FIRST (TargetMap.Integer_types) TO LAST (TargetMap.Integer_types) DO
        WITH z = TargetMap.Integer_types[i] DO
          IF TInt.LE (z.min, p.min) AND TInt.LE (p.max, z.max) THEN
            p.rep := z.cg_type; RETURN;
          END;
        END;
      END;
    END;
  END SetRep;

PROCEDURE Seal (p: P) =
  VAR emin, emax: Expr.T;  tmin, tmax: Type.T;
  BEGIN
    IF (p.sealed) THEN RETURN END;
    IF (p.minE # NIL) THEN
      emin := Expr.ConstValue (p.minE);
      IF (emin = NIL) THEN
        Error.Msg ("subrange lower bound is not constant");
        p.min := TInt.Zero;  tmin := Int.T;
      ELSIF IntegerExpr.Split (emin, p.min, tmin) THEN
        (* ok *)
      ELSIF EnumExpr.Split (emin, p.min, tmin) THEN
        (* ok *)
      ELSE
        Error.Msg ("subrange lower bound is not an ordinal value");
        p.min := TInt.Zero;  tmin := Int.T;
      END;

      emax := Expr.ConstValue (p.maxE);
      IF (emax = NIL) THEN
        Error.Msg ("subrange upper bound is not constant");
        p.max := p.min;  tmax := tmin;
      ELSIF IntegerExpr.Split (emax, p.max, tmax) THEN
        (* ok *)
      ELSIF EnumExpr.Split (emax, p.max, tmax) THEN
        (* ok *)
      ELSE
        Error.Msg ("subrange upper bound is not an ordinal value");
        p.max := p.min;  tmax := tmin;
      END;

      p.baseType := tmin;
      IF NOT Type.IsEqual (tmin, tmax, NIL) THEN
        Error.Msg ("subrange endpoints must be of same type");
      END;
    END;

    SetRep (p);
    p.sealed := TRUE;
  END Seal;

PROCEDURE Check (p: P) =
  VAR hash: INTEGER;  cs := M3.OuterCheckState;  i: INTEGER;  baseInfo: Type.Info;
  BEGIN
    Seal (p);
    Expr.TypeCheck (p.minE, cs);
    Expr.TypeCheck (p.maxE, cs);
    p.baseType := Type.CheckInfo (p.baseType, baseInfo);

    hash := baseInfo.hash;
    IF NOT TInt.ToInt (p.min, i) THEN i := 19 END;
    hash := Word.Plus (Word.Times (hash, 487), i);
    IF NOT TInt.ToInt (p.max, i) THEN i := 23 END;
    hash := Word.Plus (Word.Times (hash, 487), i);

    p.info.size      := TargetMap.CG_Size[p.rep];
    p.info.min_size  := MinSize (p);
    p.info.alignment := TargetMap.CG_Align[p.rep];
    p.info.mem_type  := p.rep;
    IF Type.IsSubtype (p.baseType, LInt.T) THEN
      IF Target.SignedType [p.rep]
        THEN p.info.stk_type := Target.Longint.cg_type;
        ELSE p.info.stk_type := Target.Long.cg_type;
      END;
    ELSE
      IF Target.SignedType [p.rep]
        THEN p.info.stk_type := Target.Integer.cg_type;
        ELSE p.info.stk_type := Target.Word.cg_type;
      END;
    END;
    p.info.class     := Type.Class.Subrange;
    p.info.isTraced  := FALSE;
    p.info.isSolid   := TRUE;
    p.info.isEmpty   := TInt.LT (p.max, p.min);
    p.info.hash      := hash;
  END Check;

PROCEDURE NoStraddle
  (p: P;  offset: INTEGER; <*UNUSED*> IsEltOrField: BOOLEAN): BOOLEAN =
  VAR
    sz := TargetMap.CG_Size[p.rep];
    z0: INTEGER;
  BEGIN
    IF p.info.lazyAligned THEN
      z0 := offset DIV Target.Byte * Target.Byte;
    ELSE
      z0 := offset DIV Target.Integer.align * Target.Integer.align;
    END;
    RETURN (offset + sz) <= (z0 + Target.Integer.size);
  END NoStraddle;

PROCEDURE Compiler (p: P) =
  BEGIN
    Type.Compile (p.baseType);
    CG.Declare_subrange (Type.GlobalUID (p), Type.GlobalUID (p.baseType),
                         p.min, p.max, TargetMap.CG_Size[p.rep]);
  END Compiler;

PROCEDURE Base (t: Type.T): Type.T =
  VAR p: P := t;
  BEGIN
    Seal (p);
    t := p.baseType;
    IF (t = NIL) THEN t := Expr.TypeOf (p.minE) END;
    RETURN t
  END Base;

PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  VAR b: P := t;
  BEGIN
    Seal (a);  Seal (b);
    RETURN (a.builtin = b.builtin)
       AND TInt.EQ (a.min, b.min)
       AND TInt.EQ (a.max, b.max)
       AND Type.IsEqual (a.baseType, b.baseType, x);
  END EqualChk;

PROCEDURE Subtyper (a: P;  t: Type.T): BOOLEAN =
  VAR b: P;
  VAR aBase, tBase: Type.T;
  BEGIN
    Seal (a);
    aBase := Type.Base (a.baseType); 
    tBase := Type.Base (t); 
    IF Type.IsEqual (aBase, tBase, NIL) THEN
      IF t.info.class # Type.Class.Subrange THEN RETURN TRUE END;
      (* Bases are equal, both are subranges. *) 
    ELSIF Type.IsEqual (aBase, Charr.T, NIL) THEN 
      IF Type.IsEqual (t, WCharr.T, NIL) THEN RETURN TRUE;
      ELSIF NOT Type.IsEqual (tBase, WCharr.T, NIL) THEN RETURN FALSE 
      END; 
      (* Lhs subrange of CHAR, Rhs subrange of WIDECHAR. *)  
    ELSIF NOT Type.IsEqual (aBase, WCharr.T, NIL) 
          OR NOT Type.IsEqual (tBase, Charr.T, NIL) THEN 
      RETURN FALSE 
    ELSIF Type.IsEqual(t, Charr.T, NIL) THEN
      RETURN TInt.LE(a.max, TWord.Max8); 
      (* Lhs subrange of WIDECHAR, Rhs subrange of CHAR. *)  
    END; 
    IF TInt.LT (a.max, a.min) THEN (* a is empty *) RETURN TRUE END;
    b := t;
    RETURN TInt.LE (b.min, a.min) AND TInt.LE (a.max, b.max);
  END Subtyper;

PROCEDURE MinSize (p: P): INTEGER =
  VAR z1, z2: INTEGER;  n1, n2: BOOLEAN;
  BEGIN
    (* compute the minimum size of these elements *)
    IF TInt.LT (p.max, p.min) THEN RETURN 0 END;
    BitWidth (p.min, z1, n1);
    BitWidth (p.max, z2, n2);
    z1 := MAX (z1, z2);
    IF (n1 OR n2) THEN INC (z1); END;
    IF Type.IsSubtype (p.baseType, LInt.T)
      THEN RETURN MIN (z1, Target.Longint.size);
      ELSE RETURN MIN (z1, Target.Integer.size);
    END;
  END MinSize;

PROCEDURE BitWidth (n: Target.Int;  VAR width: INTEGER;  VAR neg: BOOLEAN) =
  (***  valid for  Target.Longint.min <= n <= Target.Longint.max ***)
  VAR tmp: Target.Int;
  BEGIN
    neg := TInt.LT (n, TInt.Zero);
    IF (neg) THEN
      IF NOT TInt.Add (n, TInt.One, tmp)
        OR NOT TInt.Subtract (TInt.Zero, tmp, n) THEN
        (* value too large??? *)
        width := Target.Longint.size;
        RETURN;
      END;
    END;

    IF NOT powers_done THEN BuildPowerTables () END;
    width := Target.Longint.size;
    FOR i := 0 TO LAST (power) DO
      IF TInt.LE (n, power[i]) THEN width := i;  EXIT END;
    END;
  END BitWidth;

VAR (*CONST*)
  power : ARRAY [0..TInt.Size] OF Target.Int;
  powers_done := FALSE;

PROCEDURE BuildPowerTables () =
  BEGIN
    power [0] := TInt.One;
    FOR i := 1 TO LAST (power) DO
      IF NOT TInt.Add (power[i-1], power[i-1], power[i]) THEN
        power[i] := Target.Longint.max;
      END;
    END;
    powers_done := TRUE;
  END BuildPowerTables;

PROCEDURE InitCoster (p: P;  zeroed: BOOLEAN): INTEGER =
  VAR rep_min, rep_max: Target.Int;  size: INTEGER;
  BEGIN
    Seal (p);

    IF Type.IsSubtype (p.baseType, LInt.T) THEN
      size := Target.Longint.size;
      rep_min := Target.Longint.min;
      rep_max := Target.Longint.max;
    ELSE
      size := Target.Integer.size;
      rep_min := Target.Integer.min;
      rep_max := Target.Integer.max;
    END;

    IF zeroed
      AND TInt.LE (p.min, TInt.Zero)
      AND TInt.LE (TInt.Zero, p.max) THEN
      RETURN 0;
    END;

    IF (TargetMap.CG_Size[p.rep] = size)
      AND NOT (TInt.EQ (p.min, rep_min) AND TInt.EQ (p.max, rep_max)) THEN
      (* this rep uses a full integer word, but doesn't actually
         use all possible bit patterns => unsigned word =>  CARDINAL *)
      RETURN 1;
    END;

    CASE p.rep OF
    | CG.Type.Word8  => rep_min := Target.Word8.min;  rep_max := Target.Word8.max;
    | CG.Type.Word16 => rep_min := Target.Word16.min; rep_max := Target.Word16.max;
    | CG.Type.Word32 => rep_min := Target.Word32.min; rep_max := Target.Word32.max;
    | CG.Type.Word64 => rep_min := Target.Word64.min; rep_max := Target.Word64.max;
    | CG.Type.Int8   => rep_min := Target.Int8.min;   rep_max := Target.Int8.max;
    | CG.Type.Int16  => rep_min := Target.Int16.min;  rep_max := Target.Int16.max;
    | CG.Type.Int32  => rep_min := Target.Int32.min;  rep_max := Target.Int32.max;
    | CG.Type.Int64  => rep_min := Target.Int64.min;  rep_max := Target.Int64.max;
    ELSE                rep_min := TInt.Zero;         rep_max := TInt.MOne;
    END;

    IF TInt.EQ (p.min, rep_min) AND TInt.EQ (p.max, rep_max)
      THEN RETURN 0;
      ELSE RETURN 1;
    END;
  END InitCoster;

PROCEDURE GenInit (p: P;  zeroed: BOOLEAN) =
  VAR info: Type.Info;
  BEGIN
    EVAL Type.CheckInfo (p, info);
    IF TInt.LT (TInt.Zero, p.min) OR TInt.LT (p.max, TInt.Zero) THEN
      CG.Load_integer (info.stk_type, p.min);
      CG.Store_indirect (info.stk_type, 0, info.size);
    ELSIF zeroed THEN
      CG.Discard (CG.Type.Addr);
    ELSE
      CG.Load_integer (info.stk_type, TInt.Zero);
      CG.Store_indirect (info.stk_type, 0, info.size);
    END;
  END GenInit;

PROCEDURE GenMap (p: P;  offset, size: INTEGER;  refs_only: BOOLEAN) =
  VAR bit_offset := offset MOD Target.Byte;  op: TipeMap.Op;
  BEGIN
    IF refs_only THEN RETURN END;
    IF TInt.LT (p.min, TInt.Zero)
      OR TInt.LT (p.max, TInt.Zero) THEN
      (* value is signed *)
      IF (bit_offset # 0) THEN             op := TipeMap.Op.Int_Field;
      ELSIF (size = 1 * Target.Byte) THEN  op := TipeMap.Op.Int_1;
      ELSIF (size = 2 * Target.Byte) THEN  op := TipeMap.Op.Int_2;
      ELSIF (size = 4 * Target.Byte) THEN  op := TipeMap.Op.Int_4;
      ELSIF (size = 8 * Target.Byte) THEN  op := TipeMap.Op.Int_8;
      ELSE (* weird size *)                op := TipeMap.Op.Int_Field;
      END;
    ELSE (* value is unsigned *)
      IF (bit_offset # 0) THEN             op := TipeMap.Op.Word_Field;
      ELSIF (size = 1 * Target.Byte) THEN  op := TipeMap.Op.Word_1;
      ELSIF (size = 2 * Target.Byte) THEN  op := TipeMap.Op.Word_2;
      ELSIF (size = 4 * Target.Byte) THEN  op := TipeMap.Op.Word_4;
      ELSIF (size = 8 * Target.Byte) THEN  op := TipeMap.Op.Word_8;
      ELSE (* weird size *)                op := TipeMap.Op.Word_Field;
      END;
    END;
    TipeMap.Add (offset, op, bit_offset + 256 * size);
  END GenMap;

PROCEDURE GenDesc (p: P) =
  BEGIN
    IF Type.IsEqual (p, Card.T, NIL) THEN
      EVAL TipeDesc.AddO (TipeDesc.Op.Cardinal, p);
    ELSIF Type.IsEqual (p, LCard.T, NIL) THEN
      EVAL TipeDesc.AddO (TipeDesc.Op.Longcard, p);
    ELSIF TipeDesc.AddO (TipeDesc.Op.Subrange, p) THEN
      TipeDesc.AddX (p.min);
      TipeDesc.AddX (p.max);
    END;
  END GenDesc;

PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  BEGIN
    IF Type.IsEqual (p, Card.T, NIL) THEN
      x.tag := "$cardinal";
      x.n_nodes := 0;
    ELSIF Type.IsEqual (p, LCard.T, NIL) THEN
      x.tag := "$longcard";
      x.n_nodes := 0;
    ELSE
      M3Buf.PutText (x.buf, "SUBRANGE ");
      M3Buf.PutIntt (x.buf, p.min);
      M3Buf.PutChar (x.buf, ' ');
      M3Buf.PutIntt (x.buf, p.max);
      IF (p.baseType = Int.T) OR (Type.Base (p.baseType) = Int.T) THEN
        x.n_nodes := 0;
      ELSE
        x.n_nodes  := 1;
        x.nodes[0] := p.baseType;
      END;
    END;
  END FPrinter;

BEGIN
END SubrangeType.
