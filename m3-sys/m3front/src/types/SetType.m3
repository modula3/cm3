(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: SetType.m3                                            *)
(* Last modified on Tue May 23 15:28:01 PDT 1995 by kalsow     *)
(*      modified on Wed Sep 26 19:01:24 1990 by muller         *)

MODULE SetType;

IMPORT Fmt;

IMPORT M3, CG, Type, TypeRep, Target, TInt, TargetMap, Error, Token, Scanner;
IMPORT Word, TipeMap, TipeDesc, ErrType, PackedType;

TYPE
  P = Type.T OBJECT
        domType      : Type.T;
        broken       : BOOLEAN;
      OVERRIDES
        check       := Check;
        no_straddle := NoStraddle;
        isEqual     := EqualChk;
        isSubtype   := Subtyper;
        compile     := Compiler;
        initCost    := InitCoster;
        initValue   := TypeRep.InitToZeros;
        mapper      := GenMap;
        gen_desc    := GenDesc;
        fprint      := FPrinter;
      END;

(* EXPORTED: *)
PROCEDURE Parse (): Type.T =
  TYPE TK = Token.T;
  VAR p := NEW (P);
  BEGIN
    TypeRep.Init (p, Type.Class.Set);
    Scanner.Match (TK.tSET);
    Scanner.Match (TK.tOF);
    p.domType := Type.Parse ();
    RETURN p;
  END Parse;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.info.class = Type.Class.Named) THEN t := Type.Strip (t) END;
    IF (t.info.class # Type.Class.Set) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

(* EXPORTED: *)
PROCEDURE Split (t: Type.T;  VAR domType: Type.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    domType := p.domType;
    RETURN TRUE;
  END Split;

PROCEDURE Check (p: P) =
  VAR domInfo: Type.Info;
  VAR domNumT, domMinT, domMaxT: Target.Int;
  VAR domNumW, domMinI, domMaxI, wordCtW: INTEGER;
  VAR RangeTxt: TEXT;
  VAR i: INTEGER;
  VAR b: BOOLEAN;
  BEGIN
    p.broken := FALSE;
    p.domType := Type.CheckInfo (p.domType, domInfo);
    IF NOT Type.IsOrdinal (p.domType) THEN
      Error.Msg ("Domain of a SET type must be an ordinal type (2.2.6).");
      p.domType := ErrType.T;
      domNumT := TInt.One;
      domMinT := TInt.Zero;
      domMaxT := TInt.Zero;
      p.broken := TRUE;
    ELSE

      (* Checks against target range limits: *)
      domNumT := Type.Number (p.domType);
      IF TInt.GT (domNumT, Target.Word.max) THEN
        Error.Txt
          (TInt.ToText (domNumT),
           "Domain cardinality of SET type exceeds LAST(INTEGER) (2.2.6).");
        p.broken := TRUE;
      END;
      b := Type.GetBounds (p.domType, domMinT, domMaxT);
      <* ASSERT b *>
      IF TInt.GT (domMaxT, Target.Integer.max) THEN
        Error.Txt
          (TInt.ToText (domMaxT),
           "Upper bound of SET domain type exceeds LAST(INTEGER) (2.2.6).");
        p.broken := TRUE;
      END;
      IF TInt.LT (domMinT, Target.Integer.min) THEN
        Error.Txt
          (TInt.ToText (domMinT),
           "Lower bound of SET domain type less that FIRST(INTEGER) (2.2.6).");
        p.broken := TRUE;
      END;

      (* Checks against CM3 limits, specifically, all within host Word or Int. *)
(* TODO: Code below will handle a target Word, if we only had a TWord.ToWord. *)
      IF NOT TInt.ToInt (domNumT, domNumW)
         OR TInt.LT ( domNumT, TInt.Zero)
      THEN
        RangeTxt := "[0.." & Fmt.Int (LAST(INTEGER)) &"]";
        Error.Txt
          (TInt.ToText (domNumT),
           "Domain cardinality of SET type lies outside CM3 limits of "
            & RangeTxt &  "(2.2.6).");
        p.broken := TRUE;
      END;
      IF NOT TInt.ToInt (domMaxT, domMaxI) THEN
        RangeTxt
          := "[" & Fmt.Int (FIRST(INTEGER)) & ".." & Fmt.Int (LAST(INTEGER)) &"]";
        Error.Txt
          (TInt.ToText (domMaxT),
           "Upper bound of SET domain type lies outside of CM3 limit of "
            & RangeTxt &  "(2.2.6).");
        p.broken := TRUE;
      END;
      IF NOT TInt.ToInt (domMinT, domMinI) THEN
        RangeTxt
          := "[" & Fmt.Int (FIRST(INTEGER)) & ".." & Fmt.Int (LAST(INTEGER)) &"]";
        Error.Txt
          (TInt.ToText (domMinT),
           "Lower bound of SET domain type lies outside of CM3 limit of "
            & RangeTxt &  "(2.2.6).");
        p.broken := TRUE;
      END;

      (* Unconditional properties: *)
      p.info.class     := Type.Class.Set;
      p.info.addr_align:= 8 (* Irrelevant or will change. *);
      p.info.hash      := Word.Times (811, domInfo.hash);
      p.info.isTraced  := FALSE;
      p.info.isEmpty   := FALSE;
      p.info.isSolid   := TRUE;

      (* Representation-dependent properties. *)
      IF p.broken THEN
        p.info.size      := Target.Word.size;
        p.info.min_size  := Target.Word.size;
        p.info.alignment := Target.Word.align;
        p.info.stk_type  := Target.Word.cg_type;
        p.info.mem_type  := Target.Word.cg_type;
      ELSE
        i := FIRST (TargetMap.Word_types);
        LOOP
          WITH wordType = TargetMap.Word_types[i] DO
            IF domNumW <= wordType.size THEN (* It fits. *)
              p.info.size      := wordType.size;
              p.info.min_size  := domNumW;
              p.info.alignment := wordType.align;
              p.info.stk_type  := Target.Word.cg_type;
              p.info.mem_type  := wordType.cg_type;
              EXIT;
            ELSIF i >= LAST (TargetMap.Word_types)
                  OR wordType.cg_type = Target.Word.cg_type
            THEN (* It's bigger than one word. *)
              <*ASSERT domNumW >= Target.Word.size*> (* Preclude overflow below. *)
              wordCtW
                := Word.Plus (Word.Divide(Word.Minus (domNumW,1),Target.Word.size),1);
              p.info.size := Word.Times (wordCtW, Target.Word.size);
              p.info.min_size  := p.info.size;
              p.info.alignment := Target.Word.align;
              p.info.stk_type  := CG.Type.Addr;
              p.info.addr_align := Target.Address.align;
              p.info.mem_type  := CG.Type.Struct;
              EXIT;
            ELSE
              INC (i)
            END;
          END (*WITH*);
        END (*LOOP*);
      END (*IF*)
    END (*IF*)
  END Check;

(* TODO: It should be possible to unify NoStraddle in SetType, EnumType, and
         SubrangeType.  But what about Type.ScalarNoStraddle? *)
  PROCEDURE NoStraddle
    (p: P;  offset: INTEGER; <*UNUSED*> IsEltOrField: BOOLEAN): BOOLEAN =
  VAR
    z0: INTEGER;
  BEGIN
    IF p.info.size > Target.Word.size THEN RETURN TRUE END;
    IF p.info.lazyAligned THEN
      z0 := offset DIV Target.Byte * Target.Byte;
    ELSE
      z0 := offset DIV Target.Integer.align * Target.Integer.align;
    END;
    RETURN (offset + p.info.size) <= (z0 + Target.Integer.size);
  END NoStraddle;

(* EXPORTED: *)
PROCEDURE IsSmallSet (t: Type.T): BOOLEAN =
(* Fits within a word.  Handled as a scalar. *)
(* This includes a packed small set type, regardless of its use. *)
  VAR s: Type.T;
  BEGIN
    IF t = NIL THEN RETURN FALSE END;
    IF t.info.class = Type.Class.Named THEN t := Type.Strip (t) END;
    IF t.info.class = Type.Class.Set THEN
      RETURN t.info.size <= Target.Word.size
    END;
    IF t.info.class = Type.Class.Packed THEN
      s := PackedType.Base (t);
      IF s = NIL THEN RETURN FALSE END;
      IF s.info.class = Type.Class.Set THEN
        RETURN t.info.size <= Target.Word.size
      END
    END;
    RETURN FALSE
  END IsSmallSet;

PROCEDURE Compiler (p: P) =
  VAR info: Type.Info;
  BEGIN
    Type.Compile (p.domType);
    EVAL Type.CheckInfo (p, info);
    CG.Declare_set (Type.GlobalUID (p), Type.GlobalUID (p.domType), info.size);
  END Compiler;

PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN Type.IsEqual (a.domType, b.domType, x);
  END EqualChk;

PROCEDURE Subtyper (a: P;  t: Type.T): BOOLEAN =
  BEGIN
    RETURN Type.IsEqual (a, t, NIL);
  END Subtyper;

PROCEDURE InitCoster (<*UNUSED*> p: P; <*UNUSED*> zeroed: BOOLEAN): INTEGER =
  BEGIN
    RETURN 0;
  END InitCoster;

PROCEDURE GenMap (<*UNUSED*> p: P; offset, size: INTEGER;
                  <*UNUSED*> refs_only: BOOLEAN) =
  BEGIN
    TipeMap.Add (offset, TipeMap.Op.Set_1, size DIV Target.Byte);
  END GenMap;

PROCEDURE GenDesc (p: P) =
  BEGIN
    IF TipeDesc.AddO (TipeDesc.Op.Set, p) THEN
      TipeDesc.AddX (Type.Number (p.domType));
    END;
  END GenDesc;

PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  BEGIN
    x.tag      := "SET";
    x.n_nodes  := 1;
    x.nodes[0] := p.domType;
  END FPrinter;
        
BEGIN
END SetType.
