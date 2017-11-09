(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ArrayType.m3                                          *)
(* Last modified on Tue May 23 15:24:26 PDT 1995 by kalsow     *)
(*      modified on Sun Feb 24 04:39:01 1991 by muller         *)

MODULE ArrayType;

IMPORT M3, CG, Type, TypeRep, Error, Token, OpenArrayType;
IMPORT Word, Target, TInt, RecordType, TipeMap, TipeDesc, ErrType;
FROM Scanner IMPORT Match, GetToken, cur;

CONST
  MAXSIZE = LAST (INTEGER);

TYPE
  P = Type.T BRANDED "ArrayType.P" OBJECT
        index      : Type.T;
        element    : Type.T;
        alignment  : INTEGER;
        n_elts     : INTEGER;
        elt_align  : INTEGER;
        elt_pack   : INTEGER;
        total_size : INTEGER;
        openCousin : Type.T;  (* == ARRAY OF element *)
        packed     : BOOLEAN;
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
  VAR p, p0: P;
  BEGIN
    Match (TK.tARRAY);
    IF (cur.token IN Token.TypeStart) THEN
      p0 := New (NIL, NIL);  p := p0;
      LOOP
        p.index := Type.Parse ();
        IF (cur.token # TK.tCOMMA) THEN EXIT END;
        GetToken (); (* , *)
        p.element := New (NIL, NIL);
        p := p.element;
      END;
      Match (TK.tOF);
      p.element := Type.Parse ();
      RETURN p0;
    ELSE
      (* must be an open array *)
      Match (TK.tOF);
      RETURN OpenArrayType.New (Type.Parse ());
    END;
  END Parse;

PROCEDURE New (index, element: Type.T): Type.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    TypeRep.Init (p, Type.Class.Array);
    p.index      := index;
    p.element    := element;
    p.alignment  := 0;
    p.n_elts     := 0;
    p.total_size := 0;
    p.elt_align  := 0;
    p.elt_pack   := 0;
    p.openCousin := NIL;
    p.packed     := FALSE;
    RETURN p;
  END New;

PROCEDURE Split (t: Type.T;  VAR index, element: Type.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL) THEN
      index := p.index;  element := p.element;
      RETURN TRUE;
    ELSIF OpenArrayType.Split (t, element) THEN
      index := NIL;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END Split;

PROCEDURE EltPack (t: Type.T): INTEGER =
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL) THEN
      RETURN p.elt_pack;
    ELSIF OpenArrayType.Is (t) THEN
      RETURN OpenArrayType.EltPack (t);
    ELSE
      RETURN 0;
    END;
  END EltPack;

PROCEDURE EltAlign (t: Type.T): INTEGER =
  VAR p:= Reduce (t);
  BEGIN
    IF (p # NIL) THEN
      RETURN p.elt_align;
    ELSIF OpenArrayType.Is (t) THEN
      RETURN OpenArrayType.EltAlign (t);
    ELSE
      RETURN Target.Byte;
    END;
  END EltAlign;

PROCEDURE OpenCousin (t: Type.T): Type.T =
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL) THEN
      IF (p.openCousin = NIL) THEN
        p.openCousin := OpenArrayType.New (p.element);
      END;
      RETURN p.openCousin;
    ELSE
      RETURN t;
    END;
  END OpenCousin;

PROCEDURE IsBitAddressed (t: Type.T): BOOLEAN =
  VAR p:= Reduce (t);
  BEGIN
    RETURN (p # NIL) AND (p.packed);
  END IsBitAddressed;

PROCEDURE GenIndex (t: Type.T) =
  VAR p := Reduce (t);  index: CG.Val;
  BEGIN
    IF (p = NIL) THEN
      CG.Index_bytes (OpenArrayType.EltPack (t));
    ELSIF NOT p.packed THEN
      CG.Index_bytes (p.elt_pack);
    ELSE
      (* we have a packed array with non-byte-aligned elements... *)
      IF (p.elt_pack # 1) THEN
        (* compute the bit-offset of the indexed element *)
        CG.Load_intt (p.elt_pack);
        CG.Multiply (Target.Integer.cg_type);
      END;
      IF (p.total_size <= p.alignment) THEN
        CG.Index_bits ();
      ELSE
        index := CG.Pop ();
        CG.Push (index);
        CG.Load_intt (p.alignment);
        CG.Div (Target.Integer.cg_type, CG.Sign.Positive, CG.Sign.Positive);
        CG.Index_bytes (p.alignment);
        CG.Push (index);
        CG.Load_intt (p.alignment);
        CG.Mod (Target.Integer.cg_type, CG.Sign.Positive, CG.Sign.Positive);
        CG.Index_bits ();
        CG.Free (index);
      END;
    END;
  END GenIndex;

PROCEDURE Check (p: P) =
  VAR align, full_size: INTEGER;  elt_info: Type.Info;
  BEGIN
    p.index := Type.Check (p.index);
    IF NOT Type.IsOrdinal (p.index) THEN
      Error.Msg ("array index type must be an ordinal type");
      p.index := ErrType.T;
    END;

    p.element := Type.CheckInfo (p.element, elt_info);
    IF (elt_info.class = Type.Class.OpenArray) THEN
      Error.Msg ("array element type cannot be an open array");
    END;

    IF NOT TInt.ToInt (Type.Number (p.index), p.n_elts) THEN
      Error.Msg ("CM3 restriction: array has too many elements");
      p.n_elts := 1;
    END;

    align       := elt_info.alignment;
    p.elt_align := elt_info.alignment;
    p.elt_pack  := elt_info.size;
    IF (elt_info.class # Type.Class.Packed) THEN
      (* naturally aligned elements must be OK *)
      p.elt_pack  := (elt_info.size + align - 1) DIV align * align;
      p.alignment := elt_info.alignment;
      p.packed    := FALSE;
    ELSE
      (* find a packing that's allowed *)
      p.alignment := FindAlignment (p);
      p.packed := (p.elt_pack < Target.Byte)
               OR (p.elt_pack MOD p.alignment # 0);
    END;

    IF (p.n_elts > 0) AND (p.elt_pack > 0)
      AND (p.n_elts > MAXSIZE DIV p.elt_pack) THEN
      Error.Msg ("CM3 restriction: array type too large");
      full_size := 0;
      p.total_size := 0;
    ELSE
      full_size := p.elt_pack * p.n_elts;
      p.total_size := RecordType.RoundUp (full_size, p.alignment);
    END;

    p.info.size      := p.total_size;
    p.info.min_size  := p.total_size;
    p.info.alignment := p.alignment;
    p.info.mem_type  := CG.Type.Struct;
    p.info.stk_type  := CG.Type.Addr;
    p.info.class     := Type.Class.Array;
    p.info.isTraced  := elt_info.isTraced;
    p.info.isEmpty   := elt_info.isEmpty;
    p.info.isSolid   := elt_info.isSolid AND (p.elt_pack <= elt_info.size)
                            AND (p.total_size <= full_size);
    p.info.hash      := Word.Plus (Word.Times (23, p.n_elts),
                              Word.Times (29, p.elt_pack));
  END Check;

PROCEDURE FindAlignment (p: P): INTEGER =
  VAR x: INTEGER;
  BEGIN
    FOR a := FIRST (Target.Alignments) TO LAST (Target.Alignments) DO
      x := Target.Alignments[a];
      IF (x >= p.elt_align) AND Type.IsAlignedOk (p, x) THEN
        RETURN x;
      END;
    END;
    Error.Msg ("CM3 restriction: scalars in packed array elements cannot cross word boundaries");
    RETURN Target.Byte;
  END FindAlignment;

PROCEDURE CheckAlign (p: P;  offset: INTEGER): BOOLEAN =
  VAR x0 := offset MOD Target.Integer.size;  x := x0;
  BEGIN
    FOR i := 0 TO p.n_elts-1 DO
      IF NOT Type.IsAlignedOk (p.element, x) THEN RETURN FALSE END;
      x := (x + p.elt_pack) MOD Target.Integer.size;
      IF (x = x0) THEN EXIT END;
    END;
    RETURN TRUE;
  END CheckAlign;

PROCEDURE Compiler (p: P) =
  VAR self, index, elt: INTEGER;
  BEGIN
    Type.Compile (p.index);
    Type.Compile (p.element);
    self  := Type.GlobalUID (p);
    index := Type.GlobalUID (p.index);
    elt   := Type.GlobalUID (p.element);
    CG.Declare_array (self, index, elt, p.total_size);
  END Compiler;

PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN Type.IsEqual (a.element, b.element, x)
       AND Type.IsEqual (a.index, b.index, x);
  END EqualChk;

PROCEDURE Subtyper (a: P;  tb: Type.T): BOOLEAN =
  VAR ta, eb: Type.T;  b: P;
  BEGIN
    ta := a;

    (* peel off the fixed dimensions of A and open dimensions of B *)
    LOOP
      a := Reduce (ta);
      IF (a = NIL) OR NOT OpenArrayType.Split (tb, eb) THEN EXIT END;
      ta := a.element;
      tb := eb;
    END;

    (* peel off the fixed dimensions as long as the sizes are equal *)
    LOOP
      a := Reduce (ta);  b := Reduce (tb);
      IF (a = NIL) OR (b = NIL) THEN EXIT END;
      IF (a.index # b.index) THEN
        IF NOT TInt.EQ (Type.Number (a.index), Type.Number (b.index)) THEN
          RETURN FALSE;
        END;
      END;
      ta := a.element;
      tb := b.element;
    END;

    RETURN Type.IsEqual (ta, tb, NIL);
  END Subtyper;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.info.class = Type.Class.Named) THEN t := Type.Strip (t) END;
    IF (t.info.class # Type.Class.Array) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

PROCEDURE InitCoster (p: P; zeroed: BOOLEAN): INTEGER =
  VAR n, m, res: Target.Int;  x: INTEGER;
  BEGIN
    x := Type.InitCost (p.element, zeroed);
    IF NOT TInt.FromInt (x, m) THEN
      RETURN LAST (INTEGER);
    END;
    n := Type.Number (p.index);
    IF TInt.LT (n, TInt.Zero) THEN (*open array?*) RETURN 20 * x END;
    IF NOT TInt.Multiply (m, n, res) THEN RETURN LAST (INTEGER) END;
    IF NOT TInt.ToInt (res, x) THEN RETURN LAST (INTEGER) END;
    RETURN x;
  END InitCoster;

PROCEDURE GenInit (p: P;  zeroed: BOOLEAN) =
  VAR
    top   : CG.Label;
    cnt   : CG.Val;
    array := CG.Pop ();  (* capture the array's l-value *)
  BEGIN
    (* put down a loop to initialize the additional elements *)
    CG.Load_integer (Target.Integer.cg_type, TInt.Zero);
    cnt := CG.Pop_temp ();
    top := CG.Next_label ();
    CG.Set_label (top);

    (* init ARRAY[cnt] *)
    CG.Push (array);
    CG.Push (cnt);
    GenIndex (p);
    Type.InitValue (p.element, zeroed);

    (* cnt := cnt + 1 *)
    CG.Push (cnt);
    CG.Load_integer (Target.Integer.cg_type, TInt.One);
    CG.Add (Target.Integer.cg_type);
    CG.Store_temp (cnt);

    (* IF (cnt < NUMBER(ARRAY) GOTO TOP-OF-LOOP *)
    CG.Push (cnt);
    CG.Load_intt (p.n_elts);
    CG.If_compare (Target.Integer.cg_type, CG.Cmp.LT, top, CG.Likely);

    (* release the temps *)
    CG.Free (cnt);
    CG.Free (array);
  END GenInit;

PROCEDURE GenMap (p: P;  offset, size: INTEGER;  refs_only: BOOLEAN) =
  BEGIN
    EVAL size;
    IF (p.n_elts <= 0) THEN RETURN END;
    TipeMap.Add (offset, TipeMap.Op.Mark, 0);
    Type.GenMap (p.element, offset, p.elt_pack, refs_only);
    TipeMap.Add (offset + p.elt_pack, TipeMap.Op.Array_1, p.n_elts);
    TipeMap.SetCursor (offset + p.total_size);
  END GenMap;

PROCEDURE GenDesc (p: P) =
  BEGIN
    IF TipeDesc.AddO (TipeDesc.Op.Array, p) THEN
      TipeDesc.AddX (Type.Number (p.index));
      Type.GenDesc (p.element);
    END;
  END GenDesc;

PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  BEGIN
    x.tag      := "ARRAY";
    x.n_nodes  := 2;
    x.nodes[0] := p.index;
    x.nodes[1] := p.element;
  END FPrinter;

BEGIN
END ArrayType.
