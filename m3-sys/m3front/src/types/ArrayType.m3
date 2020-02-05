(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ArrayType.m3                                          *)
(* Last modified on Tue May 23 15:24:26 PDT 1995 by kalsow     *)
(*      modified on Sun Feb 24 04:39:01 1991 by muller         *)

MODULE ArrayType;

IMPORT M3, CG, Type, TypeRep, Error, Token;
IMPORT OpenArrayType, RecordType;
IMPORT Word, Target, TInt, TipeMap, TipeDesc, ErrType;
FROM Scanner IMPORT Match, GetToken, cur;

VAR MaxBitSize := LAST (INTEGER);

TYPE
  P = Type.T BRANDED "ArrayType.P" OBJECT
        indexType        : Type.T;
        elementType      : Type.T;
        alignment        : INTEGER;
        n_elts           : INTEGER;
        elt_align        : INTEGER;
        elt_pack         : INTEGER;
        total_size       : INTEGER;
        fixedDepth       : INTEGER; (* # of dimensions, lazily computed. *) 
        openCousin       : Type.T;  (* == ARRAY OF elementType *)
        eltsBitAddressed : BOOLEAN;
        (* ^Could be FALSE even if elements are BITS n FOR, if 
            n is divisible by 8. *) 
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
  VAR p, p0: P;
  BEGIN
    Match (TK.tARRAY);
    IF (cur.token IN Token.TypeStart) THEN
      p0 := New (NIL, NIL);  p := p0;
      LOOP
        p.indexType := Type.Parse ();
        IF (cur.token # TK.tCOMMA) THEN EXIT END;
        GetToken (); (* , *)
        p.elementType := New (NIL, NIL);
        p := p.elementType;
      END;
      Match (TK.tOF);
      p.elementType := Type.Parse ();
      RETURN p0;
    ELSE
      (* must be an open array *)
      Match (TK.tOF);
      RETURN OpenArrayType.New (Type.Parse ());
    END;
  END Parse;

PROCEDURE Reduce (t: Type.T): P =
  (* Strip Named and Packed.  NIL if that's not an array type. *) 
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.info.class = Type.Class.Named) THEN t := Type.Strip (t) END;
    IF (t.info.class = Type.Class.Packed) THEN t := Type.StripPacked (t) END;
    IF (t.info.class # Type.Class.Array) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

(* EXPORTED: *)
PROCEDURE New (index, element: Type.T): Type.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    TypeRep.Init (p, Type.Class.Array);
    p.indexType  := index;
    p.elementType := element;
    p.alignment   := 0;
    p.n_elts      := 0;
    p.total_size  := 0;
    p.elt_align   := 0;
    p.elt_pack    := 0;
    p.fixedDepth  := -1; 
    p.openCousin  := NIL;
    p.eltsBitAddressed := FALSE;
    RETURN p;
  END New;

PROCEDURE FixedDepth (p: P): INTEGER =
  VAR eltType: Type.T;
  BEGIN
    IF p = NIL THEN RETURN 0 END;
    IF p.fixedDepth <= 0 THEN (* Compute it lazily and cache. *) 
      p.fixedDepth := 1;
      eltType := Reduce (p.elementType);
      IF eltType # NIL THEN
        INC (p.fixedDepth, FixedDepth (eltType));
      END;
    END;
    RETURN p.fixedDepth;
  END FixedDepth;

(* EXPORTED: *)
PROCEDURE TotalDepth (t: Type.T): INTEGER =
(* Total number of dimensions of t, open + fixed.
   Works on open array t too. *) 
  VAR p: P; 
  VAR openDepth: INTEGER; 
  BEGIN
    IF t = NIL THEN RETURN 0 END;
    IF OpenArrayType.Is (t) THEN
      openDepth := OpenArrayType.OpenDepth (t);
      p := Reduce (OpenArrayType.NonopenEltType (t));
      RETURN openDepth + FixedDepth (p);
    ELSE
      p := Reduce (t);
      RETURN FixedDepth (p); 
    END;
  END TotalDepth;

(* EXPORTED: *)
PROCEDURE Is (t: Type.T): BOOLEAN =
(* An array type, open or fixed. *)
  VAR p := Reduce (t);
  BEGIN
    IF p # NIL THEN RETURN TRUE
    ELSE RETURN OpenArrayType.Is (p)
    END
  END Is;

(* EXPORTED: *)
PROCEDURE IsFixed (t: Type.T): BOOLEAN =
(* A fixed array type. *)
  VAR p := Reduce (t);
  BEGIN
    RETURN p # NIL;
  END IsFixed;

(* EXPORTED: *)
PROCEDURE Split (t: Type.T;  VAR index, element: Type.T): BOOLEAN =
(* Succeeds on an open array too. *) 
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL) THEN
      index := p.indexType;  element := p.elementType;
      RETURN TRUE;
    ELSIF OpenArrayType.Split (t, element) THEN
      index := NIL;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END Split;

(* EXPORTED: *)
PROCEDURE EltPack (t: Type.T): INTEGER =
(* If 'array' is an array type, returns the packed size in bits of
   its elements.  If 'array' is an open array type, this is for the
   outermost fixed array dimension, if such exists.  If t is not an
   array type, returns 0. *)
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

(* EXPORTED: *)
PROCEDURE EltAlign (t: Type.T): INTEGER =
(* If 'array' is an array type, returns the bit alignment of its
   elements.  If t is open, returns the nearest non-open or non-array
   alignment. If array is not an array type, returns Target.Byte. *)
  VAR p:= Reduce (t);
  BEGIN
    IF (p # NIL) THEN
      RETURN p.elt_align;
    ELSIF OpenArrayType.Is (t) THEN
      RETURN OpenArrayType.EltAlign (t);
    ELSE (* Not an array. *) 
      RETURN Target.Byte;
    END;
  END EltAlign;

(* EXPORTED: *)
PROCEDURE OpenCousin (t: Type.T): Type.T =
(* If 't' is an 'ARRAY I OF X', returns 'ARRAY OF X', otherwise
   returns 't'. *)
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL) THEN
      IF (p.openCousin = NIL) THEN
        p.openCousin := OpenArrayType.New (p.elementType);
      END;
      RETURN p.openCousin;
    ELSE
      RETURN t;
    END;
  END OpenCousin;

(* EXPORTED: *)
PROCEDURE EltsAreBitAddressed (t: Type.T): BOOLEAN =
(* PRE: t is Checked. *)
  VAR p:= Reduce (t);
  BEGIN
    RETURN (p # NIL) AND (p.eltsBitAddressed);
  END EltsAreBitAddressed;

(* EXPORTED: *)
PROCEDURE GenIndex (t: Type.T) =
(* Given "ADR(a)" and "index" on the stack, generate code to replace them
   by "ADR(a[index])" on the stack.  Beginning and ending "addresses" are
   CG 'ValRec's and may include a 'bits' expression. Generates no bounds
   checks. *)
(* Works for an open array too, but uses elt_pack of the first nonopen element
   type, so, for an open array of depth > 1, 'index' must have been already
   multiplied by the product of element counts of inner open dimensions. *)
(* PRE: t is Checked. *)
  VAR array_info : Type.Info;
  VAR p := Reduce (t); (* If non-NIL, p is a fixed array type. *) 
  VAR bit_index: CG.Val;
  VAR array_align : INTEGER (* of the elements, not the dope fields. *);
  VAR elt_pack: INTEGER;
  VAR eltsBitAddressed: BOOLEAN;
  BEGIN
    IF (p = NIL) THEN (* t is an open array. *)
      eltsBitAddressed := OpenArrayType.EltsAreBitAddressed (t);
      t := Type.CheckInfo(Type.StripPacked(t), array_info);
      array_align := array_info.alignment;
      elt_pack := OpenArrayType.EltPack (t);
    ELSE (* Fixed array, p is its narrow type. *) 
      eltsBitAddressed := p.eltsBitAddressed;
      array_align := p.alignment;
      elt_pack := p.elt_pack;
    END;
    IF NOT eltsBitAddressed THEN
      <* ASSERT elt_pack MOD Target.Byte = 0 *> 
      CG.Index_bytes (elt_pack);
    ELSE (* We have an array with non-byte-aligned packed elements. *)
      IF (elt_pack # 1) THEN
        (* compute the bit-offset of the indexed element *)
        CG.Load_intt (elt_pack);
        CG.Multiply (Target.Integer.cg_type);
      END;
      IF TRUE
         OR p # NIL (* fixed array*) AND p.total_size <= p.alignment THEN
        CG.Index_bits ();
      ELSE
        bit_index := CG.Pop ();
        CG.Push (bit_index);
        CG.Load_intt (array_align);
        CG.Div (Target.Integer.cg_type, CG.Sign.Positive, CG.Sign.Positive);
        CG.Index_bytes (array_align);
        CG.Push (bit_index);
        CG.Load_intt (array_align);
        CG.Mod (Target.Integer.cg_type, CG.Sign.Positive, CG.Sign.Positive);
        CG.Index_bits ();
        CG.Free (bit_index);
      END;
    END;
  END GenIndex;

(* Externally dispatched-to: *) 
PROCEDURE Check (p: P) =
  VAR min_size: INTEGER;
  VAR elt_info: Type.Info;
  BEGIN
    p.indexType := Type.Check (p.indexType);
    IF NOT Type.IsOrdinal (p.indexType) THEN
      Error.Msg ("Array index type must be an ordinal type (2.2.3).");
      p.indexType := ErrType.T;
    END;

    p.elementType := Type.CheckInfo (p.elementType, (*OUT*) elt_info);
    IF (elt_info.class = Type.Class.OpenArray) THEN
      Error.Msg ("fixed array element type cannot be open array (2.2.3).");
    END;

    IF NOT TInt.ToInt (Type.Number (p.indexType), (*OUT*) p.n_elts) THEN
      Error.Msg ("CM3 restriction: array type has too many elements");
(* FIXME: Tell the user how many is allowed.*)
(* FIXME: Cross-compile 32-bit host to 64. *)
      p.n_elts := 1;
    END;

    p.elt_align := elt_info.alignment;
    IF (elt_info.class = Type.Class.Packed) THEN
      (* Find an element alignment that won't cause word straddle. *)
      p.elt_pack  := elt_info.size; (* The BITS n size. *)
      p.alignment := ArrayAlignWithPackedElts (p);
      p.eltsBitAddressed
         := (p.elt_pack < Target.Byte) OR (p.elt_pack MOD p.alignment # 0);
    ELSE
      (* Natural element alignment is the alignment of the whole array. *)
      p.elt_pack := RecordType.RoundUp (elt_info.size, elt_info.alignment); 
      p.alignment := elt_info.alignment;
      p.eltsBitAddressed := FALSE;
    END;

    IF (p.n_elts > 0) AND (p.elt_pack > 0)
      AND (p.n_elts > MaxBitSize DIV p.elt_pack) THEN
      Error.Msg ("CM3 restriction: array type too large");
(* FIXME: Tell the user how large it can be.*)
(* FIXME: Cross-compile 32-bit host to 64. *)
      min_size := 0;
      p.total_size := 0;
    ELSE
      min_size := p.elt_pack * p.n_elts;
      p.total_size := RecordType.RoundUp (min_size, p.alignment);
    END;

    p.info.size      := p.total_size;
    p.info.min_size  := min_size;
    p.info.alignment := p.alignment;
    p.info.mem_type  := CG.Type.Struct;
    p.info.stk_type  := CG.Type.Addr;
    p.info.class     := Type.Class.Array;
    p.info.isTraced  := elt_info.isTraced;
    p.info.isEmpty   := elt_info.isEmpty;
    p.info.isSolid   := elt_info.isSolid
                        AND (p.elt_pack <= elt_info.size)
                        AND (p.total_size <= min_size);
    p.info.hash      := Word.Plus (Word.Times (23, p.n_elts),
                              Word.Times (29, p.elt_pack));
  END Check;

PROCEDURE ArrayAlignWithPackedElts (p: P): INTEGER =
(* The smallest alignment of the whole array that will statically ensure
   elements won't straddle word boundaries, ensured either because
   the entire array ends before crossing a word boundary, or because 
   word boundaries occur only at element boundaries. *) 
  VAR trialAlign, offset: INTEGER;
  BEGIN
    FOR a := FIRST (Target.Alignments) (* Smallest *)
             TO LAST (Target.Alignments) DO
      trialAlign := Target.Alignments[a];
      IF trialAlign > Target.Word.align THEN EXIT END; 
      offset := Target.Word.size;
      LOOP (* Backwards through multiples of trialAlign within a word. *)
        DEC (offset, trialAlign);
        IF NOT IsStraddleFree (p, offset, IsEltOrField := TRUE) THEN EXIT
        ELSIF offset <= 0 THEN RETURN trialAlign
        END
      END;
    END;
    Error.Msg
      ("CM3 restriction: Scalars in packed array elements cannot cross "
        & "word boundaries (2.2.5).");
    RETURN Target.Byte;
  END ArrayAlignWithPackedElts;

(* Externally dispatched-to: *) 
PROCEDURE IsStraddleFree
  (p: P;  offset: INTEGER; <*UNUSED*> IsEltOrField: BOOLEAN): BOOLEAN =
  VAR x0 := offset MOD Target.Word.size;  x := x0;
  BEGIN
    FOR i := 0 TO p.n_elts-1 DO
      IF NOT Type.StraddleFreeScalars
               (p.elementType, offs := x, IsEltOrField := TRUE)
      THEN RETURN FALSE
      END;
      x := (x + p.elt_pack) MOD Target.Word.size;
      IF (x = x0) THEN EXIT END;
    END;
    RETURN TRUE;
  END IsStraddleFree;

(* Externally dispatched-to: *) 
PROCEDURE Compiler (p: P) =
  VAR self, index, elt: INTEGER;
  BEGIN
    Type.Compile (p.indexType);
    Type.Compile (p.elementType);
    self  := Type.GlobalUID (p);
    index := Type.GlobalUID (p.indexType);
    elt   := Type.GlobalUID (p.elementType);
    CG.Declare_array (self, index, elt, p.total_size);
  END Compiler;

(* Externally dispatched-to: *) 
PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN Type.IsEqual (a.elementType, b.elementType, x)
       AND Type.IsEqual (a.indexType, b.indexType, x);
  END EqualChk;

(* Externally dispatched-to: *) 
PROCEDURE Subtyper (a: P;  tb: Type.T): BOOLEAN =
  VAR ta, eb: Type.T;  b: P;
  BEGIN
    ta := a;

    (* peel off the fixed dimensions of A and matching open dimensions of B *)
    LOOP
      a := Reduce (ta);
      IF (a = NIL) OR NOT OpenArrayType.Split (tb, eb) THEN EXIT END;
      ta := a.elementType;
      tb := eb;
    END;

    (* peel off the fixed dimensions as long as the sizes are equal *)
    LOOP
      a := Reduce (ta);  b := Reduce (tb);
      IF (a = NIL) OR (b = NIL) THEN EXIT END;
      IF (a.indexType # b.indexType) THEN
        IF NOT TInt.EQ (Type.Number (a.indexType), Type.Number (b.indexType))
        THEN
          RETURN FALSE;
        END;
      END;
      ta := a.elementType;
      tb := b.elementType;
    END;

    RETURN Type.IsEqual (ta, tb, NIL);
  END Subtyper;

(* Externally dispatched-to: *) 
PROCEDURE InitCoster (p: P; zeroed: BOOLEAN): INTEGER =
  VAR n, m, res: Target.Int;  x: INTEGER;
  BEGIN
    x := Type.InitCost (p.elementType, zeroed);
    IF NOT TInt.FromInt (x, m) THEN
      RETURN LAST (INTEGER);
    END;
    n := Type.Number (p.indexType);
    IF TInt.LT (n, TInt.Zero) THEN (*open array?*) RETURN 20 * x END;
    IF NOT TInt.Multiply (m, n, res) THEN RETURN LAST (INTEGER) END;
    IF NOT TInt.ToInt (res, x) THEN RETURN LAST (INTEGER) END;
    RETURN x;
  END InitCoster;

(* Externally dispatched-to: *) 
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
    Type.InitValue (p.elementType, zeroed);

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

(* Externally dispatched-to: *) 
PROCEDURE GenMap (p: P;  offset, size: INTEGER;  refs_only: BOOLEAN) =
  BEGIN
    EVAL size;
    IF (p.n_elts <= 0) THEN RETURN END;
    TipeMap.Add (offset, TipeMap.Op.Mark, 0);
    Type.GenMap (p.elementType, offset, p.elt_pack, refs_only);
    TipeMap.Add (offset + p.elt_pack, TipeMap.Op.Array_1, p.n_elts);
    TipeMap.SetCursor (offset + p.total_size);
  END GenMap;

(* Externally dispatched-to: *) 
PROCEDURE GenDesc (p: P) =
  BEGIN
    IF TipeDesc.AddO (TipeDesc.Op.Array, p) THEN
      TipeDesc.AddX (Type.Number (p.indexType));
      Type.GenDesc (p.elementType);
    END;
  END GenDesc;

(* Externally dispatched-to: *) 
PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  BEGIN
    x.tag      := "ARRAY";
    x.n_nodes  := 2;
    x.nodes[0] := p.indexType;
    x.nodes[1] := p.elementType;
  END FPrinter;

BEGIN
END ArrayType.
