(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: OpenArrayType.m3                                      *)
(* Last modified on Tue May 23 15:24:22 PDT 1995 by kalsow     *)
(*      modified on Sun Feb 24 04:39:01 1991 by muller         *)

MODULE OpenArrayType;

IMPORT M3, CG, Type, TypeRep, Error, Target, TInt, Word;
IMPORT ArrayType, PackedType, TipeMap, TipeDesc;

TYPE
  P = Type.T BRANDED "OpenArrayType.P" OBJECT
        element    : Type.T;
        baseElt    : Type.T;
        depth      : INTEGER;
        elt_align  : INTEGER;
        elt_pack   : INTEGER;
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

PROCEDURE New (element: Type.T): Type.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    TypeRep.Init (p, Type.Class.OpenArray);
    p.element    := element;
    p.baseElt    := NIL;
    p.depth      := -1;
    p.elt_pack   := 0;
    RETURN p;
  END New;

PROCEDURE Is (t: Type.T): BOOLEAN =
  BEGIN
    RETURN (Reduce (t) # NIL);
  END Is;

PROCEDURE Split (t: Type.T;  VAR element: Type.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    element := p.element;
    RETURN TRUE;
  END Split;

PROCEDURE EltPack (t: Type.T): INTEGER =
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL)
      THEN RETURN p.elt_pack;
      ELSE RETURN 0;
    END;
  END EltPack;

PROCEDURE EltAlign (t: Type.T): INTEGER =
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL)
      THEN RETURN p.elt_align;
      ELSE RETURN Target.Byte;
    END;
  END EltAlign;

PROCEDURE OpenDepth (t: Type.T): INTEGER =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN 0 END;
    IF (p.depth <= 0) THEN  p.depth := 1 + OpenDepth (p.element)  END;
    RETURN p.depth;
  END OpenDepth;

PROCEDURE OpenType (t: Type.T): Type.T =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN t END;
    IF (p.baseElt = NIL) THEN  p.baseElt := OpenType (p.element)  END;
    RETURN p.baseElt;
  END OpenType;

PROCEDURE Check (p: P) =
  VAR
    elt, elt_base : Type.T;
    align         : INTEGER;
    elt_info      : Type.Info;
    MinAlign := MAX (MAX (Target.Byte, Target.Structure_size_boundary),
                     MAX (Target.Address.align, Target.Integer.align));
  BEGIN
    p.element := Type.Check (p.element);
    elt := Type.CheckInfo (OpenType (p), elt_info);
    align := elt_info.alignment;
    p.elt_align := align;

    IF (elt_info.class = Type.Class.Packed) THEN
      PackedType.Split (elt, p.elt_pack, elt_base);
    ELSE (* naturally aligned elements must be OK *)
      p.elt_pack := (elt_info.size + align - 1) DIV align * align;
    END;

    align := MAX (align, MinAlign); (* == whole array alignment *)
    IF (p.elt_pack MOD Target.Byte) # 0 THEN
      Error.Msg ("CM3 restriction: open array elements must be byte-aligned");
    ELSIF NOT Type.IsAlignedOk (p, align) THEN
      Error.Msg ("CM3 restriction: scalars in packed array elements cannot cross word boundaries");
    END;

    p.info.size      := -1;
    p.info.min_size  := -1;
    p.info.alignment := align;
    p.info.mem_type  := CG.Type.Addr;
    p.info.stk_type  := CG.Type.Addr;
    p.info.class     := Type.Class.OpenArray;
    p.info.isTraced  := elt_info.isTraced;
    p.info.isEmpty   := elt_info.isEmpty;
    p.info.isSolid   := elt_info.isSolid AND (p.elt_pack <= elt_info.size);
    p.info.hash      := Word.Plus (Word.Times (23, OpenDepth (p)),
                              Word.Times (37, p.elt_pack));
  END Check;

PROCEDURE CheckAlign (p: P;  offset: INTEGER): BOOLEAN =
  VAR
    x0 := offset MOD Target.Integer.size;  x := x0;
    t  := OpenType (p);
  BEGIN
    REPEAT
      IF NOT Type.IsAlignedOk (t, x) THEN RETURN FALSE END;
      x := (x + p.elt_pack) MOD Target.Integer.size;
    UNTIL (x = x0);
    RETURN TRUE;
  END CheckAlign;

PROCEDURE DeclareTemp (t: Type.T): CG.Var =
(* If 't' is an open array, declare and return a temporary to hold its
   dope vector, otherwise abort. *)
  VAR
    p    := Reduce (t);
    size := Target.Address.pack + OpenDepth (p) * Target.Integer.pack;
  BEGIN
    RETURN CG.Declare_temp (size, Target.Address.align,
                            CG.Type.Struct, in_memory := TRUE);
  END DeclareTemp;

PROCEDURE Compiler (p: P) =
  VAR size := Target.Address.pack + OpenDepth (p) * Target.Integer.pack;
  BEGIN
    Type.Compile (p.element);
    CG.Declare_open_array (Type.GlobalUID(p), Type.GlobalUID(p.element), size);
  END Compiler;

PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN (OpenDepth (a) = OpenDepth (b))
       AND Type.IsEqual (a.element, b.element, x);
  END EqualChk;

PROCEDURE Subtyper (a: P;  tb: Type.T): BOOLEAN =
  VAR ta, ia, ea, ib, eb: Type.T;  b: P;
  BEGIN
    ta := a;

    (* peel off the common open dimensions *)
    LOOP
      a := Reduce (ta);
      b := Reduce (tb);
      IF (a = NIL) OR (b = NIL) THEN EXIT END;
      ta := a.element;
      tb := b.element;
    END;

    (* peel off the remaining fixed dimensions of A and open dimensions of B *)
    LOOP
      b := Reduce (tb);
      IF (b = NIL) OR NOT ArrayType.Split (ta, ia, ea) THEN EXIT END;
      ta := ea;
      tb := b.element;
    END;

    (* peel off the fixed dimensions as long as the sizes are equal *)
    WHILE ArrayType.Split (ta, ia, ea) AND ArrayType.Split (tb, ib, eb) DO
      IF NOT TInt.EQ (Type.Number (ia), Type.Number (ib)) THEN
        RETURN FALSE;
      END;
      ta := ea;
      tb := eb;
    END;

    RETURN Type.IsEqual (ta, tb, NIL);
  END Subtyper;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.info.class = Type.Class.Named) THEN t := Type.Strip (t) END;
    IF (t.info.class # Type.Class.OpenArray) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

PROCEDURE InitCoster (p: P; zeroed: BOOLEAN): INTEGER =
  VAR n, m, res: Target.Int;  x: INTEGER;
  BEGIN
    IF    TInt.FromInt (Type.InitCost (p.element, zeroed), m)
      AND TInt.FromInt (20, n) (* guess that there are 20 elements *)
      AND TInt.Multiply (m, n, res)
      AND TInt.ToInt (res, x)
      THEN RETURN x;
      ELSE RETURN LAST (INTEGER);
    END;
  END InitCoster;

PROCEDURE GenInit (p: P;  zeroed: BOOLEAN) =
  VAR
    depth := OpenDepth (p);
    elt   := OpenType (p);
    top   : CG.Label;
    cnt   : CG.Val;
    max   : CG.Val;
    array := CG.Pop (); (* capture the array's l-value *)
  BEGIN
    (* compute the number of elements *)
    FOR i := 0 TO depth-1 DO
      CG.Push (array);
      CG.Open_size (i);
      IF (i # 0) THEN CG.Multiply (Target.Word.cg_type) END;
    END;
    max := CG.Pop ();

    (* capture the pointer to the array elements *)
    CG.Push (array);
    CG.Open_elt_ptr (ArrayType.EltAlign (p));
    CG.Free (array);
    array := CG.Pop ();

    (* put down a loop to map the elements *)
    CG.Load_integer (Target.Integer.cg_type, TInt.Zero);
    cnt := CG.Pop_temp ();
    top := CG.Next_label (2);
    CG.Jump (top+1);
    CG.Set_label (top);

    (* map ARRAY[cnt] *)
    CG.Push (array);
    CG.Push (cnt);
    CG.Index_bytes (p.elt_pack);
    Type.InitValue (elt, zeroed);

    (* cnt := cnt + 1 *)
    CG.Push (cnt);
    CG.Load_integer (Target.Integer.cg_type, TInt.One);
    CG.Add (Target.Integer.cg_type);
    CG.Store_temp (cnt);

    (* IF (cnt < NUMBER(ARRAY) GOTO TOP-OF-LOOP *)
    CG.Set_label (top+1);
    CG.Push (cnt);
    CG.Push (max);
    CG.If_compare (Target.Integer.cg_type, CG.Cmp.LT, top, CG.Likely);

    (* release the temps *)
    CG.Free (cnt);
    CG.Free (max);
    CG.Free (array);
  END GenInit;

PROCEDURE GenMap (p: P;  offset: INTEGER;  <*UNUSED*> size: INTEGER;
                  refs_only: BOOLEAN) =
  VAR a: INTEGER;
  BEGIN
    TipeMap.Add (offset, TipeMap.Op.OpenArray_1, OpenDepth (p));
    a := TipeMap.GetCursor ();
    Type.GenMap (OpenType (p), a, p.elt_pack, refs_only);
    TipeMap.Add (a + p.elt_pack, TipeMap.Op.Stop, 0);
  END GenMap;

PROCEDURE GenDesc (p: P) =
  BEGIN
    IF TipeDesc.AddO (TipeDesc.Op.OpenArray, p) THEN
      TipeDesc.AddI (OpenDepth (p));
      Type.GenDesc (OpenType (p));
    END;
  END GenDesc;

PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  BEGIN
    x.tag      := "OPENARRAY";
    x.n_nodes  := 1;
    x.nodes[0] := p.element;
  END FPrinter;

BEGIN
END OpenArrayType.
