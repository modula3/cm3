(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: OpenArrayType.m3                                      *)
(* Last modified on Tue May 23 15:24:22 PDT 1995 by kalsow     *)
(*      modified on Sun Feb 24 04:39:01 1991 by muller         *)

MODULE OpenArrayType;

IMPORT M3, CG, Type, TypeRep, Error, Target, TInt, Word;
IMPORT ArrayType, PackedType, RecordType, TipeMap, TipeDesc;

TYPE
  P = Type.T BRANDED "OpenArrayType.P" OBJECT
        EltType                 : Type.T;
        NonopenEltType          : Type.T;
        openDepth               : INTEGER;
        NonopenEltAlign         : INTEGER;
        NonopenEltPack          : INTEGER;
        NonopenEltsBitAddressed : BOOLEAN := FALSE;
        (* ^Could be FALSE even if elements are BITS n FOR ... *) 
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
PROCEDURE New (EltType: Type.T): Type.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    TypeRep.Init (p, Type.Class.OpenArray);
    p.EltType := EltType;
    p.NonopenEltType := NIL;
    p.openDepth := -1;
    p.NonopenEltPack := 0;
    p.NonopenEltsBitAddressed := FALSE;
    RETURN p;
  END New;

(* EXPORTED: *)
PROCEDURE Is (t: Type.T): BOOLEAN =
  BEGIN
    RETURN (Reduce (t) # NIL);
  END Is;

(* EXPORTED: *)
PROCEDURE Split (t: Type.T;  VAR EltType: Type.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    EltType := p.EltType;
    RETURN TRUE;
  END Split;

(* EXPORTED: *)
PROCEDURE EltPack (t: Type.T): INTEGER =
(* Of outermost nonopen element. *)
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL)
      THEN RETURN p.NonopenEltPack;
      ELSE RETURN 0;
    END;
  END EltPack;

(* EXPORTED: *)
PROCEDURE EltAlign (t: Type.T): INTEGER =
(* Of first nonopen element. *)
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL)
      THEN RETURN p.NonopenEltAlign;
      ELSE RETURN Target.Byte;
    END;
  END EltAlign;

(* EXPORTED: *)
PROCEDURE EltsAreBitAddressed (t: Type.T): BOOLEAN =
(* Of first nonopen element. *)
  VAR p:= Reduce (t);
  BEGIN
    RETURN (p # NIL) AND (p.NonopenEltsBitAddressed);
  END EltsAreBitAddressed;

(* EXPORTED: *)
PROCEDURE OpenDepth (t: Type.T): INTEGER =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN 0 END;
    IF (p.openDepth <= 0) THEN  p.openDepth := 1 + OpenDepth (p.EltType)  END;
    RETURN p.openDepth;
  END OpenDepth;

(* EXPORTED: *)
PROCEDURE NonopenEltType (t: Type.T): Type.T =
(* If 't' is an n-dimensional open array, returns the type of the base
   elements; otherwise, returns t. That is, strip all the ARRAY OF in 
   front of t *)
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN t END;
    IF (p.NonopenEltType = NIL) THEN
      p.NonopenEltType := NonopenEltType (p.EltType)
    END;
    RETURN p.NonopenEltType;
  END NonopenEltType;

(* Externally dispatched-to: *)
PROCEDURE Check (p: P) =
  VAR
    LNonopenEltType, NonopenEltBase : Type.T;
    NonopenEltInfo : Type.Info;
    dope_align : INTEGER
      := MAX (MAX (Target.Byte, Target.Structure_size_boundary),
              MAX (Target.Address.align, Target.Integer.align));
  BEGIN
    p.EltType := Type.Check (p.EltType);
    LNonopenEltType := Type.CheckInfo (NonopenEltType (p), NonopenEltInfo);
    p.NonopenEltAlign := NonopenEltInfo.alignment;

    IF (NonopenEltInfo.class = Type.Class.Packed) THEN
      PackedType.Split
        (LNonopenEltType, (*OUT*) p.NonopenEltPack, (*OUT*) NonopenEltBase);
      IF Type.IsStructured (LNonopenEltType) THEN
        IF p.NonopenEltPack >= Target.Byte
           AND p.NonopenEltPack MOD p.NonopenEltAlign # 0 THEN
          Error.Msg
            ("CM3 restriction: scalar components of packed, structured array "
              & "elements cannot cross word boundaries");
          p.NonopenEltPack
            := RecordType.RoundUp (NonopenEltInfo.size, p.NonopenEltAlign);
        END
      ELSE (* Scalar packed elements. *)
        IF Target.Word64.size MOD p.NonopenEltPack # 0 THEN
           (* ^Allow 64-bit element pack, even on 32-bit target. *) 
          Error.Msg
            ("CM3 restriction: open array scalar element size must evenly "
             & "divide 64 bits.");
          p.NonopenEltPack := p.NonopenEltAlign;
        END;
      END;
      p.NonopenEltsBitAddressed := p.NonopenEltPack < Target.Byte;
    ELSE (* Not packed; Pack to satisfy their natural alignment. *)
      p.NonopenEltPack
        := RecordType.RoundUp (NonopenEltInfo.size, p.NonopenEltAlign);
      p.NonopenEltsBitAddressed := FALSE;
    END;

    dope_align := MAX (p.NonopenEltAlign, dope_align); 

    p.info.size      := -1;
    p.info.min_size  := -1;
    p.info.alignment := dope_align;
    p.info.addr_align:= p.NonopenEltAlign;
    p.info.mem_type  := CG.Type.Addr;
    p.info.stk_type  := CG.Type.Addr;
    p.info.class     := Type.Class.OpenArray;
    p.info.isTraced  := NonopenEltInfo.isTraced;
    p.info.isEmpty   := NonopenEltInfo.isEmpty;
    p.info.isSolid   := NonopenEltInfo.isSolid
                        AND (p.NonopenEltPack <= NonopenEltInfo.size);
    p.info.hash      := Word.Plus
                          ( NonopenEltInfo.hash,
                            Word.Plus
                              ( Word.Times (23, OpenDepth (p)),
                                Word.Times (37, p.NonopenEltPack)
                              )
                          );
  END Check;

(* Externally dispatched-to: *)
PROCEDURE IsStraddleFree
  (p: P;  offset: INTEGER; <*UNUSED*> IsEltOrField: BOOLEAN): BOOLEAN =
  VAR
    x0 := offset MOD Target.Word.size;  x := x0;
    t  := NonopenEltType (p);
  BEGIN
    REPEAT
      IF NOT Type.StraddleFreeScalars (t, offs := x, IsEltOrField := TRUE)
      THEN RETURN FALSE
      END;
      x := (x + p.NonopenEltPack) MOD Target.Word.size;
    UNTIL (x = x0);
    RETURN TRUE;
  END IsStraddleFree;

(* EXPORTED: *)
PROCEDURE DeclareTemp (t: Type.T): CG.Var =
(* If 't' is an open array type, declare and return a temporary to hold its
   dope vector, otherwise abort. *)
  VAR
    p    := Reduce (t);
    size := Target.Address.pack + OpenDepth (p) * Target.Integer.pack;
  BEGIN
    RETURN CG.Declare_temp (size, Target.Address.align,
                            CG.Type.Struct, in_memory := TRUE);
  END DeclareTemp;

(* Externally dispatched-to: *)
PROCEDURE Compiler (p: P) =
  VAR size := Target.Address.pack + OpenDepth (p) * Target.Integer.pack;
  BEGIN
    Type.Compile (p.EltType);
    CG.Declare_open_array (Type.GlobalUID(p), Type.GlobalUID(p.EltType), size);
  END Compiler;

(* Externally dispatched-to: *)
PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN (OpenDepth (a) = OpenDepth (b))
       AND Type.IsEqual (a.EltType, b.EltType, x);
  END EqualChk;

(* Externally dispatched-to: *)
PROCEDURE Subtyper (a: P;  tb: Type.T): BOOLEAN =
  VAR ta, ia, ea, ib, eb: Type.T;  b: P;
  BEGIN
    ta := a;

    (* peel off the common open dimensions *)
    LOOP
      a := Reduce (ta);
      b := Reduce (tb);
      IF (a = NIL) OR (b = NIL) THEN EXIT END;
      ta := a.EltType;
      tb := b.EltType;
    END;

    (* peel off the remaining fixed dimensions of A and open dimensions of B *)
    LOOP
      b := Reduce (tb);
      IF (b = NIL) OR NOT ArrayType.Split (ta, ia, ea) THEN EXIT END;
      ta := ea;
      tb := b.EltType;
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
    IF (t.info.class = Type.Class.Packed) THEN t := Type.StripPacked (t) END;
    IF (t.info.class # Type.Class.OpenArray) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

(* Externally dispatched-to: *)
PROCEDURE InitCoster (p: P; zeroed: BOOLEAN): INTEGER =
  VAR n, m, res: Target.Int;  x: INTEGER;
  BEGIN
    IF    TInt.FromInt (Type.InitCost (p.EltType, zeroed), m)
      AND TInt.FromInt (20, n) (* guess that there are 20 elements *)
      AND TInt.Multiply (m, n, res)
      AND TInt.ToInt (res, x)
      THEN RETURN x;
      ELSE RETURN LAST (INTEGER);
    END;
  END InitCoster;

(* Externally dispatched-to: *)
PROCEDURE GenInit (p: P;  zeroed: BOOLEAN) =
  VAR
    depth := OpenDepth (p);
    elt   := NonopenEltType (p);
    top   : CG.Label;
    cnt   : CG.Val;
    max   : CG.Val;
    array := CG.Pop (); (* capture the array's l-value *)
  BEGIN
    (* compute the number of non-open elements *)
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
    CG.Index_bytes (p.NonopenEltPack);
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

(* Externally dispatched-to: *)
PROCEDURE GenMap (p: P;  offset: INTEGER;  <*UNUSED*> size: INTEGER;
                  refs_only: BOOLEAN) =
  VAR a: INTEGER;
  BEGIN
    TipeMap.Add (offset, TipeMap.Op.OpenArray_1, OpenDepth (p));
    a := TipeMap.GetCursor ();
    Type.GenMap (NonopenEltType (p), a, p.NonopenEltPack, refs_only);
    TipeMap.Add (a + p.NonopenEltPack, TipeMap.Op.Stop, 0);
  END GenMap;

(* Externally dispatched-to: *)
PROCEDURE GenDesc (p: P) =
  BEGIN
    IF TipeDesc.AddO (TipeDesc.Op.OpenArray, p) THEN
      TipeDesc.AddI (OpenDepth (p));
      Type.GenDesc (NonopenEltType (p));
    END;
  END GenDesc;

(* Externally dispatched-to: *)
PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  BEGIN
    x.tag      := "OPENARRAY";
    x.n_nodes  := 1;
    x.nodes[0] := p.EltType;
  END FPrinter;

BEGIN
END OpenArrayType.
