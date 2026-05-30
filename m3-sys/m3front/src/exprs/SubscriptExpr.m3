(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: SubscriptExpr.m3                                      *)
(* Last modified on Thu Nov 10 12:06:15 PST 1994 by kalsow     *)
(*      modified on Thu Mar  7 01:44:07 1991 by muller         *)

MODULE SubscriptExpr;

IMPORT CG, Expr, ExprRep, ArrayType, Error, Type, Int, LInt;
IMPORT ArrayExpr, OpenArrayType, Host, EnumExpr;
IMPORT CheckExpr, SubtractExpr, IntegerExpr, ErrType;
IMPORT RefType, DerefExpr, Target, TInt, M3RT, RunTyme;
IMPORT MSIR, MSIRBuilder, MSIRType, CaptureAnalysis;

TYPE
  P = ExprRep.Tab BRANDED "SubscriptExpr.P" OBJECT
        biased_b     : Expr.T; (* Subscript minus lowerBound. *) 
        lhsOpenDepth : INTEGER; (* Open depth of lhs's type. *)
        tmp          : CG.Val;
        OMDopeVal    : CG.Val (* Dope of outermost array, if it's open. *);
        OMOpenDepth  : INTEGER := 0 (* Open depth of outermost array. *); 
        taBase       : Type.T; 
        leftSs       : P; (* p.a if it's a P; NIL otherwise. *) 
        ssDepth      : INTEGER; (* 1-origin, R to L depth of this subscript in an
                                   unbroken sequence of subscripts. *)
        shapeSs      : INTEGER; (* L to R shape subscript corresponding to
                                   this array subscript, in OMDopeVal. *)
        checkedPass1 : BOOLEAN 
      OVERRIDES
        typeOf       := TypeOf;
        repTypeOf    := RepTypeOf;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := PrepLV;
        compileLV    := CompileLV;
        prepBR       := ExprRep.PrepNoBranch;
        compileBR    := ExprRep.NoBranch;
        evaluate     := Fold;
        isEqual      := ExprRep.EqCheckAB;
        getBounds    := ExprRep.NoBounds;
        isWritable   := IsWritable;
        isDesignator := IsDesignator;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := NoteWrites;
        exprAlign    := SubscriptExprAlign;
        captureLV := CaptureLV;
        compileMSIR       := CompileMSIR;
        compileLValueMSIR := LValueMSIR;
      END;

(* EXPORTED: *) 
PROCEDURE New (a, b: Expr.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.a            := a;
    p.b            := b;
    p.leftSs       := NIL; 
    p.ssDepth      := 0;
    p.shapeSs      := 0;
    p.biased_b     := NIL;
    p.lhsOpenDepth := 0;
    p.tmp          := NIL;
    p.checkedPass1 := FALSE; 
    p.checked      := FALSE; 
    RETURN p;
  END New;

(* Externally dispatched-to: *) 
PROCEDURE TypeOf (p: P): Type.T =
  VAR ta, ti, te: Type.T;
  BEGIN
    ta := Type.Base (Expr.TypeOf (p.a));

    IF RefType.Is (ta) THEN
      (* auto-magic dereference *)
      p.a := DerefExpr.New (p.a);
      p.a.origin := p.origin;
      ta := Type.Base (Expr.TypeOf (p.a));
    END;

    IF ArrayType.Split (ta, ti, te)
      THEN RETURN te;
      ELSE RETURN ta;
    END;
  END TypeOf;

(* Externally dispatched-to: *)
PROCEDURE RepTypeOf (p: P): Type.T =
  VAR ta, ti, te: Type.T;
  BEGIN
    ta := Type.Base (Expr.RepTypeOf (p.a));

    IF RefType.Is (ta) THEN
      (* auto-magic dereference *)
      p.a := DerefExpr.New (p.a);
      p.a.origin := p.origin;
      ta := Type.Base (Expr.RepTypeOf (p.a));
    END;

    IF ArrayType.Split (ta, ti, te)
      THEN RETURN te;
      ELSE RETURN ta;
    END;
  END RepTypeOf;

PROCEDURE GenRangeCheck
  (p: P; mini, maxi, minb, maxb: Target.Int; VAR cs: Expr.CheckState) =
  VAR z: Target.Int;
  VAR b: BOOLEAN; 
  BEGIN
    IF TInt.LT (minb, mini) AND TInt.LT (maxi, maxb) THEN
      b := TInt.Subtract (maxi, mini, z);  <*ASSERT b *>
      p.biased_b := CheckExpr.New (p.biased_b, TInt.Zero, z,
                                   CG.RuntimeError.SubscriptOutOfRange);
      p.biased_b.origin := p.origin;
      Expr.TypeCheck (p.biased_b, cs);
    ELSIF TInt.LT (minb, mini) THEN
      IF TInt.LT (maxb, mini) THEN
        Error.Warn (2, "Will raise runtime error if executed: "
          & "subscript is below array index type.");
      END;
      p.biased_b := CheckExpr.NewLower (p.biased_b, TInt.Zero,
                                   CG.RuntimeError.SubscriptOutOfRange);
      p.biased_b.origin := p.origin;
      Expr.TypeCheck (p.biased_b, cs);
    ELSIF TInt.LT (maxi, maxb) THEN
      IF TInt.LT (maxi, minb) THEN
        Error.Warn (2, "Will raise runtime error if executed: "
          & "subscript is above array index type.");
      END;
      b := TInt.Subtract (maxi, mini, z);  <*ASSERT b *>
      p.biased_b := CheckExpr.NewUpper (p.biased_b, z,
                                   CG.RuntimeError.SubscriptOutOfRange);
      p.biased_b.origin := p.origin;
      Expr.TypeCheck (p.biased_b, cs);
    END;
  END GenRangeCheck;
  
(* Externally dispatched-to: *)
(* Called only with the rightmost (topmost) of an uninterrupted sequence
   of subscripts.  An implicit dereference counts as an interruption. *) 
PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  BEGIN
    DirectCheckPass1 (p, cs); 
    DirectCheckPass2 (p, cs, ssDepth := 1); 
  END Check;

PROCEDURE DirectCheckPass1 (p: P; VAR cs: Expr.CheckState) =
(* For this SubscriptExpr, do just enough to ascertain whether an implied
   dereference of its left operand is needed.  Fully check deeper LHS
   subexpression nodes that are not Ps.
   POST: p.leftSs and p.type are computed.
   POST: p.checkedPass1. *) 
  VAR ta, ti: Type.T;
  BEGIN
    IF p = NIL THEN RETURN END;
    IF p.checkedPass1 THEN RETURN END;
    TYPECASE p.a OF
    | NULL =>
        p.type := ErrType.T;
        p.checkedPass1 := TRUE; 
        RETURN;
    | P (pa) => (* Left operand is another subscript expression node. *)  
      DirectCheckPass1 (pa, cs);
      ta := Type.Check(Type.Base (Expr.TypeOf (p.a)));
      IF RefType.Is (ta) THEN (* Insert an implicit dereference. *)
        (* But first finish subscript node pa, as top-level. *)
        DirectCheckPass2 (pa, cs, ssDepth := 1); 
        p.leftSs := NIL; 
        p.a := DerefExpr.New (pa);
        p.a.origin := p.origin;
        Expr.TypeCheck (p.a, cs);
        ta := Type.Check (Type.Base (Expr.TypeOf (p.a)));
      ELSE
        p.leftSs := pa;
      END;
    ELSE
      Expr.TypeCheck (p.a, cs);
      ta := Type.Check (Type.Base (Expr.TypeOf (p.a)));
      IF RefType.Is (ta) THEN (* Insert an implicit dereference. *)
        p.a := DerefExpr.New (p.a);
        p.a.origin := p.origin;
        Expr.TypeCheck (p.a, cs);
        ta := Type.Check (Type.Base (Expr.TypeOf (p.a)));
      END; 
    END; 
    IF NOT ArrayType.Split (ta, ti, p.type) THEN
      Error.Msg
        ("Subscripted expression must be an array or reference thereto.");
      p.type := ErrType.T;
      ta := ErrType.T;
    END;
    p.taBase := ta;
    p.checkedPass1 := TRUE; 
  END DirectCheckPass1;

PROCEDURE DirectCheckPass2 (p: P; VAR cs: Expr.CheckState; ssDepth: INTEGER) =
(* We need the ssDepth parameter for recursing between immediately
   successive subscript nodes. *) 
  VAR
    ti, te, tb: Type.T;
    mini, maxi, minb, maxb: Target.Int;
    b: BOOLEAN;
  BEGIN
    IF p = NIL THEN RETURN END;
    IF p.checked THEN RETURN END;
    p.ssDepth := ssDepth;
    IF p.leftSs # NIL THEN
      DirectCheckPass2 (p.leftSs, cs, ssDepth+1);
    END;

    IF p.taBase = NIL OR p.taBase = ErrType.T THEN
      p.checked := TRUE;
      RETURN;
    END; 
    
    p.lhsOpenDepth := OpenArrayType.OpenDepth (p.taBase);
    IF p.leftSs = NIL THEN p.shapeSs := 0
    ELSE p.shapeSs := p.leftSs.shapeSs + 1
    END; 
    
    IF p.ssDepth = 1 AND p.lhsOpenDepth > 1
       AND OpenArrayType.EltsAreBitAddressed(p.type) THEN 
      Error.Msg
        ("CM3 restriction: Open array of non-byte-aligned elements cannot "
          & "be partially subscripted (2.2.5).");
      p.type := ErrType.T;
    END; 

    Expr.NeedsAddress (p.a);
    b := ArrayType.Split (p.taBase, ti, te); <* ASSERT b *>

    EVAL Type.GetBounds (ti, mini, maxi);
    Expr.TypeCheck (p.b, cs);
    tb := Expr.TypeOf (p.b);

    Expr.GetBounds (p.b, minb, maxb);

    p.biased_b := p.b;
    IF (ti = NIL) THEN (* p.a has open array type. *)
      IF NOT Type.IsSubtype (tb, Int.T) THEN
        Error.Msg ("open arrays must be indexed by INTEGER expressions");
        tb := Int.T;
      END;
    ELSIF Type.IsSubtype (tb, Type.Base (ti)) THEN
      (* the index value's type has a common base type with the index type *)
      IF NOT TInt.EQ (mini, TInt.Zero) THEN
        IF Type.IsSubtype (tb, LInt.T)
          THEN ti := LInt.T;
          ELSE ti := Int.T;
        END;
        p.biased_b :=
            SubtractExpr.New (p.b, IntegerExpr.New (ti, mini), TRUE);
        p.biased_b.origin := p.origin;
        Expr.TypeCheck (p.biased_b, cs);
      END;
      GenRangeCheck (p, mini, maxi, minb, maxb, cs)

    ELSE
      Error.Msg ("Subscript not assignable to array's index type.");
    END;
    p.checked := TRUE; 
  END DirectCheckPass2;

(* Externally dispatched-to: *) 
PROCEDURE NeedsAddress (p: P) =
  BEGIN
    Expr.NeedsAddress (p.a);
  END NeedsAddress;

PROCEDURE SubscriptExprAlign (p: P): Type.BitAlignT =
  VAR arrayAlign, eltPack: INTEGER;
  VAR ta, ti, te, targetType: Type.T;
  VAR minb, maxb: Target.Int;
  BEGIN
    arrayAlign := Expr.Alignment(p.a);
    ta := Type.Base (Expr.TypeOf(p.a));
    IF RefType.Split (ta, (*OUT*)targetType) THEN (* Implicit dereference. *)
       ta := Type.Base (targetType)
    END;
    eltPack := ArrayType.EltPack(ta);
    IF ArrayType.Split(ta, ti, te) THEN
      IF ti # NIL THEN (* Fixed array. *)
        Expr.GetBounds (p.b, minb, maxb);
        IF TInt.LE (maxb, minb) THEN (* one or fewer elements accessible. *)
          RETURN arrayAlign (* element's align is no smaller than array's. *);
        END
      END;
      RETURN CG.GCD (arrayAlign, eltPack);
    END;
    RETURN Expr.Alignment (p.a);
  END SubscriptExprAlign;

(* Externally dispatched-to: *) 
PROCEDURE Prep (p: P) =
  VAR info: Type.Info;
  BEGIN
    PrepLV (p, traced := FALSE);
    EVAL Type.CheckInfo (p.type, info);
    IF Host.doIncGC AND info.isTraced THEN
      CASE info.class OF
      | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
        Compile (p, StaticOnly := FALSE);
        RunTyme.EmitCheckLoadTracedRef ();
        p.tmp := CG.Pop ();
      ELSE
        (* no check *)
      END
    END
  END Prep;

(* Externally dispatched-to: *) 
PROCEDURE PrepLV (p: P; traced: BOOLEAN) =
  VAR e := Expr.ConstValue (p.biased_b);
  BEGIN
    IF (e # NIL) THEN p.biased_b := e; END;
    EVAL Expr.CheckUseFailure (p.a);
    IF Expr.IsDesignator (p.a)
      THEN Expr.PrepLValue (p.a, traced);
      ELSE Expr.Prep (p.a);
    END;
    Expr.Prep (p.biased_b);
  END PrepLV;

(* Externally dispatched-to: *) 
PROCEDURE Compile (p: P; StaticOnly: BOOLEAN) =
  BEGIN
    <* ASSERT NOT StaticOnly *>
    IF p.tmp = NIL THEN
      CompileLV (p, StaticOnly := StaticOnly);
      Type.LoadScalar (p.type) (* Loads only if not a struct. *);
    ELSE
      CG.Push (p.tmp);
      CG.Free (p.tmp);
      p.tmp := NIL;
    END
  END Compile;

PROCEDURE StaticSs (p: P; VAR subscript: INTEGER): BOOLEAN (* It's static *) =
  VAR e: Expr.T;
  VAR ssIsStatic := FALSE;
  VAR t: Type.T;
  VAR subs: Target.Int;
  BEGIN
    e := Expr.ConstValue (p.biased_b);
    IF e = NIL THEN RETURN FALSE
    ELSE 
      ssIsStatic
        := IntegerExpr.Split (e, subs, t) OR EnumExpr.Split (e, subs, t);
      ssIsStatic := ssIsStatic AND TInt.ToInt (subs, subscript);
      RETURN ssIsStatic
    END;
  END StaticSs; 

(* Externally dispatched-to: *) 
PROCEDURE CompileLV (p: P; traced := FALSE; StaticOnly: BOOLEAN) =
  VAR
    ti, te      : Type.T;
    subscript   : INTEGER;
    newShapeLast: INTEGER;
    ta          := Type.Base (Expr.TypeOf (p.a));
    tb          := Type.Base (Expr.TypeOf (p.b));
    elt_pack    := ArrayType.EltPack (ta);
    newDopeVar  : CG.Var;
    b             := ArrayType.Split (ta, ti, te);
  BEGIN
    <* ASSERT NOT StaticOnly *>
    <* ASSERT b *>

    IF Expr.IsDesignator (p.a)
      THEN Expr.CompileLValue (p.a, traced);
      ELSE Expr.Compile (p.a);
    END;

    IF (p.lhsOpenDepth = 0) THEN
    (* p.a has fixed array type.
       Top of CG stack is address of the elements.
       Replace it by address of subscripted element. *) 

      IF StaticSs (p, subscript) THEN
        IF subscript # 0 THEN
          CG.Add_offset (subscript * elt_pack (* bits. *));
        END 
      ELSE
        Expr.Compile (p.biased_b);
        IF Type.IsSubtype (tb, LInt.T) THEN
          CG.Loophole (Target.Longint.cg_type, Target.Integer.cg_type);
        END;
        ArrayType.GenIndex (ta);
        CG.Boost_addr_alignment (ArrayType.EltAlign (ta));
      END;

    ELSE (* p.a has open array type. *) 
      IF p.leftSs = NIL THEN
        (* This is the leftmost subscript, to the outermost array.  Top of CG stack
           is address of the dope.  Save the dope in p.OMDopeVal and replace it
           on the CG stack by the elements' addr. *) 
        p.OMDopeVal := CG.Pop ();
        CG.Push (p.OMDopeVal);
        CG.Open_elt_ptr (ArrayType.EltAlign (ta));
        p.OMOpenDepth := p.lhsOpenDepth;
      ELSE (* Copy OMDopeVal and OMOpenDepth up from the subscript to the left. *) 
        p.OMDopeVal := p.leftSs.OMDopeVal;
        (* Top of CG stack is already the address of the elements. *)
        p.OMOpenDepth := p.leftSs.OMOpenDepth;
      END; 

      IF p.ssDepth = 1 AND p.lhsOpenDepth > 1 THEN
      
      (* This is the RM subscript, but p.a has additional open dimensions after
         this one. The final result will be an open array, so must build new
         dope and leave its address on the CG stack.  Check will have prevented
         non-open elements from being non-byte-aligned. *)
         
        (* allocate a new dope vector *)
        newDopeVar := OpenArrayType.DeclareDopeTemp (te);
        newShapeLast := p.OMOpenDepth - p.shapeSs - 2; 

        (* copy the suffix of the shape portion of the dope vector *)
        FOR i := 0 TO newShapeLast DO
          CG.Push (p.OMDopeVal);
          CG.Open_size (i+p.shapeSs+1);
          CG.Store_int
           (Target.Integer.cg_type, newDopeVar,
            M3RT.OA_sizes + i * Target.Integer.pack);
        END;

        (* build the new data pointer *)

        Expr.Compile (p.biased_b);
        IF Host.doRangeChk THEN (* range check the subscript *)
          CG.Push (p.OMDopeVal);
          CG.Open_size (p.shapeSs);
          CG.Check_index (CG.RuntimeError.SubscriptOutOfRange);
        END;

        FOR i := 0 TO newShapeLast DO
          CG.Load_int (Target.Integer.cg_type,
                       newDopeVar, M3RT.OA_sizes + i * Target.Integer.pack);
          CG.Multiply (Target.Word.cg_type);
        END;
        (* This is where we cannot handle sub-byte elements.  newDopeVar can only
           hold a byte-aligned address or better, and Index_bytes requires a
           bit size that is a multiple of byte size. *) 
        CG.Index_bytes (elt_pack);
        CG.Store_addr (newDopeVar, M3RT.OA_elt_ptr);
        CG.Load_addr_of_temp (newDopeVar, 0, Target.Address.align);
      ELSE
      (* Either there is another subscript to the right, or the element
         is not an array.  Either way, leave the address of the element
         on top of the CG stack.  It will be a CG.Val, and can represent
         a bit-level component. *)
         
        Expr.Compile (p.biased_b);
        IF Host.doRangeChk THEN (* range check the subscript *)
          CG.Push (p.OMDopeVal);
          CG.Open_size (p.shapeSs);
          CG.Check_index (CG.RuntimeError.SubscriptOutOfRange);
        END;
        FOR I := p.shapeSs + 1 TO p.OMOpenDepth - 1 DO 
          CG.Push (p.OMDopeVal);
          CG.Open_size (I);
          CG.Multiply (Target.Word.cg_type); 
        END;
(* CHECK: Can we just use some CG op directly here? See CG.Index_bits. *)
        ArrayType.GenIndex (ta); 
        CG.Boost_addr_alignment (OpenArrayType.EltAlign (ta));
      END;
    END;
  END CompileLV;

(* Externally dispatched-to: *) 
PROCEDURE IsDesignator (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN Expr.IsDesignator (p.a);
  END IsDesignator;

(* Externally dispatched-to: *) 
PROCEDURE IsWritable (p: P;  lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN Expr.IsWritable (p.a, lhs);
  END IsWritable;

(* Externally dispatched-to: *) 
PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e2, e3: Expr.T;
  BEGIN
    e1 := Expr.ConstValue (p.a);
    e2 := Expr.ConstValue (p.b);
    e3 := NIL;
    IF e1 = NIL OR e2 = NIL THEN RETURN NIL
    ELSIF ArrayExpr.ConstSubscript (e1, e2, e3) THEN RETURN e3
    ELSE RETURN NIL; END;
  END Fold;

PROCEDURE NoteWrites (p: P) =
  BEGIN
    Expr.NoteWrite (p.a);
  END NoteWrites;

(* MSIR support: fixed (non-open) 1-D subscript only for v0. The index is
   already biased to 0..N-1 in p.biased_b. *)
PROCEDURE LValueMSIR (p: P): MSIR.Value =
  VAR blk: MSIR.Block;
      arrAddr, idxVal, oa: MSIR.Value;
      heapBase := FALSE;
  BEGIN
    idxVal := Expr.CompileMSIR (p.biased_b);
    IF idxVal = NIL THEN RETURN NIL END;
    (* GEP indices are sign-extended by LLVM; narrow types (e.g. i1 for
       BOOLEAN) must be zero-extended to i64 before use as an array index. *)
    VAR idxBits := MSIR.BitWidth (MSIR.ValueType (idxVal));
        intBits := Target.Integer.size;
    BEGIN
      IF idxBits > 0 AND idxBits < intBits THEN
        blk := MSIRBuilder.CurrentBlock ();
        idxVal := MSIR.BuildZExt (blk, "", idxVal, MSIR.TI (intBits));
      END;
    END;
    IF p.lhsOpenDepth # 0 THEN
      (* Open array: try LValueMSIR on the base to detect a heap gc_ref.
         - If LValueMSIR succeeds (e.g. Subarray.LValueMSIR → ptr(OAT), or
           DerefExpr.LValueMSIR → gc_ref(OAT)): load the dope vector from the
           result.  For gc_ref(OAT), also set the GC write-barrier container.
         - If LValueMSIR fails (e.g. VAR formal: LValueMSIRDefault abandons),
           fall back to CompileMSIR which returns the dope vector value directly.
         This avoids double-emitting code (do NOT call both LValueMSIR and
         CompileMSIR for the same expression). *)
      VAR oaBase     : MSIR.Value;
          preAbandOA := MSIRBuilder.IsAbandoned ();
      BEGIN
        oaBase := Expr.LValueMSIR (p.a);
        IF oaBase # NIL THEN
          VAR oaBaseT := MSIR.ValueType (oaBase);
          BEGIN
            IF MSIR.Kind (oaBaseT) = MSIR.TypeKind.GcRef THEN
              heapBase := TRUE;
              MSIRBuilder.SetPendingContainer (oaBase);
            END;
            (* Load the dope vector through the pointer (ptr or gc_ref). *)
            blk := MSIRBuilder.CurrentBlock ();
            oa  := MSIR.BuildLoad (blk, "", MSIR.EltType (oaBaseT), oaBase);
          END;
        ELSE
          (* LValueMSIR failed — fall back: CompileMSIR returns the dope VALUE. *)
          IF NOT preAbandOA AND MSIRBuilder.IsAbandoned () THEN
            MSIRBuilder.ClearAbandoned ();
          END;
          oa := Expr.CompileMSIR (p.a);
        END;
      END;
      IF oa = NIL THEN RETURN NIL END;
      blk := MSIRBuilder.CurrentBlock ();
      (* CONST open arrays may compile to FixedArray in MSIR (size known at
         compile time).  Spill to an alloca and use BuildArrayElemAddr so the
         element address is computed without needing a dope vector. *)
      IF MSIR.Kind (MSIR.ValueType (oa)) = MSIR.TypeKind.FixedArray THEN
        VAR tmp := MSIR.BuildAlloca (blk, "", MSIR.ValueType (oa));
        BEGIN
          MSIR.BuildStore (blk, oa, tmp);
          blk := MSIRBuilder.CurrentBlock ();
          RETURN MSIR.BuildArrayElemAddr (blk, "", tmp, idxVal);
        END;
      END;
      VAR slot := MSIR.BuildOpenArrayElemAddr (blk, "", oa,
                                               ARRAY OF MSIR.Value{idxVal});
      BEGIN
        IF heapBase THEN
          VAR elemT := MSIRType.Translate (p.type);
          BEGIN
            IF elemT # NIL AND MSIR.Kind (elemT) = MSIR.TypeKind.GcRef THEN
              RETURN MSIR.RetypeValue (slot, MSIR.TGcSlot (MSIR.EltType (elemT)));
            END;
          END;
        END;
        RETURN slot;
      END;
    END;
    (* Try LValueMSIR on the base (handles VAR designators and CONST arrays).
       If that fails (LValueMSIRDefault called Abandon), clear the flag and
       fall back to rvalue materialization into a temp alloca, which handles
       call expressions that return fixed arrays. *)
    VAR preAbandoned := MSIRBuilder.IsAbandoned ();
    BEGIN
      arrAddr := Expr.LValueMSIR (p.a);
      IF arrAddr = NIL AND NOT preAbandoned AND MSIRBuilder.IsAbandoned () THEN
        (* LValueMSIR abandoned — try rvalue materialization. *)
        MSIRBuilder.ClearAbandoned ();
        VAR arrVal := Expr.CompileMSIR (p.a);
            arrT2  : MSIR.T;
        BEGIN
          IF arrVal = NIL THEN RETURN NIL END;
          arrT2 := MSIR.ValueType (arrVal);
          IF MSIR.Kind (arrT2) # MSIR.TypeKind.FixedArray THEN
            MSIRBuilder.Abandon ("rvalue subscript base is not a fixed array in MSIR");
            RETURN NIL;
          END;
          blk := MSIRBuilder.CurrentBlock ();
          VAR tmp := MSIR.BuildAlloca (blk, "", arrT2);
          BEGIN
            MSIR.BuildStore (blk, arrVal, tmp);
            arrAddr := tmp;
          END;
        END;
      END;
    END;
    IF arrAddr = NIL THEN RETURN NIL END;
    (* Verify the array type is a FixedArray (or ptr→FixedArray or gc_ref→FixedArray)
       before calling BuildArrayElemAddr.  Packed / sub-word-element arrays
       produce non-FixedArray MSIR types and are not yet supported.
       For gc_ref(FixedArray) — heap-allocated fixed array via REF ARRAY[0..N-1] OF T —
       save the base as the GC write-barrier container and retype to ptr before GEP. *)
    VAR arrT := MSIR.ValueType (arrAddr);  arrayT: MSIR.T;
    BEGIN
      IF MSIR.Kind(arrT) = MSIR.TypeKind.Ptr THEN
        arrayT := MSIR.EltType (arrT);
      ELSIF MSIR.Kind(arrT) = MSIR.TypeKind.GcRef THEN
        arrayT := MSIR.EltType (arrT);
        heapBase := TRUE;
      ELSE
        arrayT := arrT;
      END;
      (* ByteArrayFallback [N x i1]: packed-element array with no element lvalue.
         CompileMSIR handles reads via dynamic bit extraction. *)
      IF MSIR.Kind (arrayT) = MSIR.TypeKind.FixedArray AND
         MSIR.Kind (MSIR.FixedArrayElt (arrayT)) = MSIR.TypeKind.I1 THEN
        RETURN NIL;
      END;
      IF MSIR.Kind(arrayT) # MSIR.TypeKind.FixedArray THEN
        MSIRBuilder.Abandon ("packed/sub-word array subscript not yet supported in MSIR");
        RETURN NIL;
      END;
      IF heapBase THEN
        MSIRBuilder.SetPendingContainer (arrAddr);
        arrAddr := MSIR.RetypeValue (arrAddr, MSIR.TPtr (arrayT));
      END;
    END;
    blk := MSIRBuilder.CurrentBlock ();
    VAR slot := MSIR.BuildArrayElemAddr (blk, "", arrAddr, idxVal);
    BEGIN
      IF heapBase THEN
        VAR elemT := MSIRType.Translate (p.type);
        BEGIN
          IF elemT # NIL AND MSIR.Kind (elemT) = MSIR.TypeKind.GcRef THEN
            RETURN MSIR.RetypeValue (slot, MSIR.TGcSlot (MSIR.EltType (elemT)));
          END;
        END;
      END;
      RETURN slot;
    END;
  END LValueMSIR;

PROCEDURE GetPackedElemBase (p: P;  VAR idxVal: MSIR.Value;
                             VAR eltPack: INTEGER): MSIR.Value =
(* Shared helper for packed-element fixed-array subscripts.
   Computes the base pointer (points to the ByteArrayFallback byte array),
   the element index (biased, extended to Integer width), and eltPack.
   Returns NIL on failure (base not lvalue or index compilation failed). *)
  VAR arrBase: MSIR.Value;  blk: MSIR.Block;
      idxType: MSIR.T;  idxBits, intBits: INTEGER;
  BEGIN
    eltPack := ArrayType.EltPack (p.taBase);
    arrBase := Expr.LValueMSIR (p.a);
    IF arrBase = NIL THEN RETURN NIL END;
    idxVal := Expr.CompileMSIR (p.biased_b);
    IF idxVal = NIL THEN RETURN NIL END;
    idxType := MSIR.ValueType (idxVal);
    idxBits := MSIR.BitWidth (idxType);
    intBits := Target.Integer.size;
    IF idxBits > 0 AND idxBits < intBits THEN
      blk    := MSIRBuilder.CurrentBlock ();
      idxVal := MSIR.BuildZExt (blk, "", idxVal, MSIR.TI (intBits));
    END;
    RETURN arrBase;
  END GetPackedElemBase;

PROCEDURE CompilePackedElemMSIR (p: P): MSIR.Value =
(* Read a sub-byte element from a ByteArrayFallback packed array. *)
  VAR arrBase: MSIR.Value;  idxVal: MSIR.Value;  eltPack: INTEGER;
  BEGIN
    arrBase := GetPackedElemBase (p, idxVal, eltPack);
    IF arrBase = NIL THEN RETURN NIL END;
    RETURN MSIRBuilder.ExtractBitFieldDyn (arrBase, eltPack, idxVal, p.type);
  END CompilePackedElemMSIR;

PROCEDURE SubByteStoreElemMSIR (e: Expr.T;  rhs: MSIR.Value): BOOLEAN =
(* Write a sub-byte element into a ByteArrayFallback packed array. *)
  BEGIN
    TYPECASE e OF
    | P (p) =>
        IF p.lhsOpenDepth # 0 THEN RETURN FALSE END;
        IF NOT ArrayType.EltsAreBitAddressed (p.taBase) THEN RETURN FALSE END;
        VAR arrBase: MSIR.Value;  idxVal: MSIR.Value;  eltPack: INTEGER;
        BEGIN
          arrBase := GetPackedElemBase (p, idxVal, eltPack);
          IF arrBase = NIL THEN RETURN FALSE END;
          MSIRBuilder.InsertBitFieldDyn (arrBase, eltPack, idxVal, rhs);
          RETURN TRUE;
        END;
    ELSE RETURN FALSE;
    END;
  END SubByteStoreElemMSIR;

PROCEDURE CompileMSIR (p: P): MSIR.Value =
  VAR addr    : MSIR.Value;
      ty      : MSIR.T;
      blk     : MSIR.Block;
      addrType: MSIR.T;
      addrEltT: MSIR.T;
      addrBits: INTEGER;
      tyBits  : INTEGER;
      loaded  : MSIR.Value;
      lo, hi  : Target.Int;
      stripped: Type.T;
      hasBnds : BOOLEAN;
      doSExt  : BOOLEAN;
  BEGIN
    (* Packed-element fixed array: ByteArrayFallback [N x i1] sentinel. *)
    IF p.lhsOpenDepth = 0 AND ArrayType.EltsAreBitAddressed (p.taBase) THEN
      RETURN CompilePackedElemMSIR (p);
    END;
    addr := LValueMSIR (p);
    IF addr = NIL THEN RETURN NIL END;
    (* Traced ref from a heap array slot: use GcLoad (read barrier). *)
    addrType := MSIR.ValueType (addr);
    IF MSIR.Kind (addrType) = MSIR.TypeKind.GcSlot THEN
      RETURN MSIR.BuildGcLoad (MSIRBuilder.CurrentBlock (), "", addr);
    END;
    ty := MSIRType.Translate (p.type);
    IF ty = NIL THEN
      MSIRBuilder.Abandon ("unsupported subscript element type");
      RETURN NIL;
    END;
    blk := MSIRBuilder.CurrentBlock ();
    (* When the array's actual element storage width differs from the natural
       expression type, load at the storage width and cast:
       - narrow storage → wide type: ZExt (unsigned) or SExt (signed).
         E.g. [0..255] stored as i8, natural type i64 → ZExt.
       - wide storage → narrow type: Trunc.
         E.g. BOOLEAN stored as i8, natural type i1 → Trunc. *)
    addrEltT := MSIR.EltType (addrType);
    addrBits := MSIR.BitWidth (addrEltT);
    tyBits   := MSIR.BitWidth (ty);
    IF addrBits > 0 AND tyBits > 0 AND addrBits # tyBits THEN
      loaded := MSIR.BuildLoad (blk, "", addrEltT, addr);
      IF addrBits < tyBits THEN
        stripped := Type.StripPacked (p.type);
        hasBnds  := Type.GetBounds (stripped, lo, hi);
        doSExt   := hasBnds AND TInt.LT (lo, TInt.Zero);
        IF doSExt
          THEN RETURN MSIR.BuildSExt (blk, "", loaded, ty);
          ELSE RETURN MSIR.BuildZExt (blk, "", loaded, ty);
        END;
      ELSE
        RETURN MSIR.BuildTrunc (blk, "", loaded, ty);
      END;
    END;
    RETURN MSIR.BuildLoad (blk, "", ty, addr);
  END CompileMSIR;

PROCEDURE CaptureLV (p: P;  ca: CaptureAnalysis.T) =
  BEGIN
    (* Assigning a[i] modifies the array variable a — propagate the lvalue
       context.  The subscript b is always a read. *)
    Expr.CaptureLV (p.a, ca);
    Expr.Capture   (p.b, ca);
  END CaptureLV;

BEGIN
END SubscriptExpr.
