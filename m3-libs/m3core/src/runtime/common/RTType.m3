(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE RTType EXPORTS RTType, RTTypeSRC, RTHooks;

IMPORT RT0, RTMisc, RTException, RTHeapRep, RTModule, M3toC;
IMPORT Ctypes, Cstdlib, Cstring, Word, RuntimeError;

TYPE
  TK  = RT0.TypeKind;
  RTE = RuntimeError.T;

TYPE
  InfoPtr = UNTRACED REF Info;
  Info = RECORD
    def      : RT0.TypeDefn;
    opaqueID : INTEGER;
    module   : RT0.ModulePtr;
  END;

VAR
  types     := InfoMap {"typecodes", TypecodeEq, TypecodeRehash, 1024, 1024};
  uids      := InfoMap {"typeuids",  UIDEq, UIDRehash, 1024, 512};
  brands    := InfoMap {"brands",    BrandEq, BrandRehash, 512, 256};

(*------------------------------------------------ user callable routines ---*)

PROCEDURE MaxTypecode (): Typecode =
  BEGIN
    RETURN types.cnt - 1;
  END MaxTypecode;

PROCEDURE IsSubtype (a, b: Typecode): BOOLEAN =
  VAR t: RT0.TypeDefn;
  BEGIN
    IF (a = RT0.NilTypecode) THEN RETURN TRUE END;
    t := Get (a);
    IF (t = NIL) THEN RETURN FALSE; END;
    IF (t.typecode = b) THEN RETURN TRUE END;
    WHILE (t.kind = ORD (TK.Obj)) DO
      IF (t.link_state = 0) THEN FinishTypecell (t, NIL); END;
      t := LOOPHOLE (t, RT0.ObjectTypeDefn).parent; 
      IF (t = NIL) THEN RETURN FALSE; END;
      IF (t.typecode = b) THEN RETURN TRUE; END;
    END;
    IF (t.traced # 0)
      THEN RETURN (b = RT0.RefanyTypecode);
      ELSE RETURN (b = RT0.AddressTypecode);
    END;
  END IsSubtype;

PROCEDURE Supertype (tc: Typecode): Typecode =
  VAR t := Get (tc);  ot := LOOPHOLE (t, RT0.ObjectTypeDefn);
  BEGIN
    IF (t.kind # ORD (TK.Obj)) THEN RETURN NoSuchType; END;
    IF (t.link_state = 0) THEN FinishTypecell (t, NIL); END;
    IF (ot.parent = NIL) THEN RETURN NoSuchType; END;
    RETURN ot.parent.typecode;
  END Supertype;

PROCEDURE IsTraced (tc: Typecode): BOOLEAN =
  VAR t := Get (tc);
  BEGIN
    RETURN (t.traced # 0);
  END IsTraced;

PROCEDURE Get (tc: Typecode): RT0.TypeDefn =
  VAR p: SlotPtr := types.map + tc * ADRSIZE (InfoPtr);
  BEGIN
    IF (tc >= types.cnt) THEN
      <*NOWARN*> EVAL VAL (-1, CARDINAL);  (* force a range fault *)
    ELSIF (p^ = NIL) THEN
      Fail (RTE.MissingType, NIL, NIL, NIL);
    END;
    RETURN p^.def;
  END Get;

PROCEDURE GetNDimensions (tc: Typecode): CARDINAL =
  VAR t := Get (tc);
  BEGIN
    IF (t.kind = ORD (TK.Array))
      THEN RETURN LOOPHOLE (t, RT0.ArrayTypeDefn).nDimensions;
      ELSE RETURN 0;
    END;
  END GetNDimensions;

PROCEDURE TypeName (ref: REFANY): TEXT =
  VAR t := Get (TYPECODE (ref));
  BEGIN
    RETURN TypeDefnToName (t);
  END TypeName;

PROCEDURE TypecodeName (tc: Typecode): TEXT =
  VAR t := Get (tc);
  BEGIN
    RETURN TypeDefnToName (t);
  END TypecodeName;

PROCEDURE TypeDefnToName (t: RT0.TypeDefn): TEXT =
  BEGIN
    IF (t.name = NIL) THEN RETURN "<anon type>"; END;
    RETURN M3toC.CopyStoT (LOOPHOLE (t.name, Ctypes.char_star));
  END TypeDefnToName;

(*--------------------------------------------------------------- RTHooks ---*)

PROCEDURE CheckIsType (ref: REFANY;  type: ADDRESS(*RT0.TypeDefn*)): INTEGER =
  BEGIN
    RETURN ORD (IsSubtype (TYPECODE (ref),
                           LOOPHOLE (type, RT0.TypeDefn).typecode));
  END CheckIsType;

PROCEDURE ScanTypecase (ref: REFANY;
                        x: ADDRESS(*ARRAY [0..] OF Cell*)): INTEGER =
  VAR p: UNTRACED REF TypecaseCell;  i: INTEGER;  tc, xc: Typecode;
  BEGIN
    IF (ref = NIL) THEN RETURN 0; END;
    tc := TYPECODE (ref);
    p := x;  i := 0;
    LOOP
      IF (p.uid = 0) THEN RETURN i; END;
      IF (p.defn = NIL) THEN
        p.defn := FindType (p.uid);
        IF (p.defn = NIL) THEN
          Fail (RTE.MissingType, RTModule.FromDataAddress(x),
                LOOPHOLE (p.uid, ADDRESS), NIL);
        END;
      END;
      xc := LOOPHOLE (p.defn, RT0.TypeDefn).typecode;
      IF (tc = xc) OR IsSubtype (tc, xc) THEN RETURN i; END;
      INC (p, ADRSIZE (p^));  INC (i);
    END;
  END ScanTypecase;

(*--------------------------------------------------- UID -> typecell map ---*)

PROCEDURE FindType (id: INTEGER): RT0.TypeDefn =
  VAR pi := FindSlot (uids, id, NIL);  def: RT0.TypeDefn;
  BEGIN
    IF (pi^ = NIL) THEN RETURN NIL; END;
    def := pi^.def;
    IF (def = NIL) THEN RETURN NIL; END;
    IF (def.link_state = 0) THEN FinishTypecell (def, pi^.module); END;
    RETURN def;
  END FindType;

(*------------------------------------------------- brand -> typecell map ---*)

PROCEDURE NoteBrand (info: InfoPtr) =
  VAR pi := FindSlot (brands, HashBrand (info.def.brand_ptr), info);
  BEGIN
    IF (pi^ = NIL) THEN
      (* it's a new brand, record the new entry *)
      pi^ := info;  INC (brands.cnt);
    ELSE
      Fail (RTE.DuplicateBrand, info.module, info.def, pi^.def);
    END;
  END NoteBrand;

(*-------------------------------------------------------- initialization ---*)

PROCEDURE AddTypecell (def: RT0.TypeDefn;  m: RT0.ModulePtr) =
  VAR pi := FindSlot (uids, def.selfID, NIL);  in: InfoPtr;
  BEGIN
    IF (pi^ = NIL) THEN
      (* this is a new type *)
      in := NewInfo (def, m, 0);
      pi^ := in;  INC (uids.cnt);  (* record him in the UID map *)
      AssignTypecode (in);
      IF (def.brand_ptr # NIL) THEN NoteBrand (in); END;
    ELSIF (pi^.def = NIL) THEN
      (* this is the first typecell for this UID *)
      pi^.def := def;
      pi^.module := m;
      AssignTypecode (pi^);
      IF (def.brand_ptr # NIL) THEN NoteBrand (pi^); END;
    ELSE
      (* this is a duplicate typecell *)
      NoteDuplicate (pi^, def, m);
    END;
  END AddTypecell;

PROCEDURE NoteDuplicate (in: InfoPtr;  new: RT0.TypeDefn;
                         new_mod: RT0.ModulePtr) =
  VAR old := in.def;
  BEGIN
    IF (in.module = NIL) THEN in.module := new_mod; END;
    new.next := old.next;  old.next := new;
    IF (old.name = NIL) THEN old.name := new.name; END;
    IF (new.brand_ptr # NIL) THEN
      Fail (RTE.DuplicateBrand, new_mod, new, old);
    END;
    IF (old.link_state # 0) THEN UpdateCell (old, new); END;
  END NoteDuplicate;

PROCEDURE AssignTypecode (in: InfoPtr) =
  VAR pi: SlotPtr;
  BEGIN
    IF (types.cnt = 0) THEN AssignBuiltinTypes (); END;
    pi := FindSlot (types, types.cnt, NIL);
    <*ASSERT pi^ = NIL*>
    in.def.typecode := types.cnt;
    pi^ := in;  INC (types.cnt);
  END AssignTypecode;

PROCEDURE FinishObjectTypes () =
  VAR p: SlotPtr := types.map;
  BEGIN
    FOR i := 0 TO types.cnt-1 DO
      IF (p^ = NIL) THEN
        Fail (RTE.MissingType, NIL, NIL, NIL);
      ELSIF (p^.def = NIL) THEN
        Fail (RTE.MissingType, NIL, NIL, NIL);
      ELSIF (p^.def.link_state = 0) THEN
        FinishTypecell (p^.def, p^.module);
        IF (p^.def.link_state = 0) THEN
          Fail (RTE.MissingType, RTModule.FromDataAddress (p^.def), NIL, NIL);
        END;
      END;
      INC (p, ADRSIZE (p^));
    END;
  END FinishObjectTypes;

PROCEDURE FinishTypecell (def: RT0.TypeDefn;  m: RT0.ModulePtr) =
  VAR
    odef, t, u: RT0.ObjectTypeDefn;
    a: UNTRACED REF ADDRESS;
  BEGIN
    IF (def.link_state # 0) THEN RETURN; END;

    IF (def.kind = ORD (TK.Obj)) THEN
      (* finish the object definition, if possible *)
      odef := LOOPHOLE (def, RT0.ObjectTypeDefn);

      IF (odef.parent = NIL) THEN
        odef.parent := FindType (odef.parentID);
        IF (odef.parent = NIL) THEN
          (* we still can't finish this guy yet! *)
          RETURN;
        END;

        (* check for a cycle in the parent links *)
        t := odef;  u := odef;
        WHILE (u # NIL) AND (t # NIL) DO
          t := LOOPHOLE (t.parent, RT0.ObjectTypeDefn);
          IF (t = NIL) OR (t.common.kind # ORD (TK.Obj)) THEN EXIT; END;
          u := LOOPHOLE (u.parent, RT0.ObjectTypeDefn);
          IF (u = NIL) OR (u.common.kind # ORD (TK.Obj)) THEN EXIT; END;
          u := LOOPHOLE (u.parent, RT0.ObjectTypeDefn);
          IF (u = NIL) OR (u.common.kind # ORD (TK.Obj)) THEN EXIT; END;
          IF (t = u) THEN
            IF (m = NIL) THEN m := RTModule.FromDataAddress (def); END;
            Fail (RTE.SupertypeCycle, m, odef, NIL);
            RETURN;
          END;
        END;
      END;

      IF (odef.parent # NIL) THEN
        IF (odef.parent.link_state = 0) THEN
          FinishTypecell (odef.parent, NIL);
          IF (odef.parent.link_state = 0) THEN
            (* we still can't finish this guy yet! *)
            RETURN;
          END;
        END;
      END;

      IF (odef.parent # NIL) AND (odef.dataOffset = 0) THEN
        t := LOOPHOLE (odef.parent, RT0.ObjectTypeDefn);
        IF (t.common.kind # ORD (TK.Obj)) THEN
          odef.dataOffset   := ADRSIZE (ADDRESS);
          odef.methodOffset := 0;
        ELSE
          odef.dataOffset := RTMisc.Upper (t.common.dataSize, def.dataAlignment);
          INC (def.dataSize, odef.dataOffset);
          def.dataAlignment := MAX (def.dataAlignment, t.common.dataAlignment);
          odef.methodOffset := t.methodSize;
          INC (odef.methodSize, odef.methodOffset);
        END;
      END;

      (* allocate my default method list *)
      IF (odef.methodSize > 0) AND (odef.defaultMethods = NIL) THEN
        odef.defaultMethods := Cstdlib.calloc (1, odef.methodSize);
        IF odef.defaultMethods = NIL THEN
          Fail (RTE.OutOfMemory, m, odef, NIL);
        END;

        (* initialize my method suite from my parent *)
        IF (t.common.kind = ORD (TK.Obj)) AND (t.defaultMethods # NIL) THEN
          RTMisc.Copy (t.defaultMethods, odef.defaultMethods, t.methodSize);
        END;

        (* call the link proc to fill in any other methods... *)
        IF (odef.linkProc # NIL) THEN   odef.linkProc (def); END;
      END;

      (* initialize any remaining methods to the undefined procedure *)
      IF (odef.methodSize > 0) THEN
        a := odef.defaultMethods;
        FOR j := 0 TO odef.methodSize DIV BYTESIZE (ADDRESS) - 1 DO
          IF (a^ = NIL) THEN a^ := LOOPHOLE (UndefinedMethod, ADDRESS) END;
          INC (a, ADRSIZE (a^));
        END;
      END;
    END;

    (* everybody gets a size that's a multiple of a header word *)
    (* See comments in RT0.i3, regarding dataAlignment. *)

    def.dataSize := RTMisc.Upper (def.dataSize, BYTESIZE (RTHeapRep.Header));

    (* check that all data alignments are small powers of two so that
|          "RTMisc.Align (addr, alignment)"
       can be safely replaced by
|          "addr + align [Word.And (addr, 7), alignment]"
       in "RTHeapRep.AllocTraced".*)
    IF  (def.dataAlignment # 4) AND (def.dataAlignment # 8)
    AND (def.dataAlignment # 1) AND (def.dataAlignment # 2) THEN
      IF (m = NIL) THEN m := RTModule.FromDataAddress (m); END;
      Fail (RTE.ValueOutOfRange, m, def, NIL);
    END;

    (* ensure that any equivalent typecells are also "finished" *)
    VAR d: RT0.TypeDefn := def;  BEGIN
      WHILE (d.next # NIL) DO  d := d.next;  UpdateCell (def, d);  END;
    END;

    def.link_state := 1;
  END FinishTypecell;

PROCEDURE UpdateCell (old, new: RT0.TypeDefn) =
  (* make sure any computed information is copied into the new cell *)
  BEGIN
    new.typecode       := old.typecode;
    new.link_state     := old.link_state;
    new.dataAlignment  := old.dataAlignment;
    new.dataSize       := old.dataSize;

    IF (new.kind = ORD (TK.Obj)) THEN
      VAR
        onew := LOOPHOLE (new, RT0.ObjectTypeDefn);
        oold := LOOPHOLE (old, RT0.ObjectTypeDefn);
      BEGIN
        onew.dataOffset     := oold.dataOffset;
        onew.methodOffset   := oold.methodOffset;
        onew.methodSize     := oold.methodSize;
        onew.defaultMethods := oold.defaultMethods;
        onew.parent         := oold.parent;
      END;
    END;
  END UpdateCell;

PROCEDURE ResolveTypeLink (uid: INTEGER;  t: RT0.TypeLinkPtr;  m: RT0.ModulePtr) =
  VAR pi := FindSlot (uids, uid, NIL);
  BEGIN
    IF (pi^ = NIL) OR (pi^.def = NIL) THEN
      Fail (RTE.MissingType, m, LOOPHOLE (uid, ADDRESS), NIL);
    ELSE
      t.defn     := pi^.def;
      t.typecode := pi^.def.typecode;
    END;
  END ResolveTypeLink;

PROCEDURE NoteFullRevelation (r: RT0.RevPtr;  m: RT0.ModulePtr) =
  VAR rhs, lhs: InfoPtr;  p_rhs, p_lhs: SlotPtr;  old_tc: INTEGER;
  BEGIN
    p_rhs := FindSlot (uids, r.rhs_id, NIL);  rhs := NIL;
    p_lhs := FindSlot (uids, r.lhs_id, NIL);  lhs := NIL;
    IF (p_rhs^ # NIL) THEN rhs := p_rhs^; END;
    IF (p_lhs^ # NIL) THEN lhs := p_lhs^; END;

    IF (rhs = NIL) OR (rhs.def = NIL) THEN
      Fail (RTE.MissingType, m, LOOPHOLE (r.rhs_id, ADDRESS), NIL);
    END;

    IF (lhs = NIL) THEN
      p_lhs^ := rhs;  INC (uids.cnt);  (* ok, remember the binding *)
      IF (rhs.opaqueID # 0) AND (rhs.opaqueID # r.lhs_id) THEN
        Fail (RTE.OpaqueTypeRedefined, m, LOOPHOLE (r.lhs_id, ADDRESS),
              LOOPHOLE (r.rhs_id, ADDRESS));
      END;
      rhs.opaqueID := r.lhs_id;

    ELSIF (lhs = rhs) THEN
      (* ok, the two types are already identified *)

    ELSIF (r.lhs_id = RT0.TextLiteralID) AND (lhs.def = NIL) THEN

      (* steal the RHS typecell for the opaque type *)
      old_tc := rhs.def.typecode;
      rhs.def.typecode := RT0.TextLitTypecode;
      rhs.opaqueID := RT0.TextLiteralID;
      p_lhs^ := rhs; (* fix the UID mapping *)

      (* fix the old RHS typecode to use the LHS info & a dummy typecell *)
      p_lhs := types.map + old_tc * ADRSIZE (InfoPtr);
      p_lhs^ := lhs;
      lhs.def := ADR (Dummy1_typecell);
      lhs.def.typecode := old_tc;
      lhs.opaqueID := 0;

      (* fix the LHS typecode to use the new RHS info *)
      p_lhs := types.map + RT0.TextLitTypecode * ADRSIZE (InfoPtr);
      p_lhs^ := rhs;

    ELSIF (r.lhs_id = RT0.MutexID) AND (lhs.def = NIL) THEN

      (* steal the RHS typecell for the opaque type *)
      old_tc := rhs.def.typecode;
      rhs.def.typecode := RT0.MutexTypecode;
      rhs.opaqueID := RT0.MutexID;
      p_lhs^ := rhs; (* fix the UID mapping *)

      (* fix the old RHS typecode to use the LHS info & a dummy typecell *)
      p_lhs := types.map + old_tc * ADRSIZE (InfoPtr);
      p_lhs^ := lhs;
      lhs.def := ADR (Dummy3_typecell);
      lhs.def.typecode := old_tc;
      lhs.opaqueID := 0;

      (* fix the LHS typecode to use the new RHS info *)
      p_lhs := types.map + RT0.MutexTypecode * ADRSIZE (InfoPtr);
      p_lhs^ := rhs;

    ELSE
      Fail (RTE.OpaqueTypeRedefined, m, LOOPHOLE (r.lhs_id, ADDRESS),
            LOOPHOLE (r.rhs_id, ADDRESS));
    END;
  END NoteFullRevelation;

PROCEDURE VerifyPartialRevelation (r: RT0.RevPtr;  m: RT0.ModulePtr) =
  VAR rhs, lhs: InfoPtr;  p_rhs, p_lhs: SlotPtr;
  BEGIN
    p_rhs := FindSlot (uids, r.rhs_id, NIL);  rhs := NIL;
    p_lhs := FindSlot (uids, r.lhs_id, NIL);  lhs := NIL;
    IF (p_rhs^ # NIL) THEN rhs := p_rhs^; END;
    IF (p_lhs^ # NIL) THEN lhs := p_lhs^; END;

    IF (lhs = NIL) OR (lhs.def = NIL) THEN
      Fail (RTE.MissingType, m, LOOPHOLE (r.lhs_id, ADDRESS), NIL);
    ELSIF (rhs = NIL) OR (rhs.def = NIL) THEN
      Fail (RTE.MissingType, m, LOOPHOLE (r.rhs_id, ADDRESS), NIL);
    ELSIF NOT IsSubtype (lhs.def.typecode, rhs.def.typecode) THEN
      Fail (RTE.InconsistentRevelation, m, LOOPHOLE (r.lhs_id, ADDRESS),
            LOOPHOLE (r.rhs_id, ADDRESS));
    END;
  END VerifyPartialRevelation;

(*--------------------------------------------------------- builtin types ---*)

PROCEDURE AssignBuiltinTypes () =
  BEGIN
    GenOpaque  (RT0.TextLiteralID, RT0.TextLitTypecode);
    GenOpaque  (RT0.MutexID,       RT0.MutexTypecode);
    GenBuiltin (ADR (NULL_typecell),     "NULL");
    GenBuiltin (ADR (REFANY_typecell),   "REFANY");
    GenBuiltin (ADR (ADDRESS_typecell),  "ADDRESS");
    GenBuiltin (ADR (ROOT_typecell.common), "ROOT");
    GenBuiltin (ADR (UNROOT_typecell.common), "UNTRACED ROOT");
    types.cnt := MAX (types.cnt, RT0.FirstUserTypecode);
  END AssignBuiltinTypes;

PROCEDURE GenBuiltin (def: RT0.TypeDefn;  nm: TEXT) =
  VAR pi: SlotPtr;  in := NewInfo (def, NIL, 0);
  BEGIN
    def.name := LOOPHOLE (M3toC.FlatTtoS (nm), RT0.String);
    pi := FindSlot (uids, def.selfID, NIL);
    <*ASSERT pi^ = NIL*>
    pi^ := in;  INC (uids.cnt);
    pi := FindSlot (types, def.typecode, NIL);
    <*ASSERT pi^ = NIL*>
    pi^ := in;  INC (types.cnt);
  END GenBuiltin;

PROCEDURE GenOpaque (uid: INTEGER;  typecode: INTEGER) =
  VAR pi: SlotPtr;  in := NewInfo (NIL, NIL, uid);
  BEGIN
    pi := FindSlot (uids, uid, NIL);
    <*ASSERT pi^ = NIL*>
    pi^ := in;  INC (uids.cnt);
    pi := FindSlot (types, typecode, NIL);
    <*ASSERT pi^ = NIL*>
    pi^ := in;  INC (types.cnt);
  END GenOpaque;

VAR
  Dummy1_typecell := RT0.Typecell {
    typecode      := 0,
    selfID        := -1,
    fp            := RT0.Fingerprint {16_ff, 16_ff, 16_ff, 16_ff,
                                      16_ff, 16_ff, 16_ff, 16_ff},
    traced        := 0,
    kind          := ORD (TK.Ref),
    link_state    := 0,
    dataAlignment := 1,
    dataSize      := 0,
    type_map      := NIL,
    gc_map        := NIL,
    type_desc     := NIL,
    initProc      := NIL,
    brand_ptr     := NIL,
    name          := NIL,
    next          := NIL
  };
  Dummy3_typecell := RT0.Typecell {
    typecode      := 0,
    selfID        := -3,
    fp            := RT0.Fingerprint {16_fd, 16_ff, 16_ff, 16_ff,
                                      16_fd, 16_ff, 16_ff, 16_ff},
    traced        := 0,
    kind          := ORD (TK.Ref),
    link_state    := 0,
    dataAlignment := 1,
    dataSize      := 0,
    type_map      := NIL,
    gc_map        := NIL,
    type_desc     := NIL,
    initProc      := NIL,
    brand_ptr     := NIL,
    name          := NIL,
    next          := NIL
  };

(* FP ("$null") ==> 16_248000006c6c756e => 16_48ec756e = 1223456110 *)
VAR
  NULL_typecell := RT0.Typecell {
    typecode      := RT0.NilTypecode,
    selfID        := 16_48ec756e,
    fp            := RT0.Fingerprint {16_6e, 16_75, 16_6c, 16_6c,
                                      16_00, 16_00, 16_80, 16_24},
    traced        := 0,
    kind          := ORD (TK.Ref),
    link_state    := 0,
    dataAlignment := 1,
    dataSize      := 0,
    type_map      := NIL,
    gc_map        := NIL,
    type_desc     := NIL,
    initProc      := NIL,
    brand_ptr     := NIL,
    name          := NIL,
    next          := NIL
  };

(* FP ("$objectadr") ==> 16_f80919c87187be41 => 16_898ea789 = -1987139703 *)
VAR
  UNROOT_typecell := RT0.ObjectTypecell {
    common := RT0.Typecell {
      typecode      := RT0.UnRootTypecode,
      selfID        := -1987139703,
      fp            := RT0.Fingerprint {16_41, 16_be, 16_87, 16_71,
                                        16_c8, 16_19, 16_09, 16_f8},
      traced        := 0,
      kind          := ORD (TK.Obj),
      link_state    := 0,
      dataAlignment := BYTESIZE (ADDRESS),
      dataSize      := BYTESIZE (ADDRESS),
      type_map      := NIL,
      gc_map        := NIL,
      type_desc     := NIL,
      initProc      := NIL,
      brand_ptr     := NIL,
      name          := NIL,
      next          := NIL },
    parentID      := ADDRESS_uid,
    linkProc      := NIL,
    dataOffset    := ADRSIZE (ADDRESS),
    methodOffset  := 0,
    methodSize    := 0,
    defaultMethods:= NIL,
    parent        := NIL
  };

(* FP ("$objectref") ==> 16_f80919c86586ad41 => 16_9d8fb489 = -1651526519 *)
VAR
  ROOT_typecell := RT0.ObjectTypecell {
    common := RT0.Typecell {
      typecode      := RT0.RootTypecode,
      selfID        := -1651526519,
      fp            := RT0.Fingerprint {16_41, 16_ad, 16_86, 16_65,
                                        16_c8, 16_19, 16_09, 16_f8},
      traced        := 1,
      kind          := ORD (TK.Obj),
      link_state    := 0,
      dataAlignment := BYTESIZE (ADDRESS),
      dataSize      := BYTESIZE (ADDRESS),
      type_map      := NIL,
      gc_map        := NIL,
      type_desc     := NIL,
      initProc      := NIL,
      brand_ptr     := NIL,
      name          := NIL,
      next          := NIL },
    parentID      := REFANY_uid,
    linkProc      := NIL,
    dataOffset    := ADRSIZE (ADDRESS),
    methodOffset  := 0,
    methodSize    := 0,
    defaultMethods:= NIL,
    parent        := NIL
  };

(* FP ("$refany") ==> 16_65722480796e6166 => 16_1c1c45e6 = 471614950 *)
CONST
  REFANY_uid = 16_1c1c45e6;
VAR
  REFANY_typecell := RT0.Typecell {
    typecode      := RT0.RefanyTypecode,
    selfID        := REFANY_uid,
    fp            := RT0.Fingerprint {16_66, 16_61, 16_6e, 16_79,
                                      16_80, 16_24, 16_72, 16_65},
    traced        := 1,
    kind          := ORD (TK.Ref),
    link_state    := 0,
    dataAlignment := BYTESIZE (ADDRESS),
    dataSize      := BYTESIZE (ADDRESS),
    type_map      := NIL,
    gc_map        := NIL,
    type_desc     := NIL,
    initProc      := NIL,
    brand_ptr     := NIL,
    name          := NIL,
    next          := NIL
  };

(* FP ("$address") ==> 16_628a21916aca01f2 => 16_8402063 = 138420323 *)
CONST
  ADDRESS_uid = 138420323;
VAR
  ADDRESS_typecell := RT0.Typecell {
    typecode      := RT0.AddressTypecode,
    selfID        := ADDRESS_uid,
    fp            := RT0.Fingerprint {16_f2, 16_01, 16_ca, 16_6a,
                                      16_91, 16_21, 16_8a, 16_62},
    traced        := 0,
    kind          := ORD (TK.Ref),
    link_state    := 0,
    dataAlignment := BYTESIZE (ADDRESS),
    dataSize      := BYTESIZE (ADDRESS),
    type_map      := NIL,
    gc_map        := NIL,
    type_desc     := NIL,
    initProc      := NIL,
    brand_ptr     := NIL,
    name          := NIL,
    next          := NIL
  };

(*----------------------------------------------------------- Info cells ---*)

CONST
  InfoChunk = 512;

TYPE
  InfoVec = UNTRACED REF ARRAY [0..InfoChunk-1] OF Info;

VAR
  n_info    : CARDINAL := InfoChunk;
  info_pool : InfoVec  := NIL;

PROCEDURE NewInfo (def: RT0.TypeDefn; m: RT0.ModulePtr; uid: INTEGER): InfoPtr =
  VAR p: InfoPtr;
  BEGIN
    IF (n_info >= InfoChunk) THEN
      info_pool := Cstdlib.calloc (InfoChunk, BYTESIZE (Info));
      IF info_pool = NIL THEN
        Fail (RTE.OutOfMemory, m, def, NIL);
      END;
      n_info := 0;
    END;
    p := info_pool + n_info * ADRSIZE (Info);
    INC (n_info);
    p.def := def;
    p.module := m;
    p.opaqueID := uid;
    RETURN p;
  END NewInfo;

(*---------------------------------------------------- key->InfoPtr maps ---*)

TYPE
  SlotPtr = UNTRACED REF InfoPtr;

  InfoMap = RECORD
    name         : TEXT;
    is_equal     : PROCEDURE (key: INTEGER;  aux: ADDRESS;  info: InfoPtr): BOOLEAN;
    rehash       : PROCEDURE (info: InfoPtr;  VAR key1, key2: INTEGER);
    initial_size : CARDINAL;
    full         : CARDINAL;
    cnt          : CARDINAL := 0;
    max          : CARDINAL := 0;  (* must be a power of two! *)
    mask         : INTEGER  := 0;
    map          : ADDRESS  := NIL;  (* UNTRACED REF ARRAY [0..max-1] OF InfoPtr *)
  END;

PROCEDURE FindSlot (VAR m: InfoMap;  key: INTEGER;  aux: ADDRESS): SlotPtr =
  VAR x: INTEGER;  pi: SlotPtr;
  BEGIN
    IF (m.map = NIL) OR (m.cnt >= m.full) THEN Expand (m); END;
    x  := Word.And (m.mask, key);
    pi := m.map + x * ADRSIZE (InfoPtr);
    LOOP
      IF (pi^ = NIL) OR m.is_equal (key, aux, pi^) THEN
        (* we found an empty slot or a match *)
        RETURN pi;
      END;
      INC (x); INC (pi, ADRSIZE (pi^));
      IF (x >= m.max) THEN x := 0;  pi := m.map;  END;
    END;
  END FindSlot;

PROCEDURE Expand (VAR m: InfoMap) =
  CONST NOKEY = FIRST(INTEGER);
  VAR
    new : InfoMap;
    pi, pt: SlotPtr;
    key1, key2: INTEGER;
  BEGIN
    IF m.map = NIL THEN
      (* First time... *)
      m.cnt  := 0;
      m.max  := m.initial_size;  (* must be a power of two *)
      m.mask := m.max - 1;
      m.map  := Cstdlib.calloc (m.max, BYTESIZE (InfoPtr));
      IF m.map = NIL THEN
        Fail (RTE.OutOfMemory, NIL, NIL, NIL);
      END;
    ELSE
      new := m;
      new.cnt  := 0;
      new.full := m.full + m.full;
      new.max  := m.max + m.max;
      new.mask := new.max - 1;
      new.map  := Cstdlib.calloc (new.max, BYTESIZE (InfoPtr));
      IF new.map = NIL THEN
        Fail (RTE.OutOfMemory, NIL, NIL, NIL);
      END;

      (* re-insert the existing elements *)
      pi := m.map;
      FOR i := 0 TO m.max-1 DO
        IF (pi^ # NIL) THEN
          key1 := NOKEY;  key2 := NOKEY;
          m.rehash (pi^, key1, key2);
          IF (key1 # NOKEY) THEN
            pt := FindSlot (new, key1, pi^);
            IF (pt^ = NIL) THEN pt^ := pi^; INC (new.cnt); END;
          END;
          IF (key2 # NOKEY) THEN
            pt := FindSlot (new, key2, pi^);
            IF (pt^ = NIL) THEN pt^ := pi^; INC (new.cnt); END;
          END;
        END;
        INC (pi, ADRSIZE (pi^));
      END;

      (* free the old map and reset it to the new one *)
      Cstdlib.free (m.map);
      m := new;      
    END;
  END Expand;

PROCEDURE TypecodeEq (key: INTEGER;  <*UNUSED*>aux: ADDRESS;
                      info: InfoPtr): BOOLEAN =
  BEGIN
    RETURN info.def.typecode = key;
  END TypecodeEq;

PROCEDURE TypecodeRehash (info: InfoPtr;  VAR key1: INTEGER;
                          <*UNUSED*>VAR key2: INTEGER) =
  BEGIN
    key1 := info.def.typecode;
  END TypecodeRehash;

PROCEDURE UIDEq (key: INTEGER;  <*UNUSED*>aux: ADDRESS;
                 info: InfoPtr): BOOLEAN =
  BEGIN
    RETURN (info.opaqueID = key)
        OR ((info.def # NIL) AND (info.def.selfID = key));
  END UIDEq;

PROCEDURE UIDRehash (info: InfoPtr;  VAR key1, key2: INTEGER) =
  BEGIN
    IF (info.def # NIL)    THEN key1 := info.def.selfID; END;
    IF (info.opaqueID # 0) THEN key2 := info.opaqueID;   END;
  END UIDRehash;

PROCEDURE BrandEq (<*UNUSED*> key: INTEGER;  aux: ADDRESS;
                   info: InfoPtr): BOOLEAN =
  VAR
    x: RT0.BrandPtr := LOOPHOLE (aux, InfoPtr).def.brand_ptr;
    y: RT0.BrandPtr;
  BEGIN
    IF (info.def = NIL) OR (info.def.brand_ptr = NIL) THEN RETURN FALSE; END;
    y := info.def.brand_ptr;
    RETURN (x.length = y.length)
       AND Cstring.memcmp (ADR(x.chars[0]), ADR(y.chars[0]), x.length) = 0;
  END BrandEq;

PROCEDURE BrandRehash (info: InfoPtr;  VAR key1: INTEGER;
                       <*UNUSED*>VAR key2: INTEGER) =
  BEGIN
    IF (info.def # NIL) AND (info.def.brand_ptr # NIL) THEN
      key1 := HashBrand (info.def.brand_ptr);
    END;
  END BrandRehash;

PROCEDURE HashBrand (b: RT0.BrandPtr): INTEGER =
  VAR
    hash : INTEGER := 0;
    len  : INTEGER := b.length;
    cp   : UNTRACED REF CHAR := ADR (b.chars[0]);
  BEGIN
    WHILE (len > 0) DO
      hash := Word.Plus (Word.LeftShift (hash, 1), ORD (cp^));
      INC (cp, BYTESIZE (cp^));  DEC (len);
    END;
    RETURN hash;
  END HashBrand;

(*-------------------------------------------------------- runtime errors ---*)

PROCEDURE UndefinedMethod (self: REFANY) =
  VAR
    tc   : INTEGER       := TYPECODE (self);
    info : InfoPtr       := types.map + tc * ADRSIZE (Info);
    def  : RT0.TypeDefn  := NIL;
    m    : RT0.ModulePtr := NIL;
  BEGIN
    IF (tc < types.cnt) AND (info # NIL) THEN
      def := info.def;
      m := info.module;
    END;
    Fail (RTE.UndefinedMethod, m, def, NIL);
  END UndefinedMethod;

PROCEDURE Fail (rte: RTE;  m: RT0.ModulePtr;  x, y: ADDRESS) =
  <*FATAL ANY*>
  VAR a: RT0.RaiseActivation;
  BEGIN
    a.exception   := RuntimeError.Self ();
    a.arg         := LOOPHOLE (ORD (rte), RT0.ExceptionArg);
    a.module      := m;
    a.line        := 0;
    a.pc          := NIL;
    a.info0       := x;
    a.info1       := y;
    a.un_except   := NIL;
    a.un_arg      := NIL;
    RTException.Raise (a);
  END Fail;

BEGIN
END RTType.

