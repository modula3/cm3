(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE M3Type;

IMPORT M3ID, Target, TargetMap, TInt, TWord;

REVEAL (* private methods of the types *)
  T = ROOT BRANDED "M3Type.T" OBJECT
  METHODS
    get_info    (VAR(*OUT*) x: Info) RAISES {Error};
    base        (): T;
    is_ordinal  () : BOOLEAN;
    get_bounds  (VAR(*OUT*) min, max: Target.Int): BOOLEAN;
    (******
    is_equal    (b: T): BOOLEAN;
    is_subtype  (b: T): BOOLEAN;
    ******)
  END;

EXCEPTION Error (TEXT);

(*--------------------------------------------- public access procedures ---*)

PROCEDURE GetInfo (t: T;  VAR(*OUT*) x: Info) =
  BEGIN
    TRY
      x.err_msg := NIL;
      t.get_info (x);
    EXCEPT Error (msg) =>
      x.class   := Class.Unknown;
      x.err_msg := msg;
    END;
  END GetInfo;

PROCEDURE Base (t: T): T =
  BEGIN
    RETURN t.base ();
  END Base;

PROCEDURE IsOrdinal (t: T): BOOLEAN =
  BEGIN
    RETURN t.is_ordinal ();
  END IsOrdinal;

PROCEDURE Number (t: T): Target.Int =
  VAR min, max, tmp: Target.Int;
  BEGIN
    IF t.get_bounds (min, max)
      AND TInt.Subtract (max, min, tmp)
      AND TInt.Add (tmp, TInt.One, max) THEN
      RETURN max;
    END;
    RETURN Target.Integer.max;
  END Number;

PROCEDURE GetBounds (t: T;  VAR min, max: Target.Int): BOOLEAN =
  BEGIN
    RETURN t.get_bounds (min, max);
  END GetBounds;

PROCEDURE IsEqual (a, b: T): BOOLEAN =
  BEGIN
    RETURN (a = b); (* OR a.is_equal (b) *)
  END IsEqual;

(***************************** NOT IMPLEMENTED ***************************

PROCEDURE IsSubtype (a, b: T): BOOLEAN =
  BEGIN
    RETURN a.is_subtype (b);
  END IsSubtype;

PROCEDURE IsAssignable (a, b: T;  safe: BOOLEAN): BOOLEAN =
  VAR i, e: T;  min_a, max_a, min_b, max_b, min, max: Target.Int;
  BEGIN
    IF IsEqual (a, b) OR IsSubtype (b, a) THEN
      RETURN TRUE;
    ELSIF IsOrdinal (a) THEN
      (* ordinal types:  OK if there is a common supertype
         and they have at least one member in common. *)
      IF IsEqual (Base(a), Base(b), NIL)
         AND GetBounds (a, min_a, max_a)
         AND GetBounds (b, min_b, max_b) THEN
        (* check for a non-empty intersection *)
        min := min_a;  IF TInt.LT (min, min_b) THEN min := min_b; END;
        max := max_a;  IF TInt.LT (max_b, max) THEN max := max_b; END;
        RETURN TInt.LE (min, max);
      ELSE
        RETURN FALSE;
      END;
    ELSIF IsSubtype (a, b) THEN
      (* may be ok, but must narrow rhs before doing the assignment *)
      RETURN IsSubtype (b, Refany)
          OR ArrayType.Split (b, i, e)
          OR (IsSubtype (b, Address)
              AND (NOT safe OR NOT IsEqual (b, Addr.T)));
    ELSE
      RETURN FALSE;
    END;
  END IsAssignable;

***************************** NOT IMPLEMENTED ***************************)

(*--------------------------------------------------------------- ARRAY ---*)

TYPE
  PublicArray = T OBJECT
    index   : T;
    element : T;
  END;

REVEAL
  Array = PublicArray BRANDED "M3Type.Array" OBJECT OVERRIDES
    get_info   := GetArrayInfo;
    base       := SelfBase;
    is_ordinal := IsNever;
    get_bounds := NoBounds;
    (**********************
    is_equal   := ArrayEQ;
    is_subtype := ArraySubtype;
    **********************)
  END;

PROCEDURE GetArrayInfo (self: Array;  VAR x: Info) RAISES {Error} =
  VAR
    elt        : Info;
    n_elts     : INTEGER;
    align      : INTEGER;
    full_size  : INTEGER;
    elt_pack   : INTEGER;
    total_size : INTEGER;
    packed     : BOOLEAN;
  BEGIN
    self.element.get_info (elt);

    IF NOT IsOrdinal (self.index) THEN
      Err ("array index type must be an ordinal type");
    END;
    IF (elt.class = Class.OpenArray) THEN
      Err ("array element type cannot be an open array");
    END;
    IF NOT TInt.ToInt (Number (self.index), n_elts) THEN
      Restrict ("array has too many elements");
    END;

    align     := elt.alignment;
    elt_pack  := elt.size;
    IF (elt.class # Class.Packed) THEN
      (* naturally aligned elements must be OK *)
      elt_pack := (elt.size + align - 1) DIV align * align;
      packed   := FALSE;
    ELSE
      (* find a packing that's allowed *)
      (* align  := FindAlignment (p); *)
      packed := (elt_pack < Target.Byte) OR (elt_pack MOD align # 0);
    END;

    IF (n_elts > 0) AND (elt_pack > 0)
      AND (n_elts > LAST (INTEGER) DIV elt_pack) THEN
      Restrict ("array type too large");
    END;

    full_size  := elt_pack * n_elts;
    total_size := RoundUp (full_size, align);

    x.size       := total_size;
    x.min_size   := total_size;
    x.alignment  := align;
    x.class      := Class.Array;
    x.is_traced  := elt.is_traced;
    x.is_empty   := elt.is_empty;
    x.is_solid   := elt.is_solid
                       AND (elt_pack <= elt.size)
                       AND (total_size <= full_size);
  END GetArrayInfo;

(***************************** NOT IMPLEMENTED ******************************

PROCEDURE ArrayEQ (a: Array;  b: T): BOOLEAN =
  BEGIN
    TYPECASE b OF
    | Array (bb) => RETURN IsEqual (a.element, bb.element)
                       AND IsEqual (a.index, bb.index);
    ELSE RETURN FALSE;
    END;
  END ArrayEQ;

PROCEDURE ArraySubtype (a: Array;  tb: T): BOOLEAN =
  VAR ta, eb: T;  b: Array;
  BEGIN
    ta := a;

    (* peel off the fixed dimensions of A and open dimensions of B *)
    LOOP
      a := ReduceArray (ta);
      IF (a = NIL) OR NOT OpenArrayType.Split (tb, eb) THEN EXIT END;
      ta := a.element;
      tb := eb;
    END;

    (* peel off the fixed dimensions as long as the sizes are equal *)
    LOOP
      a := ReduceArray (ta);  b := ReduceArray (tb);
      IF (a = NIL) OR (b = NIL) THEN EXIT END;
      IF (a.index # b.index) THEN
        IF Number (a.index) # Number (b.index) THEN RETURN FALSE END;
      END;
      ta := a.element;
      tb := b.element;
    END;

    RETURN IsEqual (ta, tb);
  END ArraySubtype;

PROCEDURE ReduceArray (t: T): Array =
  BEGIN
    TYPE t OF
    | NULL      => RETURN NIL;
    | Array (a) => RETURN a;
    ELSE           RETURN NIL;
    END;
  END ReduceArray;

***************************** NOT IMPLEMENTED ******************************)


(*-------------------------------------------------------- Enumerations ---*)

TYPE
  PublicEnum = T OBJECT
    elements : REF ARRAY OF M3ID.T;
  END;

REVEAL
  Enum = PublicEnum BRANDED "M3Type.Enum" OBJECT OVERRIDES
    get_info   := GetEnumInfo;
    base       := SelfBase;
    is_ordinal := IsAlways;
    get_bounds := EnumBounds;
    (***********************
    is_equal   := EnumEQ;
    is_subtype := EnumEQ;
    ***********************)
  END;

PROCEDURE GetEnumInfo (self: Enum;  VAR x: Info) RAISES {Error} =
  VAR n_elts := NUMBER (self.elements^);   max: Target.Int;  rep: EnumRep;
  BEGIN
    IF NOT TInt.FromInt (n_elts-1, max) THEN
      Err ("enumeration type too large");
    END;
    rep := FindEnumRep (max);

    x.min_size  := MinEnumSize (n_elts);
    x.size      := TargetMap.Word_types[rep].size;
    x.alignment := TargetMap.Word_types[rep].align;
    x.class     := Class.Enum;
    x.is_traced := FALSE;
    x.is_empty  := (n_elts <= 0);
    x.is_solid  := TRUE;
  END GetEnumInfo;

TYPE
  EnumRep = [FIRST (TargetMap.Word_types) .. LAST (TargetMap.Word_types)];

PROCEDURE FindEnumRep (READONLY max: Target.Int): EnumRep =
  BEGIN
    FOR i := FIRST (EnumRep) TO LAST (EnumRep) DO
      WITH t = TargetMap.Word_types[i] DO
        IF (t.size <= Target.Word.size) AND TInt.LE (max, t.max) THEN
          RETURN i;
        END;
      END;
    END;
    RETURN LAST (EnumRep);
  END FindEnumRep;

PROCEDURE MinEnumSize (n_elts: INTEGER): INTEGER =
  VAR i, j: INTEGER;
  BEGIN
    j := 1;  i := 2;
    WHILE (n_elts > i) DO INC (j); INC (i, i);  END;
    RETURN j;
  END MinEnumSize;

PROCEDURE EnumBounds (self: Enum;  VAR min, max: Target.Int): BOOLEAN =
  VAR b: BOOLEAN;
  BEGIN
    min := TInt.Zero;
    b := TInt.FromInt (NUMBER (self.elements^) - 1, max);  <*ASSERT b*>
    RETURN TRUE;
  END EnumBounds;

(***************************** NOT IMPLEMENTED ******************************
PROCEDURE EnumEQ (a: Enum;  tb: T): BOOLEAN =
  BEGIN
    TYPECASE tb OF
    | Enum (b) =>
        IF NUMBER (a.elements^) # NUMBER (b.elements^) THEN RETURN FALSE; END;
        FOR i := 0 TO LAST (a.elements^) DO
          IF (a.elements[i] # b.elements[i]) THEN RETURN FALSE; END;
        END;
        RETURN TRUE;
    ELSE
        RETURN FALSE
    END;
  END EnumEQ;
***************************** NOT IMPLEMENTED ******************************)

(*-------------------------------------------------------------- OBJECT ---*)

TYPE
  PublicObject = T OBJECT
    brand     : TEXT;
    super     : T;
    fields    : REF ARRAY OF FieldDesc;
    methods   : REF ARRAY OF MethodDesc;
    overrides : REF ARRAY OF MethodDesc;
  END;

REVEAL
  Object = PublicObject BRANDED "M3Type.Object" OBJECT OVERRIDES
    get_info   := GetObjectInfo;
    base       := SelfBase;
    is_ordinal := IsNever;
    get_bounds := NoBounds;
  END;

PROCEDURE GetObjectInfo (self: Object;  VAR x: Info) RAISES {Error} =
  VAR traced: BOOLEAN;  sup: Info;
  BEGIN
    IF (self = Root) THEN
      traced := TRUE;
    ELSIF (self = UntracedRoot) THEN
      traced := FALSE;
    ELSE
      self.super.get_info (sup);
      traced := sup.is_traced;
    END;

    x.size      := Target.Address.size;
    x.min_size  := Target.Address.size;
    x.alignment := Target.Address.align;
    x.class     := Class.Object;
    x.is_traced := traced;
    x.is_empty  := FALSE;
    x.is_solid  := TRUE;
  END GetObjectInfo;

(*------------------------------------------------------------- Opaques ---*)

TYPE
  PublicOpaque = T OBJECT
    super : T;
  END;

REVEAL
  Opaque = PublicOpaque BRANDED "M3Type.Opaque" OBJECT OVERRIDES
    get_info   := GetOpaqueInfo;
    base       := SelfBase;
    is_ordinal := IsNever;
    get_bounds := NoBounds;
  END;

PROCEDURE GetOpaqueInfo (self: Opaque;  VAR x: Info) RAISES {Error} =
  VAR sup: Info;
  BEGIN
    self.super.get_info (sup);
    x.size      := Target.Address.size;
    x.min_size  := Target.Address.size;
    x.alignment := Target.Address.align;
    x.class     := Class.Opaque;
    x.is_traced := sup.is_traced;
    x.is_empty  := FALSE;
    x.is_solid  := TRUE;
  END GetOpaqueInfo;

(*---------------------------------------------------------- OPEN ARRAY ---*)

TYPE
  PublicOpenArray = T OBJECT
    element : T;
  END;

REVEAL
  OpenArray = PublicOpenArray BRANDED "M3Type.OpenArray" OBJECT OVERRIDES
    get_info   := GetOpenArrayInfo;
    base       := SelfBase;
    is_ordinal := IsNever;
    get_bounds := NoBounds;
  END;

PROCEDURE GetOpenArrayInfo (self: OpenArray;  VAR x: Info) RAISES {Error} =
  VAR
    elt      : Info;
    elt_pack : INTEGER;
    align    : INTEGER;
    MinAlign := MAX (MAX (Target.Byte, Target.Structure_size_boundary),
                     MAX (Target.Address.align, Target.Integer.align));
  BEGIN
    self.element.get_info (elt);

    align := elt.alignment;
    IF (elt.class = Class.Packed) THEN
      elt_pack := NARROW (self.element, Packed).bits;
    ELSE (* naturally aligned elements must be OK *)
      elt_pack := (elt.size + align - 1) DIV align * align;
    END;

    align := MAX (align, MinAlign); (* == whole array alignment *)
    IF (elt_pack MOD Target.Byte) # 0 THEN
      Restrict ("open array elements must be byte-aligned");
    (*****
    ELSIF NOT Type.IsAlignedOk (p, align) THEN
      Restrict ("scalars in packed array elements cannot cross word boundaries");
    ****)
    END;

    x.size      := -1;
    x.min_size  := -1;
    x.alignment := align;
    x.class     := Class.OpenArray;
    x.is_traced := elt.is_traced;
    x.is_empty  := elt.is_empty;
    x.is_solid  := elt.is_solid AND (elt_pack <= elt.size);
  END GetOpenArrayInfo;

(*--------------------------------------------------- Packed (BITS FOR) ---*)

TYPE
  PublicPacked = T OBJECT
    bits    : INTEGER;
    element : T;
  END;

REVEAL
  Packed = PublicPacked BRANDED "M3Type.Packed" OBJECT OVERRIDES
    get_info   := GetPackedInfo;
    base       := PackedBase;
    is_ordinal := PackedOrd;
    get_bounds := PackedBounds;
  END;

PROCEDURE GetPackedInfo (self: Packed;  VAR x: Info) RAISES {Error} =
  VAR elt: Info;
  BEGIN
    self.element.get_info (elt);
    x.size      := self.bits;
    x.min_size  := self.bits;
    x.alignment := elt.alignment;
    x.class     := Class.Packed;
    x.is_traced := elt.is_traced;
    x.is_empty  := elt.is_empty;
    x.is_solid  := elt.is_solid;
  END GetPackedInfo;

PROCEDURE PackedBase (self: Packed): T =
  BEGIN
    RETURN self.element;
  END PackedBase;

PROCEDURE PackedOrd (self: Packed): BOOLEAN =
  BEGIN
    RETURN IsOrdinal (self.element);
  END PackedOrd;

PROCEDURE PackedBounds (self: Packed;  VAR min, max: Target.Int): BOOLEAN =
  BEGIN
    RETURN GetBounds (self.element, min, max);
  END PackedBounds;

(*----------------------------------------------------------- PROCEDURE ---*)

TYPE
  PublicProcedure = T OBJECT
    formals     : REF ARRAY OF FormalDesc;
    return      : T;
    raises      : REF ARRAY OF ExceptDesc;
    callingConv : Target.CallingConvention;
  END;

REVEAL
  Procedure = PublicProcedure BRANDED "M3Type.Procedure" OBJECT OVERRIDES
    get_info   := GetProcInfo;
    base       := SelfBase;
    is_ordinal := IsNever;
    get_bounds := NoBounds;
  END;

PROCEDURE GetProcInfo (<*UNUSED*> self: Procedure;  VAR x: Info) =
  BEGIN
    x.size      := Target.Address.size;
    x.min_size  := Target.Address.size;
    x.alignment := Target.Address.align;
    x.class     := Class.Procedure;
    x.is_traced := FALSE;
    x.is_empty  := FALSE;
    x.is_solid  := TRUE;
  END GetProcInfo;

(*-------------------------------------------------------------- RECORD ---*)

TYPE
  PublicRecord = T OBJECT
    fields : REF ARRAY OF FieldDesc;
  END;

REVEAL
  Record = PublicRecord BRANDED "M3Type.Record" OBJECT OVERRIDES
    get_info   := GetRecordInfo;
    base       := SelfBase;
    is_ordinal := IsNever;
    get_bounds := NoBounds;
  END;

PROCEDURE GetRecordInfo (self: Record;  VAR x: Info) RAISES {Error} =
  VAR size, align: INTEGER;  solid: BOOLEAN;
  BEGIN
    FieldSizeAndAlignment (self.fields, size, align, solid);
    x.size      := size;
    x.min_size  := size;
    x.alignment := align;
    x.class     := Class.Record;
    x.is_traced := FALSE;
    x.is_empty  := FALSE;
    x.is_solid  := solid;
  END GetRecordInfo;

PROCEDURE FieldSizeAndAlignment (fields: REF ARRAY OF FieldDesc;
                                 VAR(*OUT*) recSize, recAlign: INTEGER;
                                 VAR(*OUT*) is_solid: BOOLEAN) RAISES {Error} =
  VAR
    fieldAlign : INTEGER;
    fieldSize  : INTEGER;
    anyPacked  := FALSE;
    info       : Info;
    newSize    : INTEGER;
    newAlign   : INTEGER;
    curSize    : INTEGER;
  BEGIN
    (* compute the size of the record *)
    newSize  := 0; (* total size of the record *)
    newAlign := Target.Structure_size_boundary; (* minimum allowed alignment *)
    is_solid := TRUE;

    FOR i := 0 TO LAST (fields^) DO
      WITH f = fields[i] DO
        f.type.get_info (info);
        is_solid := is_solid AND info.is_solid;
        IF (info.class = Class.Packed) THEN
          fieldSize  := NARROW (f.type, Packed).bits;
          anyPacked  := TRUE;
        ELSE
          fieldSize  := info.size;
          fieldAlign := info.alignment;
          newAlign   := MAX (newAlign, fieldAlign);
          curSize    := newSize;
          newSize    := RoundUp (curSize, fieldAlign);
          is_solid   := is_solid AND (curSize = newSize);
        END;
        INC (newSize, fieldSize);
      END;
    END;

    IF (anyPacked) THEN
      (**************************************************
      (* add a little bit of C compatibility *)
      IF (Target.PCC_bitfield_type_matters) THEN
        newAlign := MAX (newAlign, Target.Integer.align);
      END;
      ***************************************************)
      (*********
      (* find an alignment that avoids scalar word crossings *)
      IF NOT FindAlignment (newAlign, fields) THEN
        Restrict ("scalars in packed fields cannot cross word boundaries");
      END;
      ***********)
    END;

    curSize := newSize;
    newSize := RoundUp (curSize, newAlign);
    is_solid := is_solid AND (curSize = newSize);
    (* make sure that all copy operations are an integral number of
       aligned transfers. *)

    IF (newSize > 0) THEN
      (* find the largest possible alignment that doesn't change the size
         of the record... *)
      VAR z: CARDINAL; BEGIN
        z := Target.Integer.align;
        IF (z > newAlign) AND (newSize MOD z = 0) THEN  newAlign := z;  END;
        z := Target.Int32.align;
        IF (z > newAlign) AND (newSize MOD z = 0) THEN  newAlign := z;  END;
        z := Target.Int16.align;
        IF (z > newAlign) AND (newSize MOD z = 0) THEN  newAlign := z;  END;
        z := Target.Int8.align;
        IF (z > newAlign) AND (newSize MOD z = 0) THEN  newAlign := z;  END;
      END;
    END;

    (************************
    (* find an alignment (and hence a size) that's some reasonable
       number of machine addressable units *)
    IF newSize <= Target.Int8.size THEN
      newAlign := MAX (newAlign, Target.Int8.align);
    ELSIF newSize <= Target.Int16.size THEN
      newAlign := MAX (newAlign, Target.Int16.align);
    ELSIF newSize <= Target.Int32.size THEN
      newAlign := MAX (newAlign, Target.Int32.align);
    ELSE
      newAlign := MAX (newAlign, Target.Integer.align);
    END;
    **************************)

    recSize  := newSize;
    recAlign := newAlign;
  END FieldSizeAndAlignment;

PROCEDURE RoundUp (size, alignment: INTEGER): INTEGER =
  BEGIN
    IF (alignment = 0)
      THEN RETURN size;
      ELSE RETURN ((size + alignment - 1) DIV alignment) * alignment;
    END;
  END RoundUp;

(*----------------------------------------------------------------- REF ---*)

TYPE
  PublicRef = T OBJECT
    brand  : TEXT;
    target : T;
    traced : BOOLEAN;
  END;

REVEAL
  Ref = PublicRef BRANDED "M3Type.Ref" OBJECT OVERRIDES
    get_info   := GetRefInfo;
    base       := SelfBase;
    is_ordinal := IsNever;
    get_bounds := NoBounds;
  END;

PROCEDURE GetRefInfo (self: Ref;  VAR x: Info) =
  BEGIN
    x.size      := Target.Address.size;
    x.min_size  := Target.Address.size;
    x.alignment := Target.Address.align;
    x.class     := Class.Ref;
    x.is_traced := self.traced;
    x.is_empty  := FALSE;
    x.is_solid  := TRUE;
  END GetRefInfo;

(*----------------------------------------------------------------- SET ---*)

TYPE
  PublicSet = T OBJECT
    domain : T;
  END;

REVEAL
  Set = PublicSet BRANDED "M3Type.Set" OBJECT OVERRIDES
    get_info   := GetSetInfo;
    base       := SelfBase;
    is_ordinal := IsNever;
    get_bounds := NoBounds;
  END;

PROCEDURE GetSetInfo (self: Set;  VAR x: Info) RAISES {Error} =
  VAR n: INTEGER;
  BEGIN
    IF NOT IsOrdinal (self.domain) THEN
      Err ("set domain type is not an ordinal type");
    END;
    IF NOT TInt.ToInt (Number (self.domain), n) THEN
      Err ("set type too large");
    END;
    x.size      := RoundUp (n, Target.Integer.size);
    x.min_size  := x.size;
    x.alignment := MAX (Target.Integer.align, Target.Structure_size_boundary);
    x.class     := Class.Set;
    x.is_traced := FALSE;
    x.is_empty  := FALSE;
    x.is_solid  := TRUE;
  END GetSetInfo;

(*------------------------------------------------------------ Subrange ---*)

TYPE
  PublicSubrange = T OBJECT
    min   : Target.Int;
    max   : Target.Int;
    super : T;
  END;

REVEAL
  Subrange = PublicSubrange BRANDED "M3Type.Subrange" OBJECT OVERRIDES
    get_info   := GetSubrangeInfo;
    base       := SubrangeBase;
    is_ordinal := IsAlways;
    get_bounds := SubrangeBounds;
  END;

PROCEDURE GetSubrangeInfo (self: Subrange;  VAR x: Info) =
  VAR rep := FindRangeRep (self.min, self.max);
  BEGIN
    x.size      := TargetMap.CG_Size[rep];
    x.min_size  := MinIntegerSize (self.min, self.max);
    x.alignment := TargetMap.CG_Align[rep];
    x.class     := Class.Subrange;
    x.is_traced := FALSE;
    x.is_empty  := TInt.LT (self.max, self.min);
    x.is_solid  := TRUE;
  END GetSubrangeInfo;

PROCEDURE FindRangeRep (READONLY min, max: Target.Int): Target.CGType =
  BEGIN
    IF TInt.LE (TInt.Zero, min) THEN
      (* look for an unsigned type *)
      FOR i := FIRST (TargetMap.Word_types) TO LAST (TargetMap.Word_types) DO
        WITH z = TargetMap.Word_types[i] DO
          IF (z.size <= Target.Word.size)
            AND TWord.LE (max, z.max) THEN
            RETURN z.cg_type;
          END;
        END;
      END;
    ELSE
      (* look for a signed type *)
      FOR i := FIRST (TargetMap.Integer_types) TO LAST (TargetMap.Integer_types) DO
        WITH z = TargetMap.Integer_types[i] DO
          IF (z.size <= Target.Integer.size)
            AND TInt.LE (z.min, min)
            AND TInt.LE (max, z.max) THEN
            RETURN z.cg_type;
          END;
        END;
      END;
    END;

    RETURN Target.Integer.cg_type;
  END FindRangeRep;

PROCEDURE SubrangeBase (self: Subrange): T =
  BEGIN
    RETURN self.super;
  END SubrangeBase;

PROCEDURE SubrangeBounds (self: Subrange;  VAR min, max: Target.Int): BOOLEAN =
  BEGIN
    min := self.min;
    max := self.max;
    RETURN TRUE;
  END SubrangeBounds;

(*------------------------------------------------------------- INTEGER ---*)

TYPE
  IntType = T BRANDED "M3Type.IntType" OBJECT OVERRIDES
    get_info   := GetIntInfo;
    base       := SelfBase;
    is_ordinal := IsAlways;
    get_bounds := IntBounds;
    (******************
    is_equal   := SelfEQ;
    is_subtype := SelfEQ;
    ********************)
  END;

PROCEDURE GetIntInfo (<*UNUSED*> self: IntType;  VAR x: Info) =
  BEGIN
    x.size      := Target.Integer.size;
    x.min_size  := Target.Integer.size;
    x.alignment := Target.Integer.align;
    x.class     := Class.Integer;
    x.is_traced := FALSE;
    x.is_empty  := FALSE;
    x.is_solid  := TRUE;
  END GetIntInfo;

PROCEDURE IntBounds (<*UNUSED*> t: T;  VAR min, max: Target.Int): BOOLEAN =
  BEGIN
    min := Target.Integer.min;
    max := Target.Integer.max;
    RETURN TRUE;
  END IntBounds;

(*-------------------------------------------------------------- FLOATS ---*)

TYPE
  FloatType = T BRANDED "M3Type.FloatType" OBJECT
    prec: Target.Precision;
  OVERRIDES
    get_info   := GetFloatInfo;
    base       := SelfBase;
    is_ordinal := IsNever;
    get_bounds := NoBounds;
    (********************
    is_equal   := SelfEQ;
    is_subtype := SelfEQ;
    *********************)
  END;

PROCEDURE GetFloatInfo (self: FloatType;  VAR x: Info) =
  TYPE TC = Class;
  CONST Map = ARRAY Target.Precision OF TC {TC.Real, TC.Longreal, TC.Extended};
  BEGIN
    WITH z = TargetMap.Float_types [self.prec] DO
      x.size      := z.size;
      x.min_size  := z.size;
      x.alignment := z.align;
      x.class     := Map [self.prec];
      x.is_traced := FALSE;
      x.is_empty  := FALSE;
      x.is_solid  := TRUE;
    END;
  END GetFloatInfo;

(*------------------------------------------------- shared utility procs ---*)

PROCEDURE MinIntegerSize (READONLY min, max: Target.Int): INTEGER =
  VAR z1, z2: INTEGER;  n1, n2: BOOLEAN;
  BEGIN
    (* compute the minimum size of these elements *)
    IF TInt.LT (max, min) THEN RETURN 0 END;
    BitWidth (min, z1, n1);
    BitWidth (max, z2, n2);
    z1 := MAX (z1, z2);
    IF (n1 OR n2) THEN INC (z1); END;
    RETURN MIN (z1, Target.Integer.size);
  END MinIntegerSize;

PROCEDURE BitWidth (n: Target.Int;  VAR width: INTEGER;  VAR neg: BOOLEAN) =
  (***  valid for  Target.Integer.min <= n <= Target.Integer.max ***)
  VAR tmp: Target.Int;
  BEGIN
    neg := TInt.LT (n, TInt.Zero);
    IF (neg) THEN
      IF NOT TInt.Add (n, TInt.One, tmp)
        OR NOT TInt.Subtract (TInt.Zero, tmp, n) THEN
        (* value too large??? *)
        width := Target.Integer.size;
        RETURN;
      END;
    END;

    IF NOT powers_done THEN BuildPowerTables () END;
    width := Target.Integer.size;
    FOR i := 0 TO LAST (power) DO
      IF TInt.LE (n, power[i]) THEN width := i;  EXIT END;
    END;
  END BitWidth;

VAR (*CONST*)
  power : ARRAY [0..BITSIZE (Target.Int)] OF Target.Int;
  powers_done := FALSE;

PROCEDURE BuildPowerTables () =
  BEGIN
    power [0] := TInt.One;
    FOR i := 1 TO LAST (power) DO
      IF NOT TInt.Add (power[i-1], power[i-1], power[i]) THEN
        power[i] := Target.Integer.max;
      END;
    END;
    powers_done := TRUE;
  END BuildPowerTables;

(*--------------------------------------------- internal shared methods ---*)

PROCEDURE SelfBase (t: T): T =
  BEGIN
    RETURN t;
  END SelfBase;

PROCEDURE IsNever (<*UNUSED*> t: T): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END IsNever;

PROCEDURE IsAlways (<*UNUSED*> t: T): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END IsAlways;

PROCEDURE NoBounds (<*UNUSED*> t: T;  VAR min, max: Target.Int): BOOLEAN =
  BEGIN
    min := TInt.Zero;
    max := TInt.MOne;
    RETURN FALSE;
  END NoBounds;

(*************************
PROCEDURE SelfEQ (a, b: T): BOOLEAN =
  BEGIN
    RETURN (a = b);
  END SelfEQ;
************************)

PROCEDURE Restrict (msg: TEXT) RAISES {Error} =
  BEGIN
    Err ("implementation restriction: " & msg);
  END Restrict;

PROCEDURE Err (msg: TEXT) RAISES {Error} =
  BEGIN
    RAISE Error (msg);
  END Err;

(*------------------------------------------------------ initialization ---*)

PROCEDURE InitBuiltins () =
  BEGIN
    Integer  := NEW (IntType);
    Real     := NEW (FloatType, prec := Target.Precision.Short);
    LongReal := NEW (FloatType, prec := Target.Precision.Long);
    Extended := NEW (FloatType, prec := Target.Precision.Extended);

    Root := NEW (Object, brand := NIL, super := NIL, fields := NIL,
                     methods := NIL, overrides := NIL);

    UntracedRoot := NEW (Object, brand := NIL, super := NIL, fields := NIL,
                             methods := NIL, overrides := NIL);

    Refany  := NEW (Ref, brand := NIL, target := NIL, traced := TRUE);
    Address := NEW (Ref, brand := NIL, target := NIL, traced := FALSE);
    Null    := NEW (Ref, brand := NIL, target := NIL, traced := FALSE);

    Cardinal := NEW (Subrange,
                     min := TInt.Zero,
                     max := Target.Integer.max,
                     super := Integer);

    VAR elts := NEW (REF ARRAY OF M3ID.T, 2); BEGIN
      elts[0] := M3ID.Add ("FALSE");
      elts[1] := M3ID.Add ("TRUE");
      Boolean := NEW (Enum, elements := elts);
    END;

    Char  := NEW (Enum, elements := NEW (REF ARRAY OF M3ID.T, 256));
    Mutex := NEW (Opaque, super := Root);
    Txt   := NEW (Opaque, super := Refany);

  END InitBuiltins;

BEGIN
  InitBuiltins ();
END M3Type.
