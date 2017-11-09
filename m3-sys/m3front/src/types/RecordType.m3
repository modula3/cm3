(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RecordType.m3                                         *)
(* Last modified on Tue Jun 20 09:57:05 PDT 1995 by kalsow     *)
(*      modified on Thu Jun 15 12:45:03 PDT 1995 by ericv      *)
(*      modified on Tue Mar 26 02:49:22 1991 by muller         *)

MODULE RecordType;

IMPORT M3, M3ID, CG, Type, TypeRep, Scope, Expr, Value, Token;
IMPORT Error, Field, Ident, PackedType, Target, TipeDesc;
IMPORT Word, AssignStmt, M3Buf;
FROM Scanner IMPORT Match, GetToken, cur;

TYPE
  P = Type.T OBJECT
        fields     : Scope.T;
        recSize    : INTEGER := 0;
        align      : INTEGER := 0;
      OVERRIDES
        check      := Check;
        check_align:= CheckAlign;
        isEqual    := EqualChk;
        isSubtype  := TypeRep.NoSubtypes;
        compile    := Compiler;
        initCost   := InitCoster;
        initValue  := GenInit;
        mapper     := GenMap;
        gen_desc   := GenDesc;
        fprint     := FPrinter;
      END;

PROCEDURE Parse (): Type.T =
  VAR p := Create (Scope.PushNew (FALSE, M3ID.NoID));
  BEGIN
    Match (Token.T.tRECORD);
    ParseFieldList ();
    Match (Token.T.tEND);
    Scope.PopNew ();

    RETURN p;
  END Parse;

PROCEDURE ParseFieldList () =
  TYPE TK = Token.T;
  VAR
    j, n    : INTEGER;
    info    : Field.Info;
    nFields := 0;
  BEGIN
    info.offset := 0;
    WHILE (cur.token = TK.tIDENT) DO
      n := Ident.ParseList ();
      info.type := NIL;
      IF (cur.token = TK.tCOLON) THEN
        GetToken (); (* : *)
        info.type := Type.Parse ();
      END;
      info.dfault := NIL;
      IF (cur.token = TK.tEQUAL) THEN
        Error.Msg ("default value must begin with ':='");
        cur.token := TK.tASSIGN;
      END;
      IF (cur.token = TK.tASSIGN) THEN
        GetToken (); (* := *)
        info.dfault := Expr.Parse ();
      END;
      IF (info.type = NIL) AND (info.dfault = NIL) THEN
        Error.Msg ("fields must include a type or default value");
      END;
      j := Ident.top - n;
      FOR i := 0 TO n - 1 DO
        info.name  := Ident.stack [j + i];
        info.index := nFields;  INC (nFields);
        Scope.Insert (Field.New (info));
      END;
      DEC (Ident.top, n);
      IF (cur.token # TK.tSEMI) THEN EXIT END;
      GetToken (); (* ; *)
    END;
  END ParseFieldList;

PROCEDURE New (fields: Scope.T): Type.T =
  VAR p := Create (fields);
  BEGIN
    RETURN p;
  END New;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.info.class = Type.Class.Named) THEN t := Type.Strip (t) END;
    IF (t.info.class # Type.Class.Record) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

PROCEDURE Split (t: Type.T;  VAR fields: Value.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    fields := Scope.ToList (p.fields);
    RETURN TRUE;
  END Split;

PROCEDURE LookUp (t: Type.T;  field: M3ID.T;  VAR obj: Value.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    obj := Scope.LookUp (p.fields, field, TRUE);
    RETURN (obj # NIL);
  END LookUp;

(***********************************************************************)

PROCEDURE Create (fields: Scope.T): P =
  VAR p := NEW (P);
  BEGIN
    TypeRep.Init (p, Type.Class.Record);
    p.fields := fields;
    RETURN p;
  END Create;

PROCEDURE Check (p: P) =
  VAR
    o        : Value.T;
    field    : Field.Info;
    info     : Type.Info;
    cs       := M3.OuterCheckState;
    hash     : INTEGER;
    is_solid : BOOLEAN;
  BEGIN
    Scope.TypeCheck (p.fields, cs);

    (* assign the final offsets to each field *)
    SizeAndAlignment (p.fields, p.info.lazyAligned, p.recSize, p.align,
                      is_solid);

    (* compute the hash value and per-field predicates *)
    p.info.isTraced := FALSE;
    p.info.isEmpty  := FALSE;
    p.info.isSolid  := is_solid;
    hash := Word.Plus (Word.Times (943, p.recSize), p.align);
    o := Scope.ToList (p.fields);
    WHILE (o # NIL) DO
      Field.Split (o, field);
      EVAL Type.CheckInfo (field.type, info);
      p.info.isTraced := p.info.isTraced OR info.isTraced;
      p.info.isEmpty  := p.info.isEmpty  OR info.isEmpty;
      hash := Word.Plus (Word.Times (hash, 41), M3ID.Hash (field.name));
      hash := Word.Plus (Word.Times (hash, 37), info.size);
      o := o.next;
    END;

    p.info.hash      := hash;
    p.info.size      := p.recSize;
    p.info.min_size  := p.recSize;
    p.info.alignment := p.align;
    p.info.mem_type  := CG.Type.Struct;
    p.info.stk_type  := CG.Type.Addr;
    p.info.class     := Type.Class.Record;
  END Check;

PROCEDURE SizeAndAlignment (fields: Scope.T; lazyAligned: BOOLEAN;
                            VAR(*OUT*) recSize, recAlign: INTEGER;
                            VAR(*OUT*) is_solid: BOOLEAN) =
  VAR
    field      : Field.Info;
    base       : Type.T;
    type       : Type.T;
    fieldAlign : INTEGER;
    fieldSize  : INTEGER;
    anyPacked  := FALSE;
    o          : Value.T;
    info       : Type.Info;
    newSize    : INTEGER;
    newAlign   : INTEGER;
    curSize    : INTEGER;
  BEGIN
    (* compute the size of the record *)
    newSize  := 0; (* total size of the record *)
    newAlign := Target.Structure_size_boundary; (* minimum allowed alignment *)
    is_solid := TRUE;

    (* extract the fields and set their offsets *)
    o := Scope.ToList (fields);
    WHILE (o # NIL) DO
      Field.Split (o, field);
      type := Type.CheckInfo (field.type, info);
      is_solid := is_solid AND info.isSolid;
      IF (info.class = Type.Class.Packed) THEN
        PackedType.Split (type, fieldSize, base);
        anyPacked := TRUE;
      ELSE
        fieldSize  := info.size;
        fieldAlign := info.alignment;
        newAlign   := MAX (newAlign, fieldAlign);
        curSize    := newSize;
        newSize    := RoundUp (curSize, fieldAlign);
        is_solid   := is_solid AND (curSize = newSize);
      END;
      Field.SetOffset (o, newSize);
      INC (newSize, fieldSize);
      o := o.next;
    END;

    IF (anyPacked) THEN
      (**************************************************
      (* add a little bit of C compatibility *)
      IF (Target.PCC_bitfield_type_matters) THEN
        newAlign := MAX (newAlign, Target.Integer.align);
      END;
      ***************************************************)
      IF NOT FindAlignment (newAlign, fields, lazyAligned) THEN
        Error.Msg ("Could not find a legal alignment for the packed type.");
      END;
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
        z := Target.Integer.align;  (* Int64 or Int32 *)
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
      newAlign := MAX (newAlign, Target.Int64.align);
    END;
    **************************)

    IF (newSize < 0) THEN
      Error.Msg ("CM3 restriction: record or object type is too large");
    END;
    recSize  := newSize;
    recAlign := newAlign;
  END SizeAndAlignment;

PROCEDURE FindAlignment (VAR align: INTEGER;  fields: Scope.T;
                         lazyAligned: BOOLEAN): BOOLEAN =
  VAR x: INTEGER;
  BEGIN
    FOR a := FIRST (Target.Alignments) TO LAST (Target.Alignments) DO
      x := Target.Alignments[a];
      IF (x >= align) THEN
        (* see if all the fields are ok at this alignment *)
        IF AlignmentOK (x, fields, lazyAligned) THEN
          align := x;
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END FindAlignment;

PROCEDURE AlignmentOK (align: INTEGER;  fields: Scope.T;
                       lazyAligned: BOOLEAN): BOOLEAN =
  VAR o: Value.T;  field: Field.Info;  rec_offs := 0;
      origLazyAligned: BOOLEAN;
  BEGIN
    REPEAT
      o := Scope.ToList (fields);
      WHILE (o # NIL) DO
        Field.Split (o, field);
        origLazyAligned := Type.IsLazyAligned (field.type);
        Type.SetLazyAlignment (field.type, lazyAligned);
        IF NOT Type.IsAlignedOk (field.type, rec_offs + field.offset) THEN
          Type.SetLazyAlignment (field.type, origLazyAligned);
          RETURN FALSE;
        END;
        Type.SetLazyAlignment (field.type, origLazyAligned);
        o := o.next;
      END;
      rec_offs := (rec_offs + align) MOD Target.Integer.size;
    UNTIL (rec_offs = 0);
    RETURN TRUE;
  END AlignmentOK;

PROCEDURE RoundUp (size, alignment: INTEGER): INTEGER =
  BEGIN
    IF (alignment = 0)
      THEN RETURN size;
      ELSE RETURN ((size + alignment - 1) DIV alignment) * alignment;
    END;
  END RoundUp;

PROCEDURE CheckAlign (p: P;  offset: INTEGER): BOOLEAN =
  BEGIN
    RETURN AlignmentOK (offset, p.fields, p.info.lazyAligned);
  END CheckAlign;

PROCEDURE Compiler (p: P) =
  VAR fields := Scope.ToList (p.fields);  o: Value.T;  n: INTEGER;
  BEGIN
    o := fields;  n := 0;
    WHILE (o # NIL) DO
      Field.Compile (NARROW (o, Field.T));
      o := o.next;
      INC (n);
    END;
    CG.Declare_record (Type.GlobalUID (p), p.recSize, n);
    o := fields;
    WHILE (o # NIL) DO  Field.EmitDeclaration (o);  o := o.next;  END;
  END Compiler;

PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  (* Note: it is important to do the surface syntax checks before
     checking the types of the fields.  Otherwise, we will add "a = t"
     to the current set of assumptions and may decide that
     other types are equal when they are not! *)
  VAR b: P := t;  va, vb: Value.T;
  BEGIN
    (******* too sleazy!  since it depends on type checking ...
    IF (a.align # 0) AND (b.align # 0) THEN
      (* both have already been sized *)
      (* => make some simple sanity checks *)
      IF (a.recSize # b.recSize) THEN RETURN FALSE END;
      IF (a.align # b.align) THEN RETURN FALSE END;
    END;
    ********************************************************)

    va := Scope.ToList (a.fields);
    vb := Scope.ToList (b.fields);

    (* check the field names and offsets *)
    IF NOT Field.IsEqualList (va, vb, x, types := FALSE) THEN RETURN FALSE; END;

    (* check the field types and default values *)
    IF NOT Field.IsEqualList (va, vb, x, types := TRUE) THEN RETURN FALSE; END;

    RETURN TRUE;
  END EqualChk;

PROCEDURE InitCoster (p: P;  zeroed: BOOLEAN): INTEGER =
  VAR cost: INTEGER;  v: Value.T;  field: Field.Info;
  BEGIN
    v := Scope.ToList (p.fields);  cost := 0;
    WHILE (v # NIL) DO
      Field.Split (v, field);
      IF (field.dfault = NIL) THEN
        INC (cost, Type.InitCost (field.type, zeroed ));
      ELSIF (zeroed) AND Expr.IsZeroes (field.dfault) THEN
        (* no charge *)
      ELSE
        INC (cost, MAX (1, Type.InitCost (field.type, FALSE)));
      END;
      IF (cost < 0) THEN RETURN LAST (INTEGER) END;
      v := v.next;
    END;
    RETURN cost;
  END InitCoster;

PROCEDURE GenInit (p: P;  zeroed: BOOLEAN) =
  VAR
    field  : Field.Info;
    v      := Scope.ToList (p.fields);
    ptr    := CG.Pop (); (* capture the record's address *)
  BEGIN
    WHILE (v # NIL) DO
      Field.Split (v, field);
      IF (field.dfault = NIL) THEN
        IF (Type.InitCost (field.type, zeroed) > 0) THEN
          CG.Push (ptr);
          CG.Boost_alignment (p.align);
          CG.Add_offset (field.offset);
          Type.InitValue (field.type, zeroed);
        END;
      ELSIF (NOT zeroed) OR (NOT Expr.IsZeroes (field.dfault)) THEN
        AssignStmt.PrepForEmit (field.type, field.dfault,
                                initializing := TRUE);
        CG.Push (ptr);
        CG.Boost_alignment (p.align);
        CG.Add_offset (field.offset);
        AssignStmt.DoEmit (field.type, field.dfault);
      END;
      v := v.next;
    END;
    CG.Free (ptr);
  END GenInit;

PROCEDURE GenMap (p: P;  offset: INTEGER;  <*UNUSED*> size: INTEGER;
                  refs_only: BOOLEAN) =
  VAR v := Scope.ToList (p.fields);  field: Field.Info;
  BEGIN
    WHILE (v # NIL) DO
      Field.Split (v, field);
      Type.GenMap (field.type, offset + field.offset, -1, refs_only);
      v := v.next;
    END;
  END GenMap;

PROCEDURE GenDesc (p: P) =
  VAR v := Scope.ToList (p.fields);  field: Field.Info;  n := 0;  vv := v;
  BEGIN
    IF TipeDesc.AddO (TipeDesc.Op.Record, p) THEN
      (* count the fields *)
      WHILE (vv # NIL) DO  INC (n);  vv := vv.next;  END;
      TipeDesc.AddI (n);

      (* and generate them *)
      WHILE (v # NIL) DO
        Field.Split (v, field);
        Type.GenDesc (field.type);
        v := v.next;
      END;
    END;
  END GenDesc;

PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  VAR v: Value.T;  n: INTEGER;
  BEGIN
    M3Buf.PutText (x.buf, "RECORD");

    (* count the fields and build the tag *)
    v := Scope.ToList (p.fields);  n := 0;
    WHILE (v # NIL) DO  INC (n, Value.AddFPTag (v, x));  v := v.next;  END;
    x.n_nodes := n;

    (* add the field edges *)
    IF (n > NUMBER (x.nodes)) THEN
      x.others := NEW (REF ARRAY OF Type.T, n);
    END;
    v := Scope.ToList (p.fields);  n := 0;
    WHILE (v # NIL) DO  n := Value.AddFPEdges (v, x, n);  v := v.next;  END;
  END FPrinter;

BEGIN
END RecordType.
