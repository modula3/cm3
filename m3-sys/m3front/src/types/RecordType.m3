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
IMPORT Module, Word, AssignStmt, M3Buf;
FROM Scanner IMPORT Match, GetToken, cur;

VAR MaxBitSize := LAST (INTEGER);

TYPE
  P = Type.T OBJECT
        fields     : Scope.T;
        recSize    : INTEGER := 0;
        align      : INTEGER := 0;
      OVERRIDES
        check      := Check;
        no_straddle:= NoStraddle;
        isEqual    := EqualChk;
        isSubtype  := TypeRep.NoSubtypes;
        compile    := Compiler;
        initCost   := InitCoster;
        initValue  := GenInit;
        mapper     := GenMap;
        gen_desc   := GenDesc;
        fprint     := FPrinter;
      END;

(* EXPORTED: *)
PROCEDURE Parse (): Type.T =
  VAR p := Create (Scope.PushNew (FALSE, M3ID.NoID));
  BEGIN
    Match (Token.T.tRECORD);
    ParseFieldList ();
    Match (Token.T.tEND);
    Scope.PopNew ();

    RETURN p;
  END Parse;

(* EXPORTED: *)
PROCEDURE ParseFieldList () =
  TYPE TK = Token.T;
  VAR
    j, n    : INTEGER;
    nFields := 0;
    name    : M3ID.T;
    info    : Field.Info;
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
        Error.Msg ("Field default value must begin with ':=' (2.2.4).");
        cur.token := TK.tASSIGN;
      END;
      IF (cur.token = TK.tASSIGN) THEN
        GetToken (); (* := *)
        info.dfault := Expr.Parse ();
      END;
      IF (info.type = NIL) AND (info.dfault = NIL) THEN
        Error.Msg ("Fields must include a type or default value (2.2.4)");
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

(* EXPORTED: *)
PROCEDURE New (fields: Scope.T): Type.T =
  VAR p := Create (fields);
  BEGIN
    RETURN p;
  END New;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    t := Type.Strip (t); (* StripPacked? *)
    IF (t = NIL) OR (t.info.class # Type.Class.Record) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

(* EXPORTED: *)
PROCEDURE Split (t: Type.T;  VAR fields: Value.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    fields := Scope.ToList (p.fields);
    RETURN TRUE;
  END Split;

(* EXPORTED: *)
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
    fieldInfo: Field.Info;
    typeInfo : Type.Info;
    cs       := M3.OuterCheckState;
    min_size : INTEGER;
    hash     : INTEGER;
    is_solid : BOOLEAN;
  BEGIN
    Scope.TypeCheck (p.fields, cs);

    (* assign the final offsets to each field *)
    SizeAndAlignment (p.fields, p.info.lazyAligned,
                      (*OUT*) min_size, (*OUT*) p.recSize, (*OUT*) p.align,
                      (*OUT*) is_solid
                     );

    (* compute the hash value and per-field predicates *)
    p.info.isTraced := FALSE;
    p.info.isEmpty  := FALSE;
    p.info.isSolid  := is_solid;
    hash := Word.Plus (Word.Times (943, p.recSize), p.align);
    o := Scope.ToList (p.fields);
    WHILE (o # NIL) DO
      Field.Split (o, fieldInfo);
      EVAL Type.CheckInfo (fieldInfo.type, typeInfo);
      p.info.isTraced := p.info.isTraced OR typeInfo.isTraced;
      p.info.isEmpty  := p.info.isEmpty  OR typeInfo.isEmpty;
      hash := Word.Plus (Word.Times (hash, 41), M3ID.Hash (fieldInfo.name));
      hash := Word.Plus (Word.Times (hash, 37), typeInfo.size);
      Field.NameAnonConstr ((*IN OUT*) o, cs);
      o := o.next;
    END;

    p.info.hash      := hash;
    p.info.size      := p.recSize;
    p.info.min_size  := min_size;
    p.info.alignment := p.align;
    p.info.mem_type  := CG.Type.Struct;
    p.info.stk_type  := CG.Type.Struct;
    p.info.class     := Type.Class.Record;
  END Check;

(* EXPORTED: *)
PROCEDURE SizeAndAlignment (fields: Scope.T; lazyAligned: BOOLEAN; 
                            VAR(*OUT*) minSize, recSize, recAlign: INTEGER;
                            VAR(*OUT*) is_solid: BOOLEAN) =
  (* Lay out the fields, assuming the record starts at the largest
     possible alignment for the target. *)
  VAR
    curSize             : INTEGER;
    fieldType           : Type.T;
    packedFieldBaseType : Type.T;
    fieldAlign          : INTEGER;
    fieldSize           : INTEGER;
    fieldValue          : Value.T;
    fieldInfo           : Field.Info;
    fieldTypeInfo       : Type.Info;
    anyPacked           := FALSE;
    raiseRecAlign       := FALSE;
    (* ^Let's disable this for now.  It will cause extra padding ahead of
       this record.  Its only benefit is that it may allow whole-record
       copying to be done with, e.g., a word-by-word copy instead of
       memcopy.  
    *)
    straddlesWord : BOOLEAN := FALSE;
  BEGIN
    recSize  := 0; (* total size of the record *)
    recAlign := Target.Structure_size_boundary; (* minimum record alignment *)
    is_solid := TRUE;

    (* extract the fields and set their offsets *)
    fieldValue := Scope.ToList (fields);
    WHILE (fieldValue # NIL) DO
      Field.Split (fieldValue, fieldInfo);
      fieldType := Type.CheckInfo (fieldInfo.type, fieldTypeInfo);
      is_solid := is_solid AND fieldTypeInfo.isSolid;
      IF (fieldTypeInfo.class = Type.Class.Packed) THEN
        PackedType.Split (fieldType, fieldSize, packedFieldBaseType);
        IF NOT Type.StraddleFreeScalars
                 (fieldType, recSize, IsEltOrField := TRUE)
        THEN
          Error.ID
            (fieldInfo.name,
             "CM3 restriction: scalars in packed fields cannot cross "
             & "boundaries (2.2.5).");
          recSize := RoundUp (recSize, Target.Word.size);
          straddlesWord := TRUE; 
        END;
        anyPacked := TRUE;
      ELSE (* Field not packed. *) 
        fieldSize  := fieldTypeInfo.size;
        fieldAlign := fieldTypeInfo.alignment;
        recAlign   := MAX (recAlign, fieldAlign);
        curSize    := recSize;
        recSize    := RoundUp (curSize, fieldAlign);
        is_solid   := is_solid AND (curSize = recSize);
      END;
      Field.SetOffset (fieldValue, recSize);
      INC (recSize, fieldSize);
(* TODO: Fix so recSize won't overflow. *) 
      fieldValue := fieldValue.next;
    END;

    IF anyPacked AND NOT straddlesWord THEN
      (* Look for a smaller alignment that also avoids word straddling. *) 
      RecAlignWithPackedElements ((*IN OUT*) recAlign, fields, lazyAligned)
    END;

    minSize := recSize;
    recSize := RoundUp (minSize, recAlign);
    (* ^make sure that all copy operations are an integral number of
       aligned transfers. *)
    is_solid := is_solid AND (minSize = recSize);

    IF raiseRecAlign AND recSize > 0 THEN
      (* find the largest possible alignment that doesn't change the size
         of the record... *)
      VAR z: CARDINAL; BEGIN
        z := Target.Integer.align;  (* Int64 or Int32 *)
        IF (z > recAlign) AND (recSize MOD z = 0) THEN  recAlign := z;  END;
        z := Target.Int32.align;
        IF (z > recAlign) AND (recSize MOD z = 0) THEN  recAlign := z;  END;
        z := Target.Int16.align;
        IF (z > recAlign) AND (recSize MOD z = 0) THEN  recAlign := z;  END;
        z := Target.Int8.align;
        IF (z > recAlign) AND (recSize MOD z = 0) THEN  recAlign := z;  END;
      END;
    END;

    IF recSize > MaxBitSize THEN
      Error.Msg ("CM3 restriction: record or object type is too large");
    END;
  END SizeAndAlignment;

PROCEDURE RecAlignWithPackedElements
  (VAR (*IN OUT*) recAlign: INTEGER; fields: Scope.T; lazyAligned: BOOLEAN) =
  (* Assume no straddles if aligned initial value of recAlign.
     Look for the smallest alignment that also is straddle-free. *) 
  VAR trialAlign, offset: INTEGER;
  BEGIN
    FOR a := FIRST (Target.Alignments) (* Smallest *)
             TO LAST (Target.Alignments) DO
      trialAlign := Target.Alignments[a];
      IF trialAlign >= Target.Word.align THEN (* Already know this one works. *)
        recAlign := Target.Word.align; 
        RETURN
      END; 
      IF trialAlign >= recAlign THEN
        offset := Target.Word.size;
        LOOP (* Backwards through multiples of trialAlign within a word. *)
          DEC (offset, trialAlign);
          IF NOT OffsetIsStraddleFree (offset, fields, lazyAligned)
          THEN (* trialAlign won't work. *) 
            EXIT
          ELSIF offset <= 0 THEN (* trialAlign works. *) 
            recAlign := MAX (recAlign, trialAlign);
            RETURN
          END
        END (* LOOP *)
      END 
    END; (* FOR *)
  END RecAlignWithPackedElements;

PROCEDURE OffsetIsStraddleFree
  (recOffset: INTEGER; fields: Scope.T; lazyAligned: BOOLEAN)
: BOOLEAN =
  VAR fieldValue : Value.T;
      fieldInfo  : Field.Info;
      origLazyAligned: BOOLEAN;
  BEGIN
    fieldValue := Scope.ToList (fields);
    WHILE (fieldValue # NIL) DO

      Field.Split (fieldValue, fieldInfo);
      origLazyAligned := Type.IsLazyAligned (fieldInfo.type);
      Type.SetLazyAlignment (fieldInfo.type, lazyAligned);
      IF NOT Type.StraddleFreeScalars
               (fieldInfo.type, recOffset + fieldInfo.offset, IsEltOrField := TRUE)
      THEN
        Type.SetLazyAlignment (fieldInfo.type, origLazyAligned);
        RETURN FALSE;
      END;
      Type.SetLazyAlignment (fieldInfo.type, origLazyAligned);
      fieldValue := fieldValue.next; 
    END (* WHILE *); 
    RETURN TRUE;     
  END OffsetIsStraddleFree;

PROCEDURE NoStraddle (p: P;  offset: INTEGER; <*UNUSED*> IsEltOrField: BOOLEAN)
: BOOLEAN =
  BEGIN
    RETURN OffsetIsStraddleFree ( offset, p.fields, p.info.lazyAligned); 
  END NoStraddle;

(* EXPORTED: *)
PROCEDURE RoundUp (size, alignment: INTEGER): INTEGER =
  BEGIN
    IF (alignment = 0)
      THEN RETURN size;
      ELSE RETURN ((size + alignment - 1) DIV alignment) * alignment;
    END;
  END RoundUp;

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
          CG.Boost_addr_alignment (p.align);
          CG.Add_offset (field.offset);
          Type.InitValue (field.type, zeroed);
        END;
      ELSIF (NOT zeroed) OR (NOT Expr.IsZeroes (field.dfault)) THEN
        AssignStmt.PrepForEmit
          (field.type, field.dfault, initializing := TRUE);
        CG.Push (ptr);
        CG.Boost_addr_alignment (p.align);
        CG.Add_offset (field.offset);
        AssignStmt.DoEmit (field.type, field.dfault, initializing := TRUE);
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
