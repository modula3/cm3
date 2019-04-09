(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ObjectType.m3                                         *)
(* Last modified on Tue Jun 20 09:50:29 PDT 1995 by kalsow     *)
(*      modified on Thu Jun 15 12:45:03 PDT 1995 by ericv      *)
(*      modified on Thu Dec  5 17:22:00 PST 1991 by muller     *)

MODULE ObjectType;

IMPORT M3, M3ID, CG, Type, TypeRep, Scope, Expr, Host, TInt, UserProc;
IMPORT Value, Error, RecordType, ProcType, OpaqueType, Revelation;
IMPORT Field, Reff, Addr, Word, M3Buf, ErrType, Procedure, AddressExpr;
IMPORT ObjectAdr, ObjectRef, Token, Module, Method, Brand;
IMPORT AssignStmt, M3RT, Scanner, TipeMap, TipeDesc, TypeFP, Target;
FROM Scanner IMPORT Match, GetToken, cur;

CONST
  Unknown_w_magic = -1;
  Unknown_wo_magic = -2;
  Unchecked_offset = -3;

TYPE
  P = Type.T BRANDED "ObjectType.T" OBJECT
        brand        : Brand.T;
        superType    : Type.T;
        fields       : Scope.T;
        fieldOffset  : INTEGER;
        fieldSize    : INTEGER;
        fieldAlign   : INTEGER;
        methods      : Scope.T;
        methodSize   : INTEGER;
        methodOffset : INTEGER;
        overrideSize : INTEGER;
        tc_module    : Module.T;
        inPrimLookUp : BOOLEAN;
        isTraced     : BOOLEAN;
        user_name    : TEXT;
      OVERRIDES
        check      := Check;
        no_straddle:= TypeRep.AddrNoStraddle;
        isEqual    := EqualChk;
        isSubtype  := Subtyper;
        compile    := Compiler;
        initCost   := InitCoster;
        initValue  := TypeRep.InitToZeros;
        mapper     := TypeRep.GenRefMap;
        gen_desc   := TypeRep.GenRefDesc;
        fprint     := FPrinter;
      END;

VAR
  NIL_ID: INTEGER := 0;
  ROOT_ID: INTEGER := 0;
  UROOT_ID: INTEGER := 0;

PROCEDURE Parse (sup: Type.T;  traced: BOOLEAN;  brand: Brand.T): Type.T =
  TYPE TK = Token.T;
  VAR p: P;
  BEGIN
    LOOP
      p := New (sup, traced, brand, NIL, NIL);
      Match (TK.tOBJECT);

      p.fields := Scope.PushNew (FALSE, M3ID.NoID);
      RecordType.ParseFieldList ();
      Scope.PopNew ();

      p.methods := Scope.PushNew (FALSE, M3ID.NoID);
      IF (cur.token = TK.tMETHODS) THEN
        GetToken (); (* METHODS *)
        p.methodSize := ParseMethodList (p, overrides := FALSE);
      END;
      IF (cur.token = TK.tOVERRIDES) THEN
        GetToken (); (* OVERRIDES *)
        p.overrideSize := ParseMethodList (p, overrides := TRUE);
      END;
      Scope.PopNew ();

      Match (TK.tEND);
      brand := Brand.Parse ();
      IF (cur.token # TK.tOBJECT) THEN
        IF (brand # NIL) THEN Error.Msg ("dangling brand") END;
        EXIT;
      END;
      sup := p;
      traced := FALSE;
    END;
    RETURN p;
  END Parse;

PROCEDURE ParseMethodList (p: P;  overrides := FALSE): INTEGER =
  TYPE TK = Token.T;
  VAR info: Method.Info;
  BEGIN
    info.offset   := 0;
    info.parent   := p;
    info.override := overrides;
    WHILE (cur.token = TK.tIDENT) DO
      info.name := cur.id;
      GetToken (); (* ID *)

      info.signature := NIL;
      IF (cur.token = TK.tLPAREN) THEN
        info.signature := ProcType.ParseSignature (M3ID.NoID,
                                                   Target.DefaultCall);
      END;

      info.dfault := NIL;
      IF (cur.token = TK.tEQUAL) THEN
        Error.Msg ("default value must begin with ':='");
        cur.token := TK.tASSIGN;
      END;
      IF cur.token = TK.tASSIGN THEN
        GetToken (); (* := *)
        info.dfault := Expr.Parse ();
      END;

      IF overrides THEN
        IF info.signature # NIL THEN
          Error.ID (info.name, "overrides cannot have a signature");
        ELSIF info.dfault = NIL THEN
          Error.ID (info.name, "missing default value in method override");
        END;
      ELSE 
        IF info.signature = NIL THEN
          Error.ID (info.name, "missing method signature");
        END;
      END;
      
      EVAL Method.New (info);
      INC (info.offset, Target.Address.size);

      IF (cur.token # TK.tSEMI) THEN EXIT END;
      GetToken (); (* ; *)
    END;

    RETURN info.offset;
  END ParseMethodList;

PROCEDURE New (super: Type.T;  traced: BOOLEAN;  brand: Brand.T;
                                            fields, methods: Scope.T): Type.T =
  VAR p: P;
  BEGIN
    IF (super = NIL) THEN
      IF (traced)
        THEN super := ObjectRef.T;
        ELSE super := ObjectAdr.T;
      END;
    END;
    p := NEW (P);
    TypeRep.Init (p, Type.Class.Object);
    p.isTraced     := traced;
    p.brand        := brand;
    p.superType    := super;
    p.fields       := fields;
    p.fieldOffset  := Unchecked_offset;
    p.fieldSize    := -1;
    p.fieldAlign   := -1;
    p.methods      := methods;
    p.methodSize   := 0;
    p.methodOffset := Unchecked_offset;
    p.overrideSize := 0;
    p.tc_module    := NIL;
    p.inPrimLookUp := FALSE;
    p.user_name    := NIL;
    RETURN p;
  END New;

PROCEDURE Is (t: Type.T): BOOLEAN =
  VAR m: Revelation.TypeList;  u: Type.T;  x: Revelation.TypeSet;
  BEGIN
    IF (t = NIL) THEN RETURN FALSE END;
    t := Type.StripPacked (t); 
    IF (t.info.class = Type.Class.Named) THEN t := Type.Strip (t); END;

    (* try for TYPE t = OBJECT ... END *)
    IF (t.info.class = Type.Class.Object) THEN RETURN TRUE END;

    IF (t.info.class # Type.Class.Opaque) THEN RETURN FALSE END;

    (* try for TYPE t <: ObjectType *)
    u := OpaqueType.Super (t);
    IF Is (u) THEN RETURN TRUE END;

    (***************    
    (* try for REVEAL t = OBJECT ... END *)
    u := Revelation.LookUp (t);
    IF (u # NIL) AND (u.class = Type.Class.Object) THEN RETURN TRUE END;
    ********************)

    Revelation.LookUpAll (t, x);

    (* try for REVEAL t <: OBJECT ... END *)
    FOR i := 0 TO x.cnt-1 DO
      u := Type.Strip (x.types[i]);
      IF (u # NIL) AND (u.info.class = Type.Class.Object) THEN RETURN TRUE END;
    END;
    m := x.others;
    WHILE (m # NIL) DO
      u := Type.Strip (m.type);
      IF (u # NIL) AND (u.info.class = Type.Class.Object) THEN RETURN TRUE END;
      m := m.next;
    END;

    (* try for REVEAL t <: U where U is an object type *)
    FOR i := 0 TO x.cnt-1 DO
      IF Is (x.types[i]) THEN RETURN TRUE END;
    END;
    m := x.others;
    WHILE (m # NIL) DO
      IF Is (m.type) THEN RETURN TRUE END;
      m := m.next;
    END;

    RETURN FALSE;
  END Is;

PROCEDURE IsBranded (t: Type.T): BOOLEAN =
  VAR info: Type.Info;
  BEGIN
    t := Type.StripPacked (t); 
    t := Type.CheckInfo (t, info);
    IF (info.class # Type.Class.Object) THEN RETURN FALSE END;

    (* try for TYPE t = BRANDED OBJECT ... END *)
    IF (info.class = Type.Class.Object) THEN
      RETURN (NARROW (t, P).brand # NIL);
    END;

    IF (info.class # Type.Class.Opaque) THEN RETURN FALSE END;

    (* try for REVEAL t = BRANDED OBJECT ... END *)
    t := Revelation.LookUp (t);
    IF (t = NIL) THEN RETURN FALSE END;
    t := Type.CheckInfo (t, info);
    IF (info.class = Type.Class.Object) THEN
      RETURN (NARROW (t, P).brand # NIL);
    END;

    RETURN FALSE;
  END IsBranded;

PROCEDURE Super (t: Type.T): Type.T =
  VAR info: Type.Info;
  BEGIN
    t := Type.StripPacked (t); 
    t := Type.CheckInfo (t, info);
    IF (info.class # Type.Class.Object) THEN RETURN NIL END;
    RETURN NARROW (t, P).superType;
  END Super;

PROCEDURE LookUp (t: Type.T; id: M3ID.T;
                           VAR value: Value.T;  VAR visible: Type.T): BOOLEAN =
  VAR p: P;  v: Value.T;  z: Type.T;  info: Type.Info;  x: Revelation.TypeSet;
  BEGIN
    LOOP
      t := Type.StripPacked (t); 
      t := Type.CheckInfo (t, info);

      IF (info.class = Type.Class.Error) THEN
        value := NIL;
        visible := ErrType.T;
        RETURN FALSE;

      ELSIF (info.class = Type.Class.Object) THEN
        (* found an object type => try it! *)
        p := t;
        v := Scope.LookUp (p.methods, id, TRUE);
        IF (v # NIL) THEN
          (* find the first non-override declaration for this method *)
          p := PrimaryMethodDeclaration (p, v);
          IF (p = NIL) THEN RETURN FALSE END;
        ELSE
          (* try for a field *)
          v := Scope.LookUp (p.fields, id, TRUE);
        END;
        IF (v # NIL) THEN
          value   := v;
          visible := p;
          RETURN TRUE;
        END;
        t := p.superType;

      ELSIF (info.class = Type.Class.Opaque) THEN
        (* try any revelations that are visible *)
        z := Revelation.LookUp (t);
        IF (z # NIL) THEN
          (* use the concrete type *)
          t := z;
        ELSE
          (* try any subtype revelations that are visible *)
          Revelation.LookUpAll (t, x);
          FOR i := 0 TO x.cnt-1 DO
            IF LookUp(x.types[i], id, value, visible) THEN RETURN TRUE; END;
          END;
          WHILE (x.others # NIL) DO
            IF LookUp(x.others.type, id, value, visible) THEN RETURN TRUE; END;
            x.others := x.others.next;
          END;
          t := OpaqueType.Super (t);
        END;

      ELSE (* ??? *)
        RETURN FALSE;
      END;

    END; (* LOOP *)
  END LookUp;

PROCEDURE PrimaryMethodDeclaration (p: P;  v: Value.T): P =
  VAR method: Method.Info;   visible: Type.T;  obj: Value.T;
  BEGIN
    Method.SplitX (v, method);
    IF NOT method.override THEN RETURN p END;
    IF p.inPrimLookUp THEN
      Error.Msg ("illegal recursive supertype");
    ELSE
      p.inPrimLookUp := TRUE;
      IF LookUp (p.superType, method.name, obj, visible) THEN
        p.inPrimLookUp := FALSE;
        RETURN visible;
      END;
      p.inPrimLookUp := FALSE;
    END;
    RETURN NIL;
  END PrimaryMethodDeclaration;

PROCEDURE Check (p: P) =
  VAR
    super    : Type.T;
    name     : M3ID.T;
    n        : INTEGER;
    o, v     : Value.T;
    t1       : Type.T;
    hash     : INTEGER;
    method   : Method.Info;
    cs := M3.OuterCheckState;
    super_info : Type.Info;
  BEGIN
    hash := 0;

    (* check out my super type *)
    super := p.superType;
    IF (super # NIL) THEN
      (* some super type specified *)
      super := Type.CheckInfo (super, super_info);
      p.superType := super;
      IF Is (super) THEN
        (* super type is an object type *)
        p.isTraced := super_info.isTraced;
        hash := Word.Times (super_info.hash, 37);
        IF (super = p) THEN
          Error.Msg ("illegal recursive supertype");
          super := NIL;
          p.superType := NIL;
        END;
      ELSE
        (* super type isn't an object! *)
        Error.Msg ("super type must be an object type");
        p.superType := NIL;
        p.isTraced  := super_info.isTraced;
      END;
    END;

    Brand.Check (p.brand, p, hash, cs);

    (* include the fields in my hash value *)
    o := Scope.ToList (p.fields);  n := 0;
    WHILE (o # NIL) DO
      name := Value.CName (o);
      hash := Word.Plus (Word.Times (hash, 23), M3ID.Hash (name));
      hash := Word.Plus (Word.Times (hash, 23), n);
      IF (Scope.LookUp (p.methods, name, TRUE) # NIL) THEN
        Error.ID (name, "field and method with the same name");
      END;
      o := o.next;  INC (n);
    END;

    (* include the methods in my hash value *)
    o := Scope.ToList (p.methods);
    WHILE (o # NIL) DO
      name := Value.CName (o);
      hash := Word.Plus (Word.Times (hash, 23), M3ID.Hash (name));
      hash := Word.Plus (Word.Times (hash, 23), 617);
      o := o.next;
    END;

    p.info.size      := Target.Address.size;
    p.info.min_size  := Target.Address.size;
    p.info.alignment := Target.Address.align;
    p.info.addr_align:= Target.Address.align;
    p.info.mem_type  := CG.Type.Addr;
    p.info.stk_type  := CG.Type.Addr;
    p.info.class     := Type.Class.Object;
    p.info.isTraced  := p.isTraced;
    p.info.isEmpty   := FALSE;
    p.info.isSolid   := TRUE;
    p.info.hash      := hash;

    INC (Type.recursionDepth); (*------------------------------------*)
    p.checked := TRUE;

      (* bind method overrides to their original declarations *)
      o := Scope.ToList (p.methods);
      WHILE (o # NIL) DO
        Method.SplitX (o, method);
        IF (method.override) THEN
          IF LookUp (super, method.name, v, t1)
            AND Method.Split (v, method) THEN
            Method.NoteOverride (o, v);
          ELSE
            Scanner.offset := o.origin;
            Error.ID (method.name, "no method to override in supertype");
          END;
        END;
        o := o.next;
      END;

      (* checkout my fields & methods *)
      Scope.TypeCheck (p.fields, cs);
      Scope.TypeCheck (p.methods, cs);

    DEC (Type.recursionDepth); (*------------------------------------*)

    (* compute the size & alignment requirements of my fields *)
    GetSizes (p);

    IF (NOT p.isTraced) AND Module.IsSafe() THEN CheckTracedFields (p) END;
  END Check;

PROCEDURE CheckTracedFields (p: P) =
  VAR v := Scope.ToList (p.fields);  info: Type.Info;
  BEGIN
    WHILE (v # NIL) DO
      EVAL Type.CheckInfo (Value.TypeOf (v), info);
      IF info.isTraced THEN
        Error.ID (Value.CName (v),
                     "unsafe: untraced object contains a traced field");
      END;
      v := v.next;
    END;
  END CheckTracedFields;

PROCEDURE Compiler (p: P) =
  VAR
    fields, methods, v: Value.T;
    nFields, nMethods, nOverrides: INTEGER;
  BEGIN
    Type.Compile (p.superType);

    fields  := Scope.ToList (p.fields);
    methods := Scope.ToList (p.methods);

    (* count the fields & methods *)
    v := fields;  nFields := 0;
    WHILE (v # NIL) DO INC (nFields);  v := v.next;  END;
    nMethods := p.methodSize DIV Target.Address.size;
    nOverrides := p.overrideSize DIV Target.Address.size;

    (* declare my field and method types *)
    GenMethods (methods, FALSE);
    GenOverrides (methods, FALSE);
    GenFields (fields, FALSE);

    (* declare myself, my fields, and my methods *)
    CG.Declare_object (Type.GlobalUID (p), Type.GlobalUID (p.superType),
                       Brand.ToText (p.brand), p.isTraced, nFields,
                       nMethods, nOverrides, p.fieldSize);
    GenMethods (methods, TRUE);
    GenOverrides (methods, TRUE);
    GenFields (fields, TRUE);
    (* YUCK!  m3gdb assumes the methods are declared first. *)
  END Compiler;

PROCEDURE GenFields (fields: Value.T;  declare: BOOLEAN) =
  BEGIN
    WHILE (fields # NIL) DO
      IF (declare)
        THEN Field.EmitDeclaration (fields);
        ELSE Type.Compile (Value.TypeOf (fields));
      END;
      fields := fields.next;
    END;
  END GenFields;

PROCEDURE GenMethods (methods: Value.T;  declare: BOOLEAN) =
  VAR method: Method.Info;
  BEGIN
    WHILE (methods # NIL) DO
      Method.SplitX (methods, method);
      IF (method.override) THEN
        (* skip *)
      ELSIF (declare) THEN
        CG.Declare_method (method.name, Type.GlobalUID (method.signature),
                           method.dfault);
      ELSE
        Type.Compile (method.signature);
      END;
      methods := methods.next;
    END;
  END GenMethods;

PROCEDURE GenOverrides (methods: Value.T;  declare: BOOLEAN) =
  VAR method: Method.Info;
  BEGIN
    WHILE (methods # NIL) DO
      Method.SplitX (methods, method);
      IF (method.override) AND (declare) THEN
        CG.Declare_override (method.name, method.dfault);
      END;
      methods := methods.next;
    END;
  END GenOverrides;

PROCEDURE NoteOffsets (t: Type.T;  pp: Type.T) =
  VAR p := Confirm (pp);
  BEGIN
    IF (p = NIL) THEN (* not an object *) RETURN END;
    GetOffsets (p, use_magic := NOT Module.IsInterface ());
    Host.env.note_opaque_magic (Type.GlobalUID (t),
                                Type.GlobalUID (p.superType),
                                p.fieldSize, p.fieldAlign, p.methodSize);
  END NoteOffsets;

PROCEDURE NoteRefName (t: Type.T;  name: TEXT) =
  VAR p := Confirm (t);
  BEGIN
    IF (p # NIL) THEN p.user_name := name; END;
  END NoteRefName;

PROCEDURE InitTypecell (t: Type.T;  offset, prev: INTEGER) =
  VAR
    p         : P := t;
    fields    := Scope.ToList (p.fields);
    brand     := Brand.Compile (p.brand);
    type_map  := GenTypeMap (p, fields, refs_only := FALSE);
    gc_map    := GenTypeMap (p, fields, refs_only := TRUE);
    type_desc := GenTypeDesc (p, fields);
    defaults  := GenMethodList (p);
    initProc  := GenInitProc (p);
    linkProc  := GenLinkProc (p, defaults);
    super_id  : INTEGER := 0;
    isz       : INTEGER := Target.Integer.size;
    name_offs : INTEGER := 0;
    fp        := TypeFP.FromType (p);
    globals   := Module.GlobalData (is_const := FALSE);
    consts    := Module.GlobalData (is_const := TRUE);
  BEGIN
    IF (p.superType # NIL) THEN super_id := Type.GlobalUID (p.superType) END;

    IF (p.user_name # NIL) THEN
      name_offs := CG.EmitText (p.user_name, is_const := TRUE);
    END;

    (* generate my Type cell info *)
    CG.Init_intt   (offset + M3RT.TC_selfID, isz, Type.GlobalUID (p), FALSE);
    FOR i := FIRST (fp.byte) TO LAST (fp.byte) DO
      CG.Init_intt (offset + M3RT.TC_fp + i * 8, 8, fp.byte[i], FALSE);
    END;
    CG.Init_intt (offset + M3RT.TC_traced, 8, ORD (p.isTraced), FALSE);
    CG.Init_intt (offset + M3RT.TC_kind, 8, ORD (M3RT.TypeKind.Obj), FALSE);
    CG.Init_intt (offset + M3RT.TC_dataAlignment, 8, p.fieldAlign DIV Target.Byte, FALSE);
    CG.Init_intt (offset + M3RT.TC_dataSize, isz, p.fieldSize DIV Target.Byte, FALSE);
    IF (type_map >= 0) THEN
      CG.Init_var (offset + M3RT.TC_type_map, consts, type_map, FALSE);
    END;
    IF (gc_map >= 0) THEN
      CG.Init_var (offset + M3RT.TC_gc_map, consts, gc_map, FALSE);
    END;
    IF (type_desc >= 0) THEN
      CG.Init_var (offset + M3RT.TC_type_desc, consts, type_desc, FALSE);
    END;
    IF (initProc # NIL) THEN
      CG.Init_proc (offset + M3RT.TC_initProc, initProc, FALSE);
    END;
    IF (brand >= 0) THEN
      CG.Init_var (offset + M3RT.TC_brand, consts, brand, FALSE);
    END;
    IF (p.user_name # NIL) THEN
      CG.Init_var (offset + M3RT.TC_name,  consts, name_offs, FALSE);
    END;
    IF (prev # 0) THEN
      CG.Init_var (offset + M3RT.TC_next,  globals, prev, FALSE);
    END;

    (* OBJECT specific extensions to the typecell *)
    CG.Init_intt (offset + M3RT.OTC_parentID, isz, super_id, FALSE);
    IF (linkProc # NIL) THEN
      CG.Init_proc (offset + M3RT.OTC_linkProc, linkProc, FALSE);
    END;
    CG.Init_intt (offset + M3RT.OTC_methodSize, isz,
                      p.methodSize DIV Target.Byte, FALSE);
    IF (defaults >= 0) THEN
      CG.Init_var (offset + M3RT.OTC_defaultMethods, consts, defaults, FALSE);
    END;

    NoteOffsets (p, p);
  END InitTypecell;

PROCEDURE GenTypeMap (p: P;  fields: Value.T;  refs_only: BOOLEAN): INTEGER =
  (* generate my "TypeMap" (called by the garbage collector) *)
  VAR field: Field.Info;
  BEGIN
    TipeMap.Start ();

    WHILE (fields # NIL) DO
      Field.Split (fields, field);
      Type.GenMap (field.type, field.offset, -1, refs_only);
      fields := fields.next;
    END;

    RETURN TipeMap.Finish ("type map for ", Type.Name (p));
  END GenTypeMap;

PROCEDURE GenTypeDesc (p: P;  fields: Value.T): INTEGER =
  (* generate my "TypeDesc" (called by the pickle machinery) *)
  VAR field: Field.Info;  nFields := 0;  v := fields;
  BEGIN
    IF NOT p.isTraced THEN RETURN -1 END;

    TipeDesc.Start ();

    IF TipeDesc.AddO (TipeDesc.Op.Object, p) THEN
      (* count the fields *)
      WHILE (v # NIL) DO INC (nFields);  v := v.next;  END;
      TipeDesc.AddI (nFields);

      WHILE (fields # NIL) DO
        Field.Split (fields, field);
        Type.GenDesc (field.type);
        fields := fields.next;
      END;
    END;

    RETURN TipeDesc.Finish ("type description for ", Type.Name (p));
  END GenTypeDesc;

CONST
  MaxShortMethodList = 32;
TYPE
  MethodValue = RECORD known: BOOLEAN;  proc: CG.Proc;  END;

PROCEDURE GenMethodList (p: P): INTEGER =
  VAR n := MethodOffset (p);
  BEGIN
    IF n < 0 THEN RETURN -1; (* don't know! *) END;

    (* # of methods *)
    n := (n + p.methodSize) DIV Target.Address.size;

    IF (n <= 0) THEN
      RETURN -1;
    ELSIF (n <= MaxShortMethodList) THEN
      RETURN GenShortMethodList (p, n);
    ELSE
      RETURN GenLongMethodList (p, n);
    END;
  END GenMethodList;

PROCEDURE GenShortMethodList (p: P;  n_methods: CARDINAL): INTEGER =
  VAR methods: ARRAY [0..MaxShortMethodList-1] OF MethodValue;
  BEGIN
    RETURN DoMethodList (p, SUBARRAY (methods, 0, n_methods));
  END GenShortMethodList;

PROCEDURE GenLongMethodList (p: P;  n_methods: CARDINAL): INTEGER =
  VAR methods := NEW (REF ARRAY OF MethodValue, n_methods);
  BEGIN
    RETURN DoMethodList (p, methods^);
  END GenLongMethodList;

PROCEDURE DoMethodList (p: P;  VAR m: ARRAY OF MethodValue): INTEGER =
  VAR offset: INTEGER;
  BEGIN
    FOR i := FIRST (m) TO LAST (m) DO
      m[i].known := FALSE;
    END;

    IF NOT FillMethods (p, m) THEN RETURN -1; END;

    FOR i := FIRST (m) TO LAST (m) DO
      IF NOT m[i].known THEN RETURN -1; END;
    END;

    offset := Module.Allocate (NUMBER (m) * Target.Address.size,
                               Target.Address.align, TRUE, "method list");
    FOR i := FIRST (m) TO LAST (m) DO
      VAR p := m[i].proc; BEGIN
        IF p # NIL THEN
          CG.Init_proc (offset + i * Target.Address.size, p, TRUE);
        END;
      END;
    END;
    RETURN offset;
  END DoMethodList;

PROCEDURE FillMethods (t: Type.T;  VAR m: ARRAY OF MethodValue): BOOLEAN =
  VAR
    p         : P;
    v         : Value.T;
    method    : Method.Info;
    b         : BOOLEAN;
    top       : Value.T;
    tVisible  : Type.T;
    m_offset  : INTEGER;
    expr      : Expr.T;
    proc      : Value.T;
    addr      : Target.Int;
  BEGIN
    IF (t = NIL) THEN RETURN TRUE; END;

    p := Confirm (t);
    IF p = NIL THEN RETURN FALSE; END;

    IF NOT FillMethods (p.superType, m) THEN RETURN FALSE; END;
      
    (* try splitting each of my methods/overrides *)
    v := Scope.ToList (p.methods);
    WHILE (v # NIL) DO
      Method.SplitX (v, method);
      b := LookUp (p, method.name, top, tVisible); <* ASSERT b *>
      m_offset := MethodOffset (tVisible);
      IF (m_offset < 0) THEN RETURN FALSE; END;
      m_offset := (m_offset + method.offset) DIV Target.Address.size;
      WITH info = m[m_offset] DO
        expr := Expr.ConstValue (method.dfault);
        IF (expr = NIL) THEN
          (* no method default specified or it is not a constant! *)
          info.known := FALSE;
          info.proc  := NIL;
        ELSIF UserProc.IsProcedureLiteral (expr, proc) THEN
          (* method default is a named procedure *)
          info.known := TRUE;
          info.proc  := Procedure.CGName (proc);
        ELSIF AddressExpr.Split (expr, addr) AND TInt.EQ (addr, TInt.Zero) THEN
          (* method default is NIL *)
          (*************************
            The runtime will initialize this slot to a routine that
            raises an "undefined method" exception.  If we generate
            a constant *read-only* method list, the runtime initialization
            will fail!
          info.known := TRUE;
          info.proc  := NIL;
          *************************)
          info.known := FALSE;
          info.proc  := NIL;
        ELSE
          info.known := FALSE;
          info.proc  := NIL;
        END;
      END;
      v := v.next;
    END;

    RETURN TRUE;
  END FillMethods;

PROCEDURE GenInitProc (p: P): CG.Proc =
  VAR
    v     := Scope.ToList (p.fields);
    field : Field.Info;
    ptr   : CG.Val;
    obj   : CG.Var;
    done  : BOOLEAN := TRUE;
    name  : TEXT    := NIL;
    proc  : CG.Proc := NIL;
    fieldExprAlign : INTEGER;
  BEGIN
    (* check to see if we need any initialization code *)
    WHILE (v # NIL) DO
      Field.Split (v, field);
      IF (field.dfault = NIL) THEN
        IF Type.InitCost (field.type, TRUE) # 0 THEN done := FALSE; EXIT; END;
      ELSIF NOT Expr.IsZeroes (field.dfault) THEN
        done := FALSE; EXIT;
      END;
      v := v.next;
    END;
    IF (done) THEN RETURN NIL; END;

    (* generate the procedure body *)

    name := Type.LinkName (p, "_INIT");
    CG.Comment (-1, FALSE, name & " (ObjectType)");
    Scanner.offset := p.origin;
    CG.Gen_location (p.origin);
    proc := CG.Declare_procedure (M3ID.Add (name), 1, CG.Type.Void,
                                  0, Target.DefaultCall, exported:= FALSE,
                                  parent := NIL);
    obj := CG.Declare_param (M3ID.NoID, Target.Address.size,
                             Target.Address.align, CG.Type.Addr,
                             Type.GlobalUID (p),
                             in_memory := FALSE, up_level := FALSE,
                             f := CG.Always);
    CG.Begin_procedure (proc);

    (* allocate and initialize a pointer to the data fields *)
    CG.Load_addr (obj, 0, p.fieldAlign);
    IF (p.fieldOffset >= 0) THEN
      (* the field offsets are constant *)
      CG.Add_offset (p.fieldOffset);
    ELSE
      (* the field offsets are unknown *)
      Type.LoadInfo (p, M3RT.OTC_dataOffset);
      CG.Index_bytes (Target.Byte);
    END;
    ptr := CG.Pop ();

    (* initialize each of the fields *)
    v := Scope.ToList (p.fields);
    WHILE (v # NIL) DO
      Field.Split (v, field);
      IF (field.dfault = NIL) THEN
        IF Type.InitCost (field.type, TRUE) > 0 THEN
          CG.Push (ptr);
          CG.Boost_addr_alignment (p.fieldAlign);
          CG.Add_offset (field.offset);
          Type.InitValue (field.type, TRUE);
        END;
      ELSIF NOT Expr.IsZeroes (field.dfault) THEN
     (* ArrayExpr.NoteTargetType (field.dfault, field.type); *)
        AssignStmt.PrepForEmit (field.type, field.dfault, initializing := TRUE);
        CG.Push (ptr);
        CG.Boost_addr_alignment (p.fieldAlign);
        CG.Add_offset (field.offset);
        fieldExprAlign := CG.GCD (p.fieldAlign, field.offset MOD Target.Word.size);
        AssignStmt.DoEmit (field.type, field.dfault, fieldExprAlign);
      END;
      v := v.next;
    END;

    CG.Free (ptr);
    CG.Exit_proc (CG.Type.Void);
    CG.End_procedure (proc);
    RETURN proc;
  END GenInitProc;

PROCEDURE GenLinkProc (p: P;  defaults: INTEGER): CG.Proc =
  VAR
    v         := Scope.ToList (p.methods);
    method    : Method.Info;
    top       : Value.T;
    tVisible  : Type.T;
    t_default : Type.T;
    ptr       : CG.Val;
    b         : BOOLEAN;
    m_offset  : INTEGER;
    done      : BOOLEAN := TRUE;
    name      : TEXT    := NIL;
    proc      : CG.Proc := NIL;
    defn      : CG.Var;
  BEGIN
    (* check for a statically initialized method list *)
    IF (defaults >= 0) THEN RETURN NIL; END;
      
    (* check to see if we need any setup code *)
    WHILE (v # NIL) DO
      Method.SplitX (v, method);
      IF (method.dfault # NIL) THEN done := FALSE; EXIT; END;
      v := v.next;
    END;
    IF (done) THEN RETURN NIL; END;

    Type.GenTag (p, "link-time setup code for ", -1);


    (* get a pointer to my default method list *)
    name := Type.LinkName (p, "_LINK");
    CG.Comment (-1, FALSE, name);
    Scanner.offset := p.origin;
    CG.Gen_location (p.origin);
    proc := CG.Declare_procedure (M3ID.Add (name), 1, CG.Type.Void,
                                  0, Target.DefaultCall, exported:= FALSE,
                                  parent := NIL);
    defn := CG.Declare_param (M3ID.NoID, Target.Address.size,
                             Target.Address.align, CG.Type.Addr,
                             Type.GlobalUID (Addr.T),
                             in_memory := FALSE, up_level := FALSE,
                             f := CG.Always);
    CG.Begin_procedure (proc);

    (* grab the default methodlist pointer *)
    CG.Load_addr (defn, 0 , Target.Address.align);
    CG.Load_indirect (CG.Type.Addr, M3RT.OTC_defaultMethods, Target.Address.size);
    ptr := CG.Pop ();

    v := Scope.ToList (p.methods);
    WHILE (v # NIL) DO
      Method.SplitX (v, method);
      IF (method.dfault # NIL) THEN
        t_default := Expr.TypeOf (method.dfault);
        b := LookUp (p, method.name, top, tVisible); <* ASSERT b *>

        AssignStmt.PrepForEmit (t_default, method.dfault, initializing := TRUE);
        CG.Push (ptr);
        CG.Boost_addr_alignment (Target.Address.align);
        CG.Add_offset (method.offset);
        m_offset := MethodOffset (tVisible);
        IF (m_offset >= 0) THEN
          CG.Add_offset (m_offset);
        ELSE
          Type.LoadInfo (tVisible, M3RT.OTC_methodOffset);
          CG.Index_bytes (Target.Byte);
        END;
        CG.Boost_addr_alignment (Target.Address.align);
        AssignStmt.DoEmit (t_default, method.dfault, Target.Address.align);
      END;
      v := v.next;
    END;

    CG.Free (ptr);
    CG.Exit_proc (CG.Type.Void);
    CG.End_procedure (proc);
    RETURN proc;
  END GenLinkProc;

PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  (* Note: it is important to do the surface syntax checks before
     checking the types of the fields and methods.  Otherwise, we will
     add "a = t" to the current set of assumptions and may decide that
     other types are equal when they are not! *)
  VAR b: P := t;  fa, fb, ma, mb: Value.T;
  BEGIN
    IF (a = NIL)
      OR (a.isTraced # b.isTraced)
      OR NOT Brand.Equal (a.brand, b.brand) THEN
      RETURN FALSE;
    END;

    fa := Scope.ToList (a.fields);
    fb := Scope.ToList (b.fields);

    (* check the field names and offsets *)
    IF NOT Field.IsEqualList (fa, fb, x, types := FALSE) THEN RETURN FALSE; END;

    ma := Scope.ToList (a.methods);
    mb := Scope.ToList (b.methods);

    (* check the method names and offsets *)
    IF NOT Method.IsEqualList (ma, mb, x, types := FALSE) THEN RETURN FALSE; END;

    (* check the super types *)
    IF NOT Type.IsEqual (a.superType, b.superType, x) THEN
      RETURN FALSE;
    END;

    (* check the field types and default values *)
    IF NOT Field.IsEqualList (fa, fb, x, types := TRUE) THEN RETURN FALSE; END;

    (* check the method types and defaults *)
    IF NOT Method.IsEqualList (ma, mb, x, types := TRUE) THEN RETURN FALSE; END;

    RETURN TRUE;
  END EqualChk;

PROCEDURE Subtyper (a: P;  t: Type.T): BOOLEAN =
  VAR root := Reff.T;
  BEGIN
    IF (NOT a.isTraced) THEN root := Addr.T END;
    IF Type.IsEqual (t, root, NIL) THEN RETURN TRUE END;
    RETURN Type.IsEqual (a, t, NIL)
        OR ((a.superType # NIL) AND Type.IsSubtype (a.superType, t));
  END Subtyper;

PROCEDURE InitCoster (<*UNUSED*> p: P;  zeroed: BOOLEAN): INTEGER =
  BEGIN
    IF (zeroed) THEN RETURN 0 ELSE RETURN 1 END;
  END InitCoster;

PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  VAR v: Value.T;  n: INTEGER;
  BEGIN
    IF Type.IsEqual (p, ObjectRef.T, NIL) THEN
      x.tag := "$objectref";
      x.n_nodes := 0;
    ELSIF Type.IsEqual (p, ObjectAdr.T, NIL) THEN
      x.tag := "$objectadr";
      x.n_nodes := 0;
    ELSE
      M3Buf.PutText (x.buf, "OBJECT");
      IF (NOT p.isTraced) THEN M3Buf.PutText (x.buf, "-UNTRACED") END;
      Brand.GenFPrint (p.brand, x);

      (* count the children *)
      n := 1; (* for supertype *)
      v := Scope.ToList (p.fields);
      WHILE (v # NIL) DO  INC (n, Value.AddFPTag (v, x));  v := v.next;  END;
      v := Scope.ToList (p.methods);
      IF (v # NIL) THEN
        M3Buf.PutText (x.buf, " METHODS");
        WHILE (v # NIL) DO  INC (n, Value.AddFPTag (v, x));  v := v.next; END;
      END;
      x.n_nodes := n;

      (* add the children *)
      IF (n <= NUMBER (x.nodes)) THEN
        x.nodes[0] := p.superType;  n := 1;
      ELSE
        x.others := NEW (REF ARRAY OF Type.T, n);
        x.others[0] := p.superType;  n := 1;
      END;
      v := Scope.ToList (p.fields);
      WHILE (v # NIL) DO  n := Value.AddFPEdges (v, x, n);  v := v.next;  END;
      v := Scope.ToList (p.methods);
      WHILE (v # NIL) DO  n := Value.AddFPEdges (v, x, n);  v := v.next;  END;
    END;
  END FPrinter;

PROCEDURE MethodOffset (t: Type.T): INTEGER =
  VAR p := Confirm (t);
  BEGIN
    IF (p = NIL) THEN RETURN Unknown_w_magic END;
    GetOffsets (p, use_magic := TRUE);
    RETURN p.methodOffset;
  END MethodOffset;

PROCEDURE GetFieldOffset (t: Type.T;  VAR offset, align: INTEGER) =
  VAR p := Confirm (t);
  BEGIN
    IF (p = NIL) THEN
      offset := Unknown_w_magic;
      align  := Target.Byte;
    ELSE
      GetOffsets (p, use_magic := TRUE);
      offset := p.fieldOffset;
      align  := p.fieldAlign;
    END;
  END GetFieldOffset;

PROCEDURE FieldAlignment (t: Type.T): INTEGER =
  VAR p := Confirm (t);
  BEGIN
    IF (p = NIL) THEN RETURN Target.Byte END;
    GetSizes (p);
    RETURN p.fieldAlign;
  END FieldAlignment;

(*********
PROCEDURE FieldSize (t: Type.T): INTEGER =
  VAR p := Confirm (t);
  BEGIN
    IF (p = NIL) THEN RETURN Unknown_w_magic END;
    GetOffsets (p, use_magic := TRUE);
    IF (p.fieldOffset < 0) THEN RETURN Unknown_w_magic END;
    RETURN RecordType.RoundUp (p.fieldOffset + p.fieldSize, p.fieldAlign);
  END FieldSize;
***********)

PROCEDURE GetSizes (p: P) =
  VAR min_size: INTEGER;
      solid: BOOLEAN;
  BEGIN
    IF (p.fieldSize >= 0) THEN (* already done *) RETURN END;

    IF (p.superType = NIL) THEN  (* p is ROOT or UNTRACED ROOT *)
      p.fieldSize    := 0;
      p.fieldAlign   := Target.Address.align;
    ELSE
      (* compute the field sizes and alignments *)
      RecordType.SizeAndAlignment (p.fields, p.info.lazyAligned, min_size,
                                   p.fieldSize, p.fieldAlign, solid);

      (* round the object's size up to at least the size of a heap header *)
      p.fieldSize := RecordType.RoundUp (p.fieldSize, Target.Address.size);
    END;
  END GetSizes;

PROCEDURE GetOffsets (p: P;  use_magic: BOOLEAN) =
  VAR super: P;  d_size, m_size: INTEGER;
  BEGIN
    IF (p.fieldOffset >= 0) THEN
      (* already done *)
      RETURN;
    ELSIF (p.fieldOffset = Unchecked_offset) THEN
      (* we haven't tried yet *)
    ELSIF (p.fieldOffset = Unknown_w_magic) THEN
      (* we've tried everything already *)
      RETURN;
    ELSIF (NOT use_magic) THEN
      (* we've already tried it without magic *)
      RETURN;
    END;

    GetSizes (p);

    IF (p.superType = NIL) THEN  (* p is ROOT or UNTRACED ROOT *)
      p.fieldOffset  := Target.Address.size;
      p.methodOffset := 0;
    ELSE
      IF (use_magic) THEN
        p.fieldOffset  := Unknown_w_magic;
        p.methodOffset := Unknown_w_magic;
      ELSE
        p.fieldOffset  := Unknown_wo_magic;
        p.methodOffset := Unknown_wo_magic;
      END;

      (* try to get my supertype's offset *)
      super := Confirm (p.superType);
      IF (super # NIL) THEN (* supertype is visible *)
        GetOffsets (super, use_magic);
        IF (super.fieldOffset >= 0) THEN
          p.fieldOffset  := super.fieldOffset + super.fieldSize;
          p.fieldOffset  := RecordType.RoundUp (p.fieldOffset, p.fieldAlign);
          p.methodOffset := super.methodOffset + super.methodSize;
        END;
      ELSIF (use_magic)
        AND FindMagic (Type.GlobalUID (p.superType), d_size, m_size) THEN
        p.fieldOffset  := RecordType.RoundUp (d_size, p.fieldAlign);
        p.methodOffset := m_size;
      END;
    END;
  END GetOffsets;

PROCEDURE FindMagic (t_id: INTEGER;  VAR d_size, m_size: INTEGER): BOOLEAN =
  VAR super, my_d_size, my_d_align, my_m_size, s_d_size, s_m_size: INTEGER;
  BEGIN
    IF (NIL_ID = 0) THEN
      NIL_ID   := Type.GlobalUID (NIL);
      ROOT_ID  := Type.GlobalUID (ObjectRef.T);
      UROOT_ID := Type.GlobalUID (ObjectAdr.T);
    END;

    IF (t_id = NIL_ID) OR (t_id = ROOT_ID) OR (t_id = UROOT_ID) THEN
      d_size := Target.Address.size;
      m_size := 0;
      RETURN TRUE;

    ELSIF NOT Host.env.find_opaque_magic (t_id, super, my_d_size,
                                          my_d_align, my_m_size) THEN
      RETURN FALSE;

    ELSIF NOT FindMagic (super, s_d_size, s_m_size) THEN
      RETURN FALSE;

    ELSE (* we did it! *)
      d_size := s_d_size + my_d_size;
      d_size := RecordType.RoundUp (d_size, my_d_align);
      m_size := s_m_size + my_m_size;
      RETURN TRUE;

    END;
  END FindMagic;

PROCEDURE Confirm (t: Type.T): P =
  VAR info: Type.Info;
  BEGIN
    t := Type.StripPacked (t); 
    LOOP
      t := Type.CheckInfo (t, info);
      IF (info.class = Type.Class.Object) THEN
        RETURN t;
      ELSIF (info.class = Type.Class.Opaque) THEN
        t := Revelation.LookUp (t);
      ELSE
        RETURN NIL;
      END;
    END;
  END Confirm;

BEGIN
END ObjectType.
