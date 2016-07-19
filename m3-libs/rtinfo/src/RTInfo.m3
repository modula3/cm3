UNSAFE MODULE RTInfo;

IMPORT
  Env,
  FileRd,
  Fmt,
  IntRefTbl,
  MxConfig,
  OSError,
  Pathname,
  Param,
  Rd,
  RefSeq,
  RT0,
  RTHeap,
  RTHooks,
  RTType,
  RTTypeSRC,
  Stdio,
  Text,
  TextIntTbl,
  TextRefTbl,
  Thread,
  Word,
  Wr,

  DateOps,
  TimeOps,
  TextOps;

REVEAL
  T = Public BRANDED "HoNet-RTInfo.T-1.0" OBJECT
    rd: Rd.T;
    external: BOOLEAN;
    ch: CHAR;
    types: IntRefTbl.T;
    typesByName,
    typesByBrand,
    typesByUID: TextRefTbl.T;
    args: IntRefTbl.T;
  METHODS
    addBuiltin(uid: INTEGER; name: TEXT; aligned: BOOLEAN) := AddBuiltin;
    skipHeader() RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := SkipHeader;
    skipBlanks() RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := SkipBlanks;
    nextChar() RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := NextChar;
    nextLine() RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := NextLine;
    scanName(): TEXT RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanName;
    scanText(): TEXT RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanText;
    scanInt(): INTEGER RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanInt;
    scanUID(): INTEGER RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanUID;
    scanTypeUID(calledByScanTypeName: BOOLEAN := FALSE): Type RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanTypeUID;
    scanSignatureUID(): Signature RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanSignatureUID;
    scanTypeName() RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanTypeName;
    scanField(type: Type) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanField;
    scanMethod(type: Type; override: BOOLEAN) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanMethod;
    scanSignature() RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanSignature;
    scanVarArg() RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanVarArg;
    scanOpenArray() RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanOpenArray;
    scanSubrange() RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanSubrange;
    scanArray() RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanArray;
    scanRef(traced: BOOLEAN) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanRef;
    scanPacked() RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanPacked;
    scanTypeEnumDefinition() RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanTypeEnumDefinition;
    scanTypeSetDefinition() RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanTypeSetDefinition;
    scanTypeDefinition(traced, record: BOOLEAN) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanTypeDefinition;
    scanTypeRevelation() RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } := ScanTypeRevelation;
  OVERRIDES
    init := InitT;
    parse := Parse;
    getType := GetType;
    getTypeByName := GetTypeByName;
    getTypeByBrand := GetTypeByBrand;
    getTypeByUID := GetTypeByUID;
    getTypeByTypecode := GetTypeByTypecode;
    getSubtypesOf := GetSubtypesOf;
    getField := GetField;
    setField := SetField;
    show := Show;
  END;

REVEAL
  Type = PublicType BRANDED "HoNet-RTInfo.Type-1.0" OBJECT
    aligned: BOOLEAN;
    cached: TextRefTbl.T;
  OVERRIDES
    fieldsLayout := FieldsLayout;
    getFieldByName := GetFieldByName;
    isSubtypeOf := IsSubtypeOf;
    getTypecodeByBrand := GetTypecodeByBrand;
    getTypecodeByUID := GetTypecodeByUID;
  END;

CONST
  Blanks = SET OF CHAR { ' ' };

VAR
  DebugOn: BOOLEAN := FALSE; (* Param.Switch("-debugrti"); *) 

PROCEDURE Log(<*UNUSED*>msg: TEXT) =
  BEGIN
    IF DebugOn THEN
      (* Debug.Log("[RTI] " & msg); *)
    END;
  END Log;

PROCEDURE AddBuiltin(t: T; uid: INTEGER; name: TEXT; aligned: BOOLEAN) =
  VAR
    type: Type := NEW(Type, uid := uid, name := name, aligned := aligned, external := TRUE);
  BEGIN
    type.aliases := NEW(RefSeq.T).init();
    type.fields := NEW(RefSeq.T).init();
    
    EVAL t.types.put(uid, type);
    EVAL t.typesByName.put(name, type);
    EVAL t.typesByUID.put(Fmt.Unsigned(uid), type);
  END AddBuiltin;

PROCEDURE SkipHeader(t: T) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  BEGIN
    WHILE Text.GetChar(Rd.GetLine(t.rd), 0) # '$' DO END;
  END SkipHeader;

PROCEDURE SkipBlanks(t: T) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  BEGIN
    WHILE t.ch IN Blanks DO
      t.nextChar();
    END;
  END SkipBlanks;

PROCEDURE NextChar(t: T) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  BEGIN
    t.ch := Rd.GetChar(t.rd);
  END NextChar;

PROCEDURE NextLine(t: T) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  BEGIN
    WHILE t.ch # '\n' DO
      t.nextChar();
    END;
    t.nextChar();
  END NextLine;

PROCEDURE ScanName(t: T): TEXT RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    text: TEXT := "";
  BEGIN
    WHILE NOT t.ch IN (Blanks + SET OF CHAR { '\n' }) DO
      IF t.ch # '\r' THEN
        text := text & Text.FromChar(t.ch);
      END;
      t.nextChar();
    END;
    RETURN text;
  END ScanName;

PROCEDURE ScanText(t: T): TEXT RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    text: TEXT := "";
  BEGIN
    WHILE t.ch # '\n' DO
      IF t.ch # '\r' THEN
        text := text & Text.FromChar(t.ch);
      END;
      t.nextChar();
    END;
    RETURN text;
  END ScanText;

PROCEDURE ScanInt(t: T): INTEGER RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    int: INTEGER := 0;
  BEGIN
    LOOP
      IF (t.ch >= '0') AND (t.ch <= '9') THEN
        int := int*10 + ORD(t.ch) - ORD('0');
      ELSE
        EXIT;
      END;
      t.nextChar();
    END;
    RETURN int;
  END ScanInt;

PROCEDURE ScanUID(t: T): INTEGER RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    uid: INTEGER := 0;
    digit: INTEGER;
  BEGIN
    LOOP
      IF (t.ch >= '0') AND (t.ch <= '9') THEN
        digit := ORD(t.ch) - ORD('0');
      ELSIF (t.ch >= 'a') AND (t.ch <= 'f') THEN
        digit := 10 + ORD(t.ch) - ORD('a');
      ELSE
        EXIT;
      END;

      uid := Word.LeftShift(uid, 4) + digit;
      uid := Word.And(uid, 16_ffffffff);

      t.nextChar();
    END;

    RETURN uid;
  END ScanUID;

PROCEDURE ScanTypeUID(t: T; <*UNUSED*>calledByScanTypeName: BOOLEAN := FALSE): Type RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    uid: INTEGER;
    type: Type;
    ra: REFANY;
  BEGIN
    uid := t.scanUID();

    IF t.types.get(uid, ra) THEN
      RETURN ra;
    END;

    (*
    IF NOT calledByScanTypeName THEN
      <* DEBUG "Forward reference for uid = " & Fmt.Int(uid) & ", but not from ScanTypeName" *>
    END;
    *)
    (* Sat Apr 07 21:45 2012: IMO, this is place to be only when we parse 'E'. Checking now.
    *)
   
    type := NEW(Type, uid := uid, external := t.external);
    type.aliases := NEW(RefSeq.T).init();
    type.fields := NEW(RefSeq.T).init();
    type.methods := NEW(RefSeq.T).init();
    type.cached := NEW(TextRefTbl.Default).init();
    
    Log("Type created without name, UID = 16_" & Fmt.Int(uid, 16) & "\n");
    EVAL t.types.put(uid, type);
    EVAL t.typesByUID.put(Fmt.Unsigned(uid), type);
    (* Name will be added later *)

    RETURN type;
  END ScanTypeUID;

PROCEDURE ScanSignatureUID(t: T): Signature RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    uid: INTEGER;
    args: Signature;
    ra: REFANY;
  BEGIN
    uid := t.scanUID();

    IF NOT t.args.get(uid, ra) THEN
      args := NEW(Signature, list := NEW(RefSeq.T).init(), exceptions := NEW(RefSeq.T).init());
      EVAL t.args.put(uid, args);
    ELSE
      args := ra;
    END;

    RETURN args;
  END ScanSignatureUID;

(* 'E'
   Parse() is only caller of this method.
*)
PROCEDURE ScanTypeName(t: T) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    type: Type;
    alias: TEXT;
  BEGIN
    type := t.scanTypeUID();
    IF type.name = NIL THEN
      t.skipBlanks();
      type.name := t.scanText();
      Log("Adding: " & type.name & " type\n");
      EVAL t.typesByName.put(type.name, type);
    ELSE
      t.skipBlanks();
      alias := t.scanText();
      type.aliases.addhi(alias);
      Log("Adding: " & alias & " alias\n");
      EVAL t.typesByName.put(alias, type);
    END;
    t.nextLine();
  END ScanTypeName;

PROCEDURE ScanField(t: T; type: Type) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    field: Field := NEW(Field);
  BEGIN
    t.nextChar();
    field.name := t.scanName();
    
    t.skipBlanks();
    field.offset := t.scanInt();
    
    t.skipBlanks();
    field.size := t.scanInt();
    
    t.skipBlanks();
    field.type := t.scanTypeUID();

    t.nextLine();
    FOR i := 1 TO type.fields.size() DO
      WITH f = NARROW(type.fields.get(i-1), Field) DO
        IF Text.Equal(f.name, field.name) AND f.offset = field.offset THEN
          Log("  Discarding duplicate field " & "." & f.name);
          RETURN;
        END;
      END;
    END;
    
    type.fields.addhi(field);
  END ScanField;

(* 'W' - method, 'X' - override
*)
PROCEDURE ScanMethod(t: T; type: Type; override: BOOLEAN) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    method: Method := NEW(Method, override := override);
  BEGIN
    t.nextChar();
    method.name := t.scanName();
    t.skipBlanks();
    IF NOT override THEN
      method.args := t.scanSignatureUID();
      t.skipBlanks();
    END;
    method.proc := t.scanName();

    t.nextLine();
    FOR i := 1 TO type.methods.size() DO
      WITH m = NARROW(type.methods.get(i-1), Method) DO
        IF Text.Equal(m.name, method.name) THEN
          Log("  Discarding duplicate method " & "." & m.name);
          RETURN;
        END;
      END;
    END;

    type.methods.addhi(method);
  END ScanMethod;

(* This is method signature, 'R'
*)
PROCEDURE ScanSignature(t: T) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    args: Signature;
    arg: Arg;
    n: CARDINAL;
  BEGIN
    args := t.scanSignatureUID();
    t.skipBlanks();
    n := t.scanInt(); (* Arguments *)
    t.skipBlanks();
    args.return := t.scanTypeUID(); (* Return value *)
    t.skipBlanks();
    n := n + t.scanInt(); (* Exceptions *)

    t.nextLine();

    FOR i := 1 TO n DO
      CASE t.ch OF
      | 'S' =>
        t.nextChar();
        arg := NEW(Arg);
        arg.name := t.scanName();
        t.skipBlanks();
        arg.type := t.scanTypeUID();
        args.list.addhi(arg);
        
      | 'T' =>
        t.nextChar();
        args.exceptions.addhi(t.scanName());
      ELSE
        <* ASSERT FALSE *>
      END;
      t.nextLine();
    END;
  END ScanSignature;

(* 'Q'
   This is "var arg type".. "by reference" formal argument.
   It looks like there is no difference in .M3WEB between VAR and READONLY formals.
*)
PROCEDURE ScanVarArg(t: T) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    type: Type;
  BEGIN
    type := t.scanTypeUID();
    t.skipBlanks();
    type.varArg := t.scanTypeUID();
    t.nextLine();
  END ScanVarArg;

(* 'G'
*)
PROCEDURE ScanOpenArray(t: T) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    type: Type;
  BEGIN
    type := t.scanTypeUID();
    t.skipBlanks();
    type.openArray := t.scanTypeUID();
    t.nextLine();
  END ScanOpenArray;

(* 'N'
*)
PROCEDURE ScanSubrange(t: T) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    type: Type;
  BEGIN
    type := t.scanTypeUID();
    t.skipBlanks();
    type.subrange := NEW(Subrange);
    type.subrange.baseType := t.scanTypeUID();
    t.skipBlanks();
    type.subrange.first := t.scanInt();
    t.skipBlanks();
    type.subrange.last := t.scanInt();
    t.skipBlanks();
    type.subrange.elemSize := t.scanInt();
    t.nextLine();
  END ScanSubrange;

PROCEDURE ScanArray(t: T) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    type: Type;
  BEGIN
    type := t.scanTypeUID();
    t.skipBlanks();
    type.array := NEW(Array);
    type.array.indexType := t.scanTypeUID();
    t.skipBlanks();
    type.array.elemType := t.scanTypeUID();
    t.skipBlanks();
    type.bits := t.scanInt();
    t.nextLine();
  END ScanArray;

PROCEDURE ScanRef(t: T; traced: BOOLEAN) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    type: Type;
  BEGIN
    type := t.scanTypeUID();
    t.skipBlanks();
    type.traced := traced;
    type.ref := t.scanTypeUID();
    t.nextLine();
  END ScanRef;

(* 'J'
*)
PROCEDURE ScanPacked(t: T) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    type: Type;
  BEGIN
    type := t.scanTypeUID();
    t.skipBlanks();
    type.bits := t.scanInt();
    t.skipBlanks();
    type.packed := t.scanTypeUID();
    t.nextLine();
  END ScanPacked;

(* 'H'
*)
PROCEDURE ScanTypeEnumDefinition(t: T) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    type: Type;
    enums: CARDINAL;
  BEGIN
    type := t.scanTypeUID();
    t.skipBlanks();
    enums := t.scanInt();
    t.skipBlanks();
    type.bits := t.scanInt();
    t.nextLine();
    type.enums := NEW(TextIntTbl.Default).init();
    type.enumsByOrd := NEW(REF ARRAY OF TEXT, enums);
    
    FOR i := 1 TO enums DO
      CASE t.ch OF
      | 'I' =>
        t.nextChar();
        WITH item = t.scanText() DO
          EVAL type.enums.put(item, i-1);
          type.enumsByOrd[i-1] := item;
        END;
        t.nextLine();
      ELSE
        <* ASSERT FALSE *>
      END;
    END;
  END ScanTypeEnumDefinition;

(* 'M'
*)
PROCEDURE ScanTypeSetDefinition(t: T) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    type: Type;
  BEGIN
    type := t.scanTypeUID();
    t.skipBlanks();
    type.setOf := t.scanTypeUID();
    t.nextLine();
  END ScanTypeSetDefinition;

(* 'U', 'V' untraced and traced object type definition
   'K' - record, meaning only fields are present
*)
PROCEDURE ScanTypeDefinition(t: T; traced, record: BOOLEAN) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    type: Type;
    nrFields, nrMethods, nrOverrides: INTEGER;
  BEGIN
    type := t.scanTypeUID();
    t.skipBlanks();

    IF record THEN
      type.bits := t.scanInt();
      t.skipBlanks();
      nrFields := t.scanInt();
    ELSE
      type.traced := traced;
      
      type.super := t.scanTypeUID();
      t.skipBlanks();
      nrFields := t.scanInt();
      t.skipBlanks();
      nrMethods := t.scanInt();
      t.skipBlanks();
      nrOverrides := t.scanInt();
      t.skipBlanks();
      type.bits := t.scanInt();

      t.skipBlanks();
      type.brand := t.scanText();
    END;

    (* nrFields L, nrMethods W, nrOverrides X
    *)

    IF NOT Text.Empty(type.brand) THEN
      EVAL t.typesByBrand.put(type.brand, type);
    END;

    t.nextLine(); (* End of U/V/K line *)

    LOOP
      CASE t.ch OF
      | 'L' =>
        t.scanField(type);
        DEC(nrFields);
        
      | 'X', 'W' =>
        IF record THEN
          <* DEBUG "'W'/'X' following 'K'" *>
        END;
        WITH override = (t.ch = 'X') DO
          t.scanMethod(type, override := override);
          IF override THEN
            DEC(nrOverrides);
          ELSE
            DEC(nrMethods);
          END;
        END;        
      ELSE
        EXIT;
      END;
    END;
    (* Insert right check in right places.. this is not ok now that we scan records with this method
    <* ASSERT nrFields = 0 AND nrMethods = 0 AND nrOverrides = 0 *>
    *)
  END ScanTypeDefinition;

(* 'Z'
*)
PROCEDURE ScanTypeRevelation(t: T) RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR
    revel, type: Type;
  BEGIN
    type := t.scanTypeUID();
    t.skipBlanks();
    revel := t.scanTypeUID();
    type.revel := revel;
    type.brand := revel.brand;
    t.nextLine();
  END ScanTypeRevelation;

PROCEDURE InitT(t: T): T =
  BEGIN
    t.external := FALSE;
    t.types := NEW(IntRefTbl.Default).init();
    t.typesByName := NEW(TextRefTbl.Default).init();
    t.typesByBrand := NEW(TextRefTbl.Default).init();
    t.typesByUID := NEW(TextRefTbl.Default).init();
    t.args := NEW(IntRefTbl.Default).init();

    t.addBuiltin(16_195c2a74, "INTEGER", TRUE);
    t.addBuiltin(16_05562176, "LONGINT", TRUE);
    t.addBuiltin(16_97e237e2, "CARDINAL", TRUE);
    t.addBuiltin(16_1e59237d, "BOOLEAN", FALSE);
    t.addBuiltin(16_08402063, "ADDRESS", TRUE);
    t.addBuiltin(16_56e16863, "CHAR", FALSE);
    t.addBuiltin(16_48e16572, "REAL", TRUE);
    t.addBuiltin(16_94fe32f6, "LONGREAL", TRUE);
    t.addBuiltin(16_9ee024e3, "EXTENDED", TRUE);
    t.addBuiltin(16_48ec756e, "NULL", TRUE);
    t.addBuiltin(16_1c1c45e6, "REFANY", TRUE);
    t.addBuiltin(16_00000000, "VOID", TRUE);
    t.addBuiltin(16_9d8fb489, "ROOT", TRUE);
    t.addBuiltin(16_898ea789, "UNTRACED-ROOT", TRUE);
    t.addBuiltin(16_50f86574, "TEXT", TRUE);
    t.addBuiltin(16_1541f475, "MUTEX", TRUE);

    RETURN t;
  END InitT;

PROCEDURE Parse(t: T; filename: TEXT; external: BOOLEAN) RAISES { OSError.E, Rd.Failure, Thread.Alerted } =
  BEGIN
    t.rd := FileRd.Open(filename);
    t.external := external;
    TRY
      t.skipHeader();
      t.nextChar();

      WHILE NOT Rd.EOF(t.rd) DO
        CASE t.ch OF
        | 'E' =>
          t.nextChar();
          t.scanTypeName();
          
        | 'H' =>
          t.nextChar();
          t.scanTypeEnumDefinition();
          
        | 'M' =>
          t.nextChar();
          t.scanTypeSetDefinition();
          
        | 'U', 'V', 'K' =>
          WITH traced = (t.ch = 'V'), record = (t.ch = 'K') DO
            t.nextChar();
            t.scanTypeDefinition(traced, record);
          END;

        | 'Z' =>
          t.nextChar();
          t.scanTypeRevelation();
          
        | 'R' =>
          t.nextChar();
          t.scanSignature();
          
        | 'Q' =>
          t.nextChar();
          t.scanVarArg();
          
        | 'G' =>
          t.nextChar();
          t.scanOpenArray();
          
        | 'N' =>
          t.nextChar();
          t.scanSubrange();
          
        | 'F' =>
          t.nextChar();
          t.scanArray();
          
        | 'O', 'P' =>
          t.nextChar();
          t.scanRef(t.ch = 'P');

        | 'J' =>
          t.nextChar();
          t.scanPacked();

        | (* '@', 'A', 'B', *) 'C', 'D' =>
          t.nextLine(); (* silent ignore *)
          (* start of compilation unit, implementation, interface, import, exports *)

        ELSE
          (*
          <* DEBUG "in ELSE: " & (* Text.FromChar(t.ch) & *)t.scanText() *>
          *)
          t.nextLine();
        END;
      END;
    EXCEPT
    | Rd.EndOfFile =>
    END;

    Rd.Close(t.rd);
  END Parse;

PROCEDURE GetType(t: T; ra: REFANY): Type =
  BEGIN
    RETURN t.getTypeByName(RTTypeSRC.TypecodeName(TYPECODE(ra)));
  END GetType;

PROCEDURE GetTypeByName(t: T; name: TEXT): Type =
  VAR
    ra: REFANY := NIL;
  BEGIN
    EVAL t.typesByName.get(name, ra);
    RETURN ra;
  END GetTypeByName;

PROCEDURE GetTypeByBrand(t: T; brand: TEXT): Type =
  VAR
    ra: REFANY := NIL;
  BEGIN
    EVAL t.typesByBrand.get(brand, ra);
    RETURN ra;
  END GetTypeByBrand;

PROCEDURE GetTypeByTypecode(t: T; tc: CARDINAL): Type =
  BEGIN
    RETURN t.getTypeByName(RTTypeSRC.TypecodeName(tc));
  END GetTypeByTypecode;

PROCEDURE GetTypeByUID(t: T; hexUID: TEXT): Type =
  VAR
    ra: REFANY := NIL;
  BEGIN
    EVAL t.typesByUID.get(hexUID, ra);
    RETURN ra;
  END GetTypeByUID;

PROCEDURE GetSubtypesOf(t: T; type: Type): RefSeq.T =
  VAR
    subtypes: RefSeq.T := NEW(RefSeq.T).init();
    subtype: Type;
    uid: INTEGER;
    ra: REFANY;
  BEGIN
    WITH i = t.types.iterate() DO
      WHILE i.next(uid, ra) DO
        subtype := ra;
        IF subtype.isSubtypeOf(type) THEN
          subtypes.addhi(subtype);
        END;
      END;
    END;
    RETURN subtypes;
  END GetSubtypesOf;

PROCEDURE GetField(t: T; ra: REFANY; fieldName: TEXT): TEXT =
  VAR
    type: Type;
    field: Field;
    adr: ADDRESS;
  BEGIN
    IF t = NIL THEN
      RETURN NIL;
    END;

    type := t.getType(ra);
    IF type = NIL THEN
      RETURN NIL;
    END;

    field := type.getFieldByName(fieldName);
    IF field = NIL THEN
      RETURN NIL;
    END;

    adr := RTHeap.GetDataAdr(ra) + (field.offset DIV 8);
    IF field.type = t.getTypeByName("CHAR") THEN
      RETURN Text.FromChar(LOOPHOLE(adr, UNTRACED REF CHAR)^);
    ELSIF field.type = t.getTypeByName("BOOLEAN") THEN
      RETURN Fmt.Bool(LOOPHOLE(adr, UNTRACED REF BOOLEAN)^);
    ELSIF field.type = t.getTypeByName("INTEGER") THEN
      RETURN Fmt.Int(LOOPHOLE(adr, UNTRACED REF INTEGER)^);
    ELSIF field.type = t.getTypeByName("CARDINAL") THEN
      RETURN Fmt.Int(LOOPHOLE(adr, UNTRACED REF CARDINAL)^);
    ELSIF field.type = t.getTypeByName("REAL") THEN
      RETURN Fmt.Real(LOOPHOLE(adr, UNTRACED REF REAL)^, Fmt.Style.Fix, 4);
    ELSIF field.type = t.getTypeByName("LONGREAL") THEN
      RETURN Fmt.LongReal(LOOPHOLE(adr, UNTRACED REF LONGREAL)^, Fmt.Style.Fix, 4);
    ELSIF field.type = t.getTypeByName("EXTENDED") THEN
      RETURN Fmt.Extended(LOOPHOLE(adr, UNTRACED REF EXTENDED)^, Fmt.Style.Fix, 4);
    ELSIF field.type = t.getTypeByName("TEXT") THEN
      RETURN LOOPHOLE(adr, UNTRACED REF TEXT)^;
    ELSIF field.type = t.getTypeByName("DateOps.T") THEN
      RETURN DateOps.ToText(LOOPHOLE(adr, UNTRACED REF DateOps.T)^);
    ELSIF field.type = t.getTypeByName("TimeOps.T") THEN
      RETURN TimeOps.ToText(LOOPHOLE(adr, UNTRACED REF TimeOps.T)^);
    ELSIF field.type.enums # NIL THEN
      WITH ord = ORD(LOOPHOLE(adr, UNTRACED REF CHAR)^) DO
        RETURN field.type.enumsByOrd[ord];
      END;
    ELSE
      RETURN "???";
    END;
  END GetField;

PROCEDURE SetField(t: T; ra: REFANY; fieldName: TEXT; value: TEXT): BOOLEAN =
  VAR
    type: Type;
    field: Field;
    i: INTEGER;
    adr: ADDRESS;
  BEGIN
    IF t = NIL THEN
      RETURN FALSE;
    END;

    type := t.getType(ra);
    IF type = NIL THEN
      RETURN FALSE;
    END;

    field := type.getFieldByName(fieldName);
    IF field = NIL THEN
      RETURN FALSE;
    END;

    adr := RTHeap.GetDataAdr(ra) + (field.offset DIV 8);
    IF field.type = t.getTypeByName("CHAR") THEN
      LOOPHOLE(adr, UNTRACED REF CHAR)^ := Text.GetChar(value, 0);
    ELSIF field.type = t.getTypeByName("BOOLEAN") THEN
      LOOPHOLE(adr, UNTRACED REF BOOLEAN)^ := TextOps.ToBoolean(value);
    ELSIF field.type = t.getTypeByName("INTEGER") THEN
      LOOPHOLE(adr, UNTRACED REF INTEGER)^ := TextOps.ToInt(value);
    ELSIF field.type = t.getTypeByName("CARDINAL") THEN
      LOOPHOLE(adr, UNTRACED REF CARDINAL)^ := TextOps.ToInt(value, FIRST(CARDINAL), LAST(CARDINAL));
    ELSIF field.type = t.getTypeByName("REAL") THEN
      LOOPHOLE(adr, UNTRACED REF REAL)^ := TextOps.ToRReal(value);
    ELSIF field.type = t.getTypeByName("LONGREAL") THEN
      LOOPHOLE(adr, UNTRACED REF LONGREAL)^ := TextOps.ToReal(value);
    ELSIF field.type = t.getTypeByName("EXTENDED") THEN
      LOOPHOLE(adr, UNTRACED REF EXTENDED)^ := FLOAT(TextOps.ToReal(value), EXTENDED)
    ELSIF field.type = t.getTypeByName("TEXT") THEN
      (* Hosking's...traced being changed by LOOPHOLE()... Let system know about this new reference.

         Thu Apr 04 17:03 2013: LOOPHOLE hides referenced data change so following call is not inserted by
         compiler.
      *)
      RTHooks.CheckStoreTraced(ra);
      LOOPHOLE(adr, UNTRACED REF TEXT)^ := value;
    ELSIF field.type = t.getTypeByName("DateOps.T") THEN
      LOOPHOLE(adr, UNTRACED REF DateOps.T)^ := DateOps.FromText(value);
    ELSIF field.type = t.getTypeByName("TimeOps.T") THEN
      LOOPHOLE(adr, UNTRACED REF TimeOps.T)^ := TimeOps.FromText(value);
    ELSIF field.type.enums # NIL AND field.type.enums.get(value, i) THEN
      LOOPHOLE(adr, UNTRACED REF CHAR)^ := VAL(i, CHAR);
    ELSE
      RETURN FALSE;
    END;

    RETURN TRUE;
  END SetField;

PROCEDURE Show(t: T; ra: REFANY; maxDepth: CARDINAL := 0; wr: Wr.T := NIL) RAISES { Wr.Failure, Thread.Alerted } =
  VAR
    type: Type;
    field: Field;
    fields: RefSeq.T;
    value: TEXT;
    i: CARDINAL := 0;
    showPretty: BOOLEAN := Param.Switch("-showpretty");
    showType: BOOLEAN := Param.Switch("-showtype");
    adr: ADDRESS;
  BEGIN
    IF t = NIL THEN
      RETURN;
    END;

    type := t.getType(ra);
    IF type = NIL THEN
      RETURN;
    END;

    IF wr = NIL THEN
      wr := Stdio.stdout;
    END;

    IF showType THEN
      Wr.PutText(wr, type.name);
    END;
    Wr.PutText(wr, "{");
    IF showPretty THEN
      Wr.PutText(wr, "\n");
    END;

    fields := type.fieldsLayout();

    WHILE i < fields.size() DO
      field := fields.get(i);
      value := t.getField(ra, field.name);
      IF value = NIL THEN
        value := "NIL";
      END;

      IF showPretty THEN
        Wr.PutText(wr, "  ");
      END;
      Wr.PutText(wr, field.name);

      IF Text.Equal(value, "???") AND (maxDepth > 0) THEN
        adr := RTHeap.GetDataAdr(ra) + (field.offset DIV 8);
        t.show(LOOPHOLE(adr, UNTRACED REF REFANY)^, maxDepth-1, wr);
      ELSE
        IF showType THEN
          Wr.PutText(wr, ":" & field.type.name);
        END;
        Wr.PutText(wr, "=" & value);
      END;
      
      IF i+1 < fields.size() THEN
        Wr.PutText(wr, ",");
      END;
      IF showPretty THEN
        Wr.PutText(wr, "\n");
      END;
      INC(i);
    END;

    Wr.PutText(wr, "}\n");
    Wr.Flush(wr);
  END Show;

PROCEDURE FieldsLayout(type: Type): RefSeq.T =
  VAR
    fields: RefSeq.T := NEW(RefSeq.T).init();
    field: Field;
    offset: CARDINAL := 0;
  BEGIN
    IF type.super # NIL THEN
      (*
      IF type.super.name # NIL THEN
        <* DEBUG "SUPER " & type.super.name *>
      END;
      *)
      fields := RefSeq.Cat(fields, type.super.fieldsLayout());
    END;
    
    IF type.revel # NIL THEN
      IF type.revel.name # NIL THEN
        (*
        <* DEBUG "REVEAL " & type.revel.name *>
        *)
      END;
      fields := RefSeq.Cat(fields, type.revel.fieldsLayout());
    END;

    IF fields.size() > 0 THEN
      field := fields.gethi();
      offset := field.offset + field.size;
    ELSE
      offset := 0;
    END;

    FOR i := 1 TO type.fields.size() DO
      field := type.fields.get(i-1);
      IF (i=1) AND (field.type.aligned OR (field.type.super # NIL)) THEN
        (* Meaning it is a) an builtin aligned, or b) non builtin abject type.. Covers everything??
        *)
        offset := (offset + BITSIZE(REFANY) - 1) DIV BITSIZE(REFANY) * BITSIZE(REFANY);
      END;
      fields.addhi(NEW(Field, name := field.name, type := field.type, offset := offset + field.offset, size := field.size));
    END;
    
    RETURN fields;
  END FieldsLayout;

(* CHECK: Ovo prenebregava situaciju kad se trazi polje koje je u novijem tipu overriden. Ovo je
   trik i direktno u M3 (NARROW...) a iz klijenta ovoga... Jedino da buduca funkcija prima argument koji je
   tip do kog (tj od kog, _unazad_) ide u pretrazi.
       
   TODO: Napraviti u kešu da je index fieldName@narrowToTypeName za narrowe queries...
       
   PROBLEM: revealed tip cesto nije vidljiv u Modula-3 kodu, kad m3 kod ne importuje taj interfejs... Ovdje ce se
   ponasati kao da je SVE importovano. Security issue koji to nije, kod je moj. Sakrivanje je radi bolje enkapsulacije, ne da
   ja ne bih mogao nečemu prići.
   
   Ovdje naravno ide samo getFieldByName(fieldName: TEXT; narrowTo: Type := NIL): Field
*)

PROCEDURE GetFieldByName(type: Type; fieldName: TEXT): Field =
  VAR
    ra: REFANY;
  BEGIN
    IF type.cached.get(fieldName, ra) THEN
      RETURN ra;
    END;
    
    WITH fields = type.fieldsLayout() DO
      FOR i := fields.size() TO 1 BY -1 DO
        WITH field = NARROW(fields.get(i-1), Field) DO
          IF Text.Equal(field.name, fieldName) THEN
            EVAL type.cached.put(fieldName, field);
            RETURN field;
          END;
        END;
      END;
    END;

    EVAL type.cached.put(fieldName, NIL);
    RETURN NIL;
  END GetFieldByName;

PROCEDURE IsSubtypeOf(type: Type; supertype: Type): BOOLEAN =
  BEGIN
    IF supertype = NIL THEN
      RETURN FALSE;
    ELSIF type = NIL THEN
      RETURN FALSE;
    ELSIF type.uid = supertype.uid THEN
      RETURN TRUE;
    ELSIF type.super = NIL THEN
      IF type.revel # NIL THEN
        RETURN type.revel.isSubtypeOf(supertype);
      ELSE
        RETURN FALSE;
      END;
    ELSIF type.super.uid = supertype.uid THEN
      RETURN TRUE;
    ELSE
      RETURN type.super.isSubtypeOf(supertype);
    END;
  END IsSubtypeOf;

PROCEDURE GetTypecodeByBrand(type: Type): CARDINAL =
  VAR
    t: RT0.TypeDefn;
  BEGIN
    FOR i := 1 TO RTType.MaxTypecode()-1 DO
      t := RTType.Get(i);
      IF t.brand_ptr # NIL THEN
        IF Text.Equal(type.brand, Text.FromChars(SUBARRAY(t.brand_ptr.chars, 0, t.brand_ptr.length))) THEN
          (* M3toC.StoT(LOOPHOLE(t.brand_ptr.chars, Ctypes.char_star))) *)
          RETURN i;
        END;
      END;
    END;
    RETURN LAST(CARDINAL);
  END GetTypecodeByBrand;

PROCEDURE GetTypecodeByUID(type: Type): CARDINAL =
  BEGIN
    RETURN RTTypeSRC.FindType(type.uid).typecode;
  END GetTypecodeByUID;

PROCEDURE DoImportFile(filename: TEXT; base: BOOLEAN): BOOLEAN =
  BEGIN
    TRY
      Log(filename & "...\n");
        t.parse(filename, base);

      Log("... ok.\n");
      RETURN TRUE;
    EXCEPT
    | OSError.E =>
      Log("... unable to open file.\n");

    | Rd.Failure, Thread.Alerted =>
      Log("... unable to parse file.\n");
    END;
    RETURN FALSE;
  END DoImportFile;

VAR
  PKG_INSTALL := "/usr/local/cm3/pkg"; (* My default *)
  BUILD_DIR := MxConfig.HOST; (* assumes HOST = TARGET == no cross. Of course. *)

PROCEDURE ImportPkg(pkg: TEXT): BOOLEAN =
  BEGIN
    RETURN DoImportFile(PKG_INSTALL & "/" & pkg & "/" & BUILD_DIR & "/.M3WEB", TRUE);
  END ImportPkg;

PROCEDURE DoImportBase() =
  BEGIN
    EVAL ImportPkg("libm3");
    EVAL ImportPkg("tcp");
    EVAL ImportPkg("udp");
    EVAL ImportPkg("tempfiles");

    EVAL ImportPkg("m3lib0");
    EVAL ImportPkg("xl");
  END DoImportBase;

PROCEDURE DoImportDefault() =
  VAR
    exename, path, name: TEXT;
  BEGIN
    exename := Param.Item(0);
    Log("exename is '" & exename & "'.\n");
    IF Text.FindChar(exename, '/') = -1 THEN
      path := Env.Get("PATH");
      IF path # NIL THEN
        WITH dirs = TextOps.SplitS(path, ":") DO
          IF dirs.size() > 0 THEN
            FOR i := 0 TO dirs.size()-1 DO
              name := dirs.get(i) & "/" & Pathname.Base(exename) & ".rti";
              IF DoImportFile(name, FALSE) THEN
                RETURN;
              END;
            END;
          END;
        END;
      END;
    ELSE
      name := Pathname.Prefix(exename) & "/.M3WEB";
      IF DoImportFile(name, FALSE) THEN
        RETURN;
      END;
      name := Pathname.Base(exename) & ".rti";
      IF DoImportFile(name, FALSE) THEN
        RETURN
      END;
    END;
  END DoImportDefault;

PROCEDURE DoImportParams(param: TEXT; base: BOOLEAN): BOOLEAN =
  VAR
    files: TEXT;
    res: BOOLEAN := FALSE;
  BEGIN
    IF NOT Param.GetSwitch(param, files) THEN
      RETURN FALSE;
    END;
    WITH rtis = TextOps.SplitS(files, ",") DO
      IF rtis.size() > 0 THEN
        FOR i := 0 TO rtis.size()-1 DO
          IF DoImportFile(rtis.get(i), base) THEN
            res := TRUE;
          END;
        END;
      END;
    END;
    RETURN res;
  END DoImportParams;

PROCEDURE DoImportParamsBase(): BOOLEAN =
  BEGIN
    RETURN DoImportParams("-rtibase:", TRUE);
  END DoImportParamsBase;

PROCEDURE DoImportParamsDefault(): BOOLEAN =
  BEGIN
    RETURN DoImportParams("-rti:", FALSE);
  END DoImportParamsDefault;

PROCEDURE Init() =
  BEGIN
    t := NEW(T).init();
    (* Za ovo je potreban thread i mutex (moj rw?) na kome ce se lokovati klijenti...
       Jako cesto klijenti tek kasnije u toku rada dodju ili ne na neki od metoda ovog tipa,
       ali svejedno se ovaj init ceka...

       Wed Apr 18 21:46 2012: Measure delay here and decide. Or save as a pickle, scan once and pack for future use.
    *)
    IF NOT Param.Switch("-norti") THEN
      IF NOT DoImportParamsBase() THEN
        DoImportBase();
      END;
      IF NOT DoImportParamsDefault() THEN
        DoImportDefault();
      END;
      Log(Fmt.Int(t.types.size()) & " types registered.\n");
      Log(Fmt.Int(t.args.size()) & " args  registered.\n");
    END;
  END Init;

PROCEDURE Add(pkg: TEXT): BOOLEAN =
  BEGIN
    RETURN DoImportFile(pkg & ".M3WEB", FALSE);
  END Add;

BEGIN
  Init();
END RTInfo.
