INTERFACE RTInfo;

IMPORT
  OSError,
  Rd,
  RefSeq,
  TextIntTbl,
  Thread,
  Wr;

TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(): T;

    parse(filename: TEXT; external: BOOLEAN) RAISES { OSError.E, Rd.Failure, Thread.Alerted };

    getType(ra: REFANY): Type;

    getTypeByName(name: TEXT): Type;

    getTypeByBrand(brand: TEXT): Type;

    getTypeByTypecode(tc: CARDINAL): Type;

    getTypeByUID(hexUID: TEXT): Type;

    getSubtypesOf(type: Type): RefSeq.T;

    getField(ra: REFANY; fieldName: TEXT): TEXT;

    setField(ra: REFANY; fieldName: TEXT; value: TEXT): BOOLEAN;

    show(ra: REFANY; maxDepth: CARDINAL := 0; wr: Wr.T := NIL) RAISES { Wr.Failure, Thread.Alerted };
  END;

  Type <: PublicType;
  PublicType = OBJECT
    uid: INTEGER;
    name: TEXT := NIL;
    aliases: RefSeq.T;

    (* OBJECT type atributes
    *)
    brand: TEXT := "";
    super: Type := NIL;
    revel: Type := NIL;
    fields: RefSeq.T;
    methods: RefSeq.T;

    (* [UNTRACED] OBJECT, BITS FOR, RECORD, ARRAY, enums - all have this info at type level.
    *)
    bits: CARDINAL;

    (* REF and OBJECT can use this
    *)
    traced: BOOLEAN;
    
    (* Enum types
    *)
    enums: TextIntTbl.T := NIL;
    enumsByOrd: REF ARRAY OF TEXT := NIL;

    (* REF
    *)
    ref: Type := NIL;

    (* BITS FOR
    *)
    packed: Type := NIL;

    (* SET OF
    *)
    setOf: Type := NIL;

    (* VAR arg
    *)
    varArg: Type := NIL;

    (* BITS elemSize FOR [first .. last] OF elemType
    *)
    subrange: Subrange := NIL;

    (* ARRAY OF
    *)
    openArray: Type := NIL;

    (* BITS totalSize FOR ARRAY indexType OF elemType
    *)
    array: Array := NIL;

    (* what? *)
    external: BOOLEAN := FALSE;
  METHODS
    fieldsLayout(): RefSeq.T;

    getFieldByName(fieldName: TEXT): Field;

    isSubtypeOf(supertype: Type): BOOLEAN;

    getTypecodeByBrand(): CARDINAL;

    getTypecodeByUID(): CARDINAL;

    show(maxLevel: CARDINAL := LAST(CARDINAL));
  END;

  Field = OBJECT
    name: TEXT;
    type: Type;
    offset: CARDINAL;
    size: CARDINAL
  END;

  Subrange = OBJECT
    first, last: INTEGER;
    baseType: Type;
    elemSize: CARDINAL;
  END;

  Array = OBJECT
    indexType,
    elemType: Type;
    totalSize: CARDINAL;
  END;

  Method = OBJECT
    name: TEXT;
    override: BOOLEAN;
    proc: TEXT;
    args: Signature := NIL;
  END;

  Signature = OBJECT
    list: RefSeq.T;
    return: Type;
    exceptions: RefSeq.T;
  END;

  Arg = OBJECT
    name: TEXT;
    type: Type;
  END;

VAR
  t: T;

PROCEDURE Add(pkg: TEXT): BOOLEAN;
(* pkg here is full path of package.
*)

PROCEDURE ImportPkg(pkg: TEXT): BOOLEAN;
(* pkg is name of package, installed globally in PKG_INSTALL
*)

END RTInfo.
