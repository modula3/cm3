UNSAFE MODULE RTName;
IMPORT Ctypes, M3toC;
IMPORT RT0, RTType;
IMPORT RTTipe, RTPacking;
FROM RTTipe IMPORT Kind;

PROCEDURE GetByTipe(t: RTTipe.T): TEXT =
  BEGIN
    TYPECASE t OF
    | RTTipe.Builtin =>
      CASE t.kind OF
      | Kind.Boolean => RETURN "BOOLEAN";
      | Kind.Char => RETURN "CHAR";
      | Kind.Cardinal => RETURN "CARDINAL";
      | Kind.Integer => RETURN "INTEGER";
      | Kind.Address => RETURN "ADDRESS";
      ELSE
        RETURN "BUILTIN";
      END;
    | RTTipe.Array => RETURN "ARRAY";
    | RTTipe.Enum => RETURN "ENUM";
    | RTTipe.Object => RETURN "OBJECT";
    | RTTipe.OpenArray(e) => RETURN "REF ARRAY OF " & GetByTipe(e.element);
    | RTTipe.Packed => RETURN "PACKED";
    | RTTipe.Record(e) =>
      IF e.fields = NIL THEN
        RETURN "RECORD <empty>";
      ELSE
        RETURN "RECORD " & GetByTipe(e.fields.type) & "..";
      END;
    | RTTipe.Ref(e) => RETURN GetByDefn(e.self);
    | RTTipe.Set => RETURN "SET";
    | RTTipe.Subrange => RETURN "SUBRANGE";
    ELSE
      RETURN "??";
    END;
  END GetByTipe; 

PROCEDURE GetByDefn(defn : RT0.TypeDefn) : TEXT = 
  BEGIN
    IF defn = NIL THEN RETURN "**NULL**" END;

    VAR
      b := defn.name;
      s := LOOPHOLE(b, Ctypes.char_star);
    BEGIN
      IF LOOPHOLE(s,INTEGER) = 0 THEN
        RETURN GetByTipe(RTTipe.Get(defn.typecode, RTPacking.Local()));
      END;
      RETURN M3toC.StoT(s)
    END
  END GetByDefn; 

PROCEDURE GetByTC(c : RT0.Typecode) : TEXT = 
  BEGIN
    RETURN GetByDefn(RTType.Get(c));
  END GetByTC;

BEGIN
END RTName.
