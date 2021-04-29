UNSAFE MODULE MySQLMaps;

IMPORT MySQL, MySQLRaw, Ctypes, M3toC;
IMPORT IO, Fmt;
(*IMPORT RTIO; debug*)

PROCEDURE NewString (s: Ctypes.char_star): TEXT =
  BEGIN
    IF s # NIL THEN RETURN M3toC.CopyStoT(s); ELSE RETURN NIL; END;
  END NewString;

(* debug <*UNUSED *>PROCEDURE DumpRaw(fieldRef : MySQLRaw.MYSQL_FIELD) =
   BEGIN RTIO.PutText("Raw Dump\n"); RTIO.PutString(fieldRef.name);
   RTIO.PutString(fieldRef.org_name); RTIO.PutString(fieldRef.table);
   RTIO.PutString(fieldRef.org_table); RTIO.PutString(fieldRef.db);
   RTIO.PutString(fieldRef.catalog); RTIO.PutString(fieldRef.def);

   RTIO.PutInt(fieldRef.length); RTIO.PutInt(fieldRef.max_length);

   RTIO.PutInt(fieldRef.name_length);
   RTIO.PutInt(fieldRef.org_name_length);

   RTIO.PutInt(fieldRef.table_length);
   RTIO.PutInt(fieldRef.org_table_length);

   RTIO.PutInt(fieldRef.db_length); RTIO.PutInt(fieldRef.catalog_length);

   RTIO.PutInt(fieldRef.def_length);

   RTIO.PutInt(fieldRef.flags); RTIO.PutInt(fieldRef.decimals);
   RTIO.PutInt(fieldRef.charsetnr); RTIO.PutInt(fieldRef.type);

   RTIO.Flush();

   RTIO.PutText("\nRaw Dump END\n"); END DumpRaw;

   PROCEDURE DumpSizes() = BEGIN IO.Put("Size INTEGER " &
   Fmt.Int(BYTESIZE(INTEGER)) & "\n"); IO.Put("Size LONGINT " &
   Fmt.Int(BYTESIZE(LONGINT)) & "\n"); IO.Put("Size C.unsgined_long " &
   Fmt.Int(BYTESIZE(Ctypes.unsigned_long)) & "\n"); IO.Put("Size
   C.unsgined_long_long " & Fmt.Int(BYTESIZE(Ctypes.unsigned_long_long)) &
   "\n"); IO.Put("Size C.int " & Fmt.Int(BYTESIZE(Ctypes.int)) & "\n"); END
   DumpSizes; *)

PROCEDURE DumpField (m3field: M3FieldRef) =
  BEGIN

    IO.Put(
      "Size M3Field " & Fmt.Int(BYTESIZE(MySQLRaw.MYSQL_FIELD_REC)) & "\n");
    WITH f = m3field DO
      IO.Put("name: " & f.name & "\n");
      IO.Put("org_name: " & f.org_name & "\n");
      IO.Put("table: " & f.table & "\n");
      IO.Put("org table: " & f.org_table & "\n");
      IO.Put("db: " & f.db & "\n");
      IO.Put("catalog: " & f.catalog & "\n");

      IF f.def # NIL THEN
        IO.Put("def: " & f.def & "\n");
      ELSE
        IO.Put("def(null)\n");
      END;

      IO.Put("length: " & Fmt.Int(f.length) & "\n");
      IO.Put("max length: " & Fmt.Int(f.max_length) & "\n");
      IO.Put("name length: " & Fmt.Int(f.name_length) & "\n");
      IO.Put("org name length: " & Fmt.Int(f.org_name_length) & "\n");
      IO.Put("table length: " & Fmt.Int(f.table_length) & "\n");
      IO.Put("org table length: " & Fmt.Int(f.org_table_length) & "\n");
      IO.Put("db length: " & Fmt.Int(f.db_length) & "\n");
      IO.Put("catalog length: " & Fmt.Int(f.catalog_length) & "\n");
      IO.Put("def length: " & Fmt.Int(f.def_length) & "\n");
      IO.Put("flags: " & Fmt.Int(f.flags) & "\n");
      IO.Put("decimals: " & Fmt.Int(f.decimals) & "\n");
      IO.Put("char set nr: " & Fmt.Int(f.charsetnr) & "\n");
      IO.Put("type: " & Fmt.Int(f.type) & "\n");
    END;

  END DumpField;

PROCEDURE NewField (fieldRef: MySQLRaw.MYSQL_FIELD): M3FieldRef =
  VAR ret := NEW(M3FieldRef);
  BEGIN
    ret.name := NewString(fieldRef.name);
    ret.org_name := NewString(fieldRef.org_name);
    ret.table := NewString(fieldRef.table);
    ret.org_table := NewString(fieldRef.org_table);
    ret.db := NewString(fieldRef.db);
    ret.catalog := NewString(fieldRef.catalog);
    ret.def := NewString(fieldRef.def);
    ret.length := fieldRef.length;
    ret.max_length := fieldRef.max_length;
    ret.name_length := fieldRef.name_length;
    ret.org_name_length := fieldRef.org_name_length;
    ret.table_length := fieldRef.table_length;
    ret.org_table_length := fieldRef.org_table_length;
    ret.db_length := fieldRef.db_length;
    ret.catalog_length := fieldRef.catalog_length;
    ret.def_length := fieldRef.def_length;
    ret.flags := fieldRef.flags;
    ret.decimals := fieldRef.decimals;
    ret.charsetnr := fieldRef.charsetnr;
    ret.type := fieldRef.type;
    RETURN ret;
  END NewField;

PROCEDURE Field (fieldRef: MySQL.FieldT): M3FieldRef =
  VAR field := LOOPHOLE(fieldRef, MySQLRaw.MYSQL_FIELD);
  BEGIN
    RETURN NewField(field);
  END Field;

PROCEDURE GetFieldList (fieldRef: MySQL.FieldT; numFields: CARDINAL):
  M3FieldArray =
  VAR
    ret     : M3FieldArray;
    rawField: MySQLRaw.MYSQL_FIELD;
  BEGIN
    rawField := LOOPHOLE(fieldRef, MySQLRaw.MYSQL_FIELD);
    ret := NEW(M3FieldArray, numFields);
    FOR j := 0 TO numFields - 1 DO
      ret[j] := NewField(rawField);
      INC(rawField, BYTESIZE(MySQLRaw.MYSQL_FIELD_REC));
    END;
    RETURN ret;
  END GetFieldList;

PROCEDURE FieldList (res: MySQL.ResultT): M3FieldArray =
  VAR
    fields   : MySQL.FieldT;
    numFields: CARDINAL;
  BEGIN
    numFields := MySQL.NumFields(res);
    IF numFields = 0 THEN RETURN NIL; END;
    fields := MySQL.FetchFields(res);
    IF fields = NIL THEN RETURN NIL; END;
    RETURN GetFieldList(fields, numFields);
  END FieldList;

BEGIN
END MySQLMaps.
