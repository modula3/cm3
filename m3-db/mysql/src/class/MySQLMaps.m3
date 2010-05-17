UNSAFE MODULE MySQLMaps;

IMPORT MySQL,MySQLRaw,Ctypes,M3toC;
IMPORT RTIO,IO,Fmt;

PROCEDURE NewString(s : Ctypes.char_star) : TEXT =
BEGIN
  IF s # NIL THEN RETURN M3toC.CopyStoT(s); ELSE RETURN NIL; END;
END NewString;

<*UNUSED *>PROCEDURE DumpRaw(fieldRef : MySQLRaw.RefMysqlFieldT) =
BEGIN
  RTIO.PutText("Raw Dump\n");
  RTIO.PutString(fieldRef.name);
  RTIO.PutString(fieldRef.org_name);
  RTIO.PutString(fieldRef.table);
  RTIO.PutString(fieldRef.org_table);
  RTIO.PutString(fieldRef.db);
  RTIO.PutString(fieldRef.catalog);
  RTIO.PutString(fieldRef.def);

  RTIO.PutInt(fieldRef.length);
  RTIO.PutInt(fieldRef.max_length);

  RTIO.PutInt(fieldRef.name_length);
  RTIO.PutInt(fieldRef.org_name_length);

  RTIO.PutInt(fieldRef.table_length);
  RTIO.PutInt(fieldRef.org_table_length);

  RTIO.PutInt(fieldRef.db_length);
  RTIO.PutInt(fieldRef.catalog_length);

  RTIO.PutInt(fieldRef.def_length);

  RTIO.PutInt(fieldRef.flags);
  RTIO.PutInt(fieldRef.decimals);
  RTIO.PutInt(fieldRef.charsetnr);
  RTIO.PutInt(fieldRef.type);

  RTIO.Flush();

  RTIO.PutText("\nRaw Dump END\n");
END DumpRaw;

PROCEDURE DumpField(m3field : M3FieldRef) =
BEGIN
(* fixme to handle null strings like def*)
  IO.Put("Size M3Field " & Fmt.Int(BYTESIZE(MySQLRaw.MYSQL_FIELD)) & "\n");
  WITH f = m3field DO
   IO.Put("name: " & f.name & "\n");
   IO.Put("org_name: " & f.org_name & "\n");
   IO.Put("table: " & f.table & "\n");
   IO.Put("org table: " & f.org_table & "\n");
   IO.Put("db: " & f.db & "\n");
   IO.Put("catalog: " & f.catalog & "\n");

   IF f.def # NIL THEN IO.Put("def: " & f.def & "\n"); ELSE IO.Put("def(null)\n"); END;

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

PROCEDURE NewField(fieldRef : MySQLRaw.RefMysqlFieldT) : M3FieldRef =
VAR
  ret := NEW(M3FieldRef);
BEGIN
  ret.name             := NewString(fieldRef.name);
  ret.org_name         := NewString(fieldRef.org_name);
  ret.table            := NewString(fieldRef.table);
  ret.org_table        := NewString(fieldRef.org_table);
  ret.db               := NewString(fieldRef.db);
  ret.catalog          := NewString(fieldRef.catalog);
  ret.def              := NewString(fieldRef.def);
  ret.length           := fieldRef.length;
  ret.max_length       := fieldRef.max_length;
  ret.name_length      := fieldRef.name_length ;
  ret.org_name_length  := fieldRef.org_name_length;
  ret.table_length     := fieldRef.table_length;
  ret.org_table_length := fieldRef.org_table_length;
  ret.db_length        := fieldRef.db_length;
  ret.catalog_length   := fieldRef.catalog_length;
  ret.def_length       := fieldRef.def_length;
  ret.flags            := fieldRef.flags;
  ret.decimals         := fieldRef.decimals;
  ret.charsetnr        := fieldRef.charsetnr;
  ret.type             := fieldRef.type;
  RETURN ret;
END NewField;


PROCEDURE Field(fieldRef : MySQL.FieldT) : M3FieldRef =
VAR
  field := LOOPHOLE(fieldRef,MySQLRaw.RefMysqlFieldT);
BEGIN
  RETURN NewField(field);
END Field;

(*
Old version
  better to pass a res handle which also has the field pointer
  and the field_count for safety so dont have to pass numFields
  as in FieldLists below

TYPE

  RefArrMysqlFieldT = UNTRACED BRANDED REF ARRAY OF MySQLRaw.MYSQL_FIELD;

PROCEDURE FieldList(fieldRef : MySQL.FieldT; numFields : CARDINAL) : M3FieldArray =
VAR
  ret : M3FieldArray;
  rawField : MySQLRaw.RefMysqlFieldT;
  fields := LOOPHOLE(ADR(fieldRef),RefArrMysqlFieldT);
BEGIN
  ret := NEW(M3FieldArray,numFields);
  FOR j := 0 TO numFields - 1 DO
    rawField := ADR(fields[j]);
    ret[j] := NewField(rawField);
  END;
  RETURN ret;
END FieldList;
*)

PROCEDURE FieldList(res : MySQL.ResT) : M3FieldArray =
VAR
  numFields : INTEGER;
  result : M3FieldArray;
  rawField : MySQLRaw.RefMysqlFieldT;
  rawRes : MySQLRaw.RefMysqlResT;
BEGIN
  IF res = NIL THEN RETURN NIL; END;
  rawRes := LOOPHOLE(res,MySQLRaw.RefMysqlResT);
  numFields := rawRes.field_count;
  result := NEW(M3FieldArray,numFields);
  rawField := rawRes.fields;
  FOR j := 0 TO numFields - 1 DO
    result[j] := NewField(rawField);
    INC(rawField,BYTESIZE(MySQLRaw.MYSQL_FIELD));
  END;
  RETURN result;
END FieldList;

BEGIN
END MySQLMaps.
