UNSAFE MODULE MySQLMaps;

IMPORT MySQL,MySQLRaw,M3toC;

TYPE
  M3RawArrField = UNTRACED BRANDED REF ARRAY OF MySQLRaw.MYSQL_FIELD;

PROCEDURE NewField(fieldRef : MySQLRaw.RefMysqlFieldT) : M3FieldRef =
VAR
  ret := NEW(M3FieldRef);
BEGIN
  ret.name             := M3toC.StoT(fieldRef.name);
  ret.org_name         := M3toC.StoT(fieldRef.org_name);
  ret.table            := M3toC.StoT(fieldRef.table);
  ret.org_table        := M3toC.StoT(fieldRef.org_table);
  ret.db               := M3toC.StoT(fieldRef.db);
  ret.catalog          := M3toC.StoT(fieldRef.catalog);
  ret.def              := M3toC.StoT(fieldRef.def);
  ret.length           := LOOPHOLE(fieldRef.length,LONGINT);
  ret.max_length       := LOOPHOLE(fieldRef.max_length,LONGINT);
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

PROCEDURE FieldList(fieldRef : MySQL.FieldT; numFields : CARDINAL) : M3FieldArray =
VAR
  ret : M3FieldArray;
  fields := LOOPHOLE(ADR(fieldRef),M3RawArrField);
BEGIN
  ret := NEW(M3FieldArray,numFields);
  FOR j := 0 TO numFields - 1 DO
    ret[j] := NewField(ADR(fields[j]));
  END;
  RETURN ret;
END FieldList;


BEGIN
END MySQLMaps.
