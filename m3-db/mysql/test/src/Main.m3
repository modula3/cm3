MODULE Main;

IMPORT IO,Fmt,Random,Text,MySQL,MySQLMaps;

VAR
  conn : MySQL.T;
  res  : MySQL.ResultT;
  field : MySQL.FieldT;
  m3field : MySQLMaps.M3FieldRef;
  m3fields : MySQLMaps.M3FieldArray;
  
  resi : INTEGER;

  (* set these to local configuration. You must have a user defined
  with sufficient privileges to create,drop databases, select,insert etc *)
  host := "localhost";
  user := "user";
  passwd := "password";
  db := "test";

  numFields,lastVal : INTEGER;
  numRows : LONGINT;
  fieldCount : CARDINAL;

  lengths : MySQL.RefLengthsT;
  sql : TEXT;
  
  row : REF ARRAY OF TEXT;
  r := NEW(Random.Default).init();

PROCEDURE Error() =
BEGIN
      IO.Put("Query: Errno: " & Fmt.Int(MySQL.Errno(conn)) & " errmsg: " & MySQL.Error(conn) & "\n");
END Error;

PROCEDURE Info() =
BEGIN
(* requires a connection *)
  IO.Put("ClientInfo : " & MySQL.GetClientInfo() & "\n");
  IO.Put("Server Version : " & Fmt.Int(MySQL.GetServerVersion(conn)) & "\n");
  IO.Put("Client Version : " & Fmt.Int(MySQL.GetClientVersion()) & "\n");
  IO.Put("Host Info : " & MySQL.GetHostInfo(conn) & "\n");
END Info;

PROCEDURE RandText() : TEXT =
VAR
  int : INTEGER;
  ch : CHAR;
  s : TEXT := "";
BEGIN
  FOR i := 1 TO 20 DO
    int := r.integer(65,90);
    ch := VAL(int,CHAR);
    s := s & Text.FromChar(ch);
  END;
  RETURN s;
END RandText;

PROCEDURE TestInit() =
BEGIN
  conn := MySQL.Init(NIL);
  IF conn = NIL THEN
    IO.Put("Cannot Init MySQL\n");
    RETURN;
  END;
END TestInit;

PROCEDURE TestConnection() =
BEGIN
  TRY
    EVAL MySQL.RealConnect(conn,host,user,passwd,db,0,NIL,0);
    IO.Put("CONN is OK\n");
  EXCEPT
  | MySQL.ConnE => Error(); RETURN;
  END;
END TestConnection;
  
PROCEDURE TestQueries() =
BEGIN

  TRY
    EVAL MySQL.Query(conn,"CREATE TABLE IF NOT EXISTS Temp(Id integer not null, Name varchar(20), Primary Key(Id))");
    IO.Put("CREATE TABLE Query OK\n");
  EXCEPT
  | MySQL.ReturnE(res) => Error();
  END;
  
  TRY
    EVAL MySQL.Query(conn,"DELETE FROM Temp");
    IO.Put("CREATE TABLE Query OK\n");
  EXCEPT
  | MySQL.ReturnE(res) => Error();
  END;
  
(* this is slow, multiple inserts. possibly sped up by setting auto-commit off 
  FOR i := 0 TO 10 DO
    TRY
      resi := MySQL.Query(conn,"INSERT INTO Temp(Id,Name) VALUES(" & Fmt.Int(i) & ",\"peter\")");
    EXCEPT
    | MySQL.ReturnE(res) => Error(); RETURN;
    END;    
  END;
  IO.Put("INSERT Query OK\n");
*)
  lastVal := 15;
  sql := "INSERT INTO Temp(Id,Name) VALUES ";
  FOR i := 1 TO lastVal DO
    sql := sql & "(" & Fmt.Int(i) & ",\"" & RandText() & "\")";
    IF i < lastVal THEN
      sql := sql & ",";
    END;
  END;
  (*
  IO.Put("sql-" & sql);
  *)
  
  TRY
    resi := MySQL.Query(conn,sql);
  EXCEPT
  | MySQL.ReturnE(res) => Error(); RETURN;
  END;    
  IO.Put("INSERT Query OK\n");
  
  TRY
    EVAL MySQL.Query(conn,"SELECT id,name FROM Temp");
    res := MySQL.UseResult(conn);
  EXCEPT
  | MySQL.ResultE => Error(); RETURN;
  | MySQL.ReturnE(res) => Error(); RETURN;
  END;    
  IO.Put("SELECT Query OK\n");

  numFields := MySQL.NumFields(res);
  IO.Put("Num fields : " & Fmt.Int(numFields) & "\n");

  fieldCount := MySQL.FieldCount(conn);
  IO.Put("Field count : " & Fmt.Int(fieldCount) & "\n");

  (* test getting fields individually *)
  FOR i := 0 TO numFields - 1 DO
    field := MySQL.FetchFieldDirect(res,i);
    m3field := MySQLMaps.Field(field);
    MySQLMaps.DumpField(m3field);
  END;
  
  (* test getting fields all at once *)
  (* Instead of this call use FieldList
  field := MySQL.FetchFields(res);  
  *)
  m3fields := MySQLMaps.FieldList(res);
  
  FOR i := 0 TO numFields - 1 DO
    MySQLMaps.DumpField(m3fields[i]);  
  END;
  
  row := MySQL.FetchRow(res);
  WHILE row # NIL DO
    lengths := MySQL.FetchLengths(res);
    FOR i := 0 TO numFields - 1 DO
(*
      IO.Put("Len:" & Fmt.Int(lengths[i]));
*)
      m3field := MySQLMaps.Field(MySQL.FetchFieldDirect(res,i));
      IF row[i] = NIL THEN
        IO.Put(">>NULL<<")
      ELSE
        IO.Put(m3field.name & " " & row[i] & " : ");
      END;
    END;
    IO.Put("\n");
    row := MySQL.FetchRow(res);
  END;

  numRows := MySQL.NumRows(res);
  IO.Put("Num rows : " & Fmt.LongInt(numRows) & "\n");
  IO.Put("End of select query\n");
    
  TRY
    EVAL MySQL.Query(conn,"DROP TABLE Temp");
  EXCEPT
  | MySQL.ReturnE(res) => Error(); RETURN;
  END;      
  IO.Put("DROP Query OK\n");
    
END TestQueries;

PROCEDURE ListProcessTest() =
BEGIN
  IO.Put("ListProcessesTest\n");

  res := MySQL.ListProcesses(conn);

  numFields := MySQL.NumFields(res);
  IO.Put("Num fields : " & Fmt.Int(numFields) & "\n");

  row := MySQL.FetchRow(res);
  WHILE row # NIL DO
    lengths := MySQL.FetchLengths(res);
    FOR i := 0 TO numFields - 1 DO

      IO.Put("Len:" & Fmt.Int(lengths[i]));
      IF row[i] = NIL THEN
        IO.Put(" NULL")
      ELSE
        IO.Put(" : " & row[i]);
      END;

    END;
    IO.Put("\n");
    row := MySQL.FetchRow(res);
  END;
  MySQL.FreeResult(res);
END ListProcessTest;

PROCEDURE ListDbsTest() =
BEGIN
  IO.Put("ListDbsTest\n");

  res := MySQL.ListDbs(conn,"%");

  numFields := MySQL.NumFields(res);
  IO.Put("Num fields : " & Fmt.Int(numFields) & "\n");

  row := MySQL.FetchRow(res);
  WHILE row # NIL DO
    lengths := MySQL.FetchLengths(res);
    FOR i := 0 TO numFields - 1 DO

      IO.Put("Len:" & Fmt.Int(lengths[i]));
      IF row[i] = NIL THEN
        IO.Put(" NULL")
      ELSE
        IO.Put(" : " & row[i]);
      END;

    END;
    IO.Put("\n");
    row := MySQL.FetchRow(res);
  END;
  MySQL.FreeResult(res);
END ListDbsTest;

PROCEDURE ListTablesTest() =
BEGIN
  IO.Put("ListTablesTest\n");

  res := MySQL.ListTables(conn,"%");

  numFields := MySQL.NumFields(res);
  IO.Put("Num fields : " & Fmt.Int(numFields) & "\n");

  row := MySQL.FetchRow(res);
  WHILE row # NIL DO
    lengths := MySQL.FetchLengths(res);
    FOR i := 0 TO numFields - 1 DO

      IO.Put("Len:" & Fmt.Int(lengths[i]));
      IF row[i] = NIL THEN
        IO.Put(" NULL")
      ELSE
        IO.Put(" : " & row[i]);
      END;

    END;
    IO.Put("\n");
    row := MySQL.FetchRow(res);
  END;
  MySQL.FreeResult(res);
END ListTablesTest;

BEGIN
  TestInit();
  TestConnection();
  
  Info();

  TestQueries();
  
  ListProcessTest();
  ListDbsTest();
  ListTablesTest();

END Main.
