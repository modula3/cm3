MODULE Main;

IMPORT IO, Fmt, Thread, Random, Text, MySQL;

EXCEPTION E(TEXT);

VAR
  conn : MySQL.T;
  res  : MySQL.ResultT;
  field: MySQL.FieldT;
  fields: MySQL.RefFieldArray;

  keyId: INTEGER;

  (* set these to your local configuration.  You must have a user defined with
     sufficient privileges to create/drop databases, create/drop tables,
     select,insert,update,delete etc *)
  host   := "localhost";
  user   := "user";
  passwd := "password";
  db     := "M3TestDB";

  numFields, lastVal: INTEGER;
  numRows           : LONGINT;
  fieldCount        : CARDINAL;

  lengths: MySQL.RefLengthsT;
  sql    : TEXT;

  row: REF ARRAY OF TEXT;
  rnd := NEW(Random.Default).init();

PROCEDURE Error (msg : TEXT; err : INTEGER := 0) =
  BEGIN
    IO.Put(msg & "\n");
    IO.Put("Errno: " & Fmt.Int(err) & " " & Fmt.Int(MySQL.Errno(conn)) & 
           "\nErrmsg: " & MySQL.Error(conn) & "\n");
  END Error;

PROCEDURE TestInfo () =
  VAR 
    hex : TEXT := "                "; (* must be 2*len(from)+1 bytes *)
    from : TEXT := "1234";
    sock : MySQL.Int32;
  BEGIN
    (* requires a connection *)
    IO.Put("ClientInfo : " & MySQL.GetClientInfo() & "\n");
    IO.Put(
      "Server Version : " & Fmt.Int(MySQL.GetServerVersion(conn)) & "\n");
    IO.Put("Server Name : " & MySQL.GetServerName(conn) & "\n");
    IO.Put("Client Version : " & Fmt.Int(MySQL.GetClientVersion()) & "\n");
    IO.Put("Host Info : " & MySQL.GetHostInfo(conn) & "\n");
    IO.Put("Protocol : " & Fmt.Int(MySQL.GetProtoInfo(conn)) & "\n");
    IO.Put("Status : " & MySQL.Stat(conn) & "\n");
    IO.Put("Thread Id : " & Fmt.Int(MySQL.ThreadId(conn)) & "\n");
    IO.Put("Thread Safe : " & Fmt.Int(MySQL.ThreadSafe()) & "\n");
    IO.Put("Warning Count : " & Fmt.Int(MySQL.WarningCount(conn)) & "\n");
(* FIXME HexString needs work *)
    IO.Put("Hex String : " & Fmt.Int(MySQL.HexString(hex,from,Text.Length(from))) & " " &  hex & "\n");
    IO.Put("Timeout Value : " & Fmt.Int(MySQL.GetTimeoutValue(conn)) & "\n");
    IO.Put("Timeout Value Ms: " & Fmt.Int(MySQL.GetTimeoutValueMs(conn)) & "\n");
    IO.Put("Charset Name: " & MySQL.CharacterSetName(conn) & "\n");
    IO.Put("SQL state : " & MySQL.Sqlstate(conn) & "\n");

    sock := MySQL.GetSocket(conn);
    IF sock = 0 THEN IO.Put("Cannot get socket\n"); END;
    IO.Put("Socket : " & Fmt.Int(sock) & "\n");
  END TestInfo;

PROCEDURE RandText (): TEXT =
  VAR
    int: INTEGER;
    ch : CHAR;
    s  : TEXT    := "";
  BEGIN
    FOR i := 1 TO 20 DO
      int := rnd.integer(65, 90);
      ch := VAL(int, CHAR);
      s := s & Text.FromChar(ch);
    END;
    RETURN s;
  END RandText;

PROCEDURE TestInit () RAISES{E} =
  BEGIN
    conn := MySQL.Init(NIL);
    IF conn = NIL THEN RAISE E("Cannot Init MySQL"); END;
    IO.Put("MySQL Init is OK\n");
  END TestInit;

PROCEDURE TestConnection () RAISES {E} =
  VAR connDB : TEXT := NIL;
  BEGIN
    conn := MySQL.RealConnect(conn, host, user, passwd, connDB, 0, NIL, 0);
    IF conn = NIL THEN RAISE E("Cannot connect to MySQL"); END;
    IO.Put("MySQL Connection is OK\n");
  END TestConnection;

PROCEDURE TestSelectDb () RAISES{E} =
  VAR ret : INTEGER;
  BEGIN
    sql := "CREATE DATABASE " & db;
    ret := MySQL.Query(conn, sql);
    IF ret # 0 THEN
       IO.Put("Database " & db & " already exists\n");
    ELSE
      IO.Put("CREATE DATABASE Query OK\n");
    END;

    (* test select unknown db *)
    ret := MySQL.SelectDb(conn, "nonexistant db");
    IF ret # 0 THEN 
      IO.Put("Cannot select DB\n");
    ELSE
      IO.Put("MySQL select db is OK\n");
    END;
    (* select our test db *)
    ret := MySQL.SelectDb(conn, db);
    IF ret # 0 THEN RAISE E("Cannot select DB"); END;
    IO.Put("MySQL select db is OK\n");
  END TestSelectDb;

PROCEDURE TestQueries () RAISES {E} =
  VAR ret : INTEGER;
  BEGIN
    (* create a temp table to practise *)
    sql := "CREATE TABLE IF NOT EXISTS Temp(Id integer not null, Name varchar(20), Primary Key(Id))";
    ret := MySQL.Query(conn, sql);
    IF ret # 0 THEN RAISE E("Create Table query NOT OK"); END;
    IO.Put("CREATE TABLE Query OK\n");

    sql := "DELETE FROM Temp";
    ret := MySQL.Query(conn, sql);
    IF ret # 0 THEN IO.Put("Delete all rows query NOT OK\n");
    ELSE
      IO.Put("DELETE all rows Query OK\n");
    END;

    (* this is slow using multiple inserts.  possibly sped up by setting
       auto-commit off *)
    keyId := 0;
    FOR i := 0 TO 10 DO
      sql := "INSERT INTO Temp(Id,Name) VALUES(" & Fmt.Int(keyId)
               & ",\"random text\")";
      ret := MySQL.Query(conn, sql);
      IF ret # 0 THEN RAISE E("Insert Table query NOT OK"); END;
      IO.Put(sql & "\nINSERT Query OK\n");
      INC(keyId);
    END;

    (* faster - one insert stmt *)
    lastVal := 15;
    sql := "INSERT INTO Temp(Id,Name) VALUES ";
    FOR i := 1 TO lastVal DO
      sql := sql & "(" & Fmt.Int(keyId) & ",\"" & RandText() & "\")";
      INC(keyId);
      IF i < lastVal THEN sql := sql & ","; END;
    END;

    IO.Put("Trying \n" & sql & "\n");
    ret := MySQL.Query(conn, sql);
    IF ret # 0 THEN RAISE E("Insert Table query NOT OK"); END;
    IO.Put("INSERT Query OK\n");

    (* select query test *)
    sql := "SELECT id,name FROM Temp WHERE id > 1 ORDER BY id DESC";
    ret := MySQL.Query(conn, sql);
    IF ret # 0 THEN RAISE E("Select query NOT OK"); END;
    res := MySQL.UseResult(conn);
    IF res = NIL THEN RAISE E("Use Result NOT OK"); END;
    IO.Put("SELECT Query OK\n");

    numRows := MySQL.NumRows(res);
    IO.Put("Num rows : " & Fmt.LongInt(numRows) & "\n");

    numFields := MySQL.NumFields(res);
    IO.Put("Num fields : " & Fmt.Int(numFields) & "\n");

    fieldCount := MySQL.FieldCount(conn);
    IO.Put("Field count : " & Fmt.Int(fieldCount) & "\n");

    (* test getting fields individually *)
    IO.Put("Field names direct\n");
    FOR i := 0 TO numFields - 1 DO
      field := MySQL.FetchFieldDirect(res, i);
      IO.Put("fieldname " & field.name & "\n");
      IO.Put("fieldtype "); IO.PutInt(field.type); IO.Put("\n");
    END;

    (* test getting fields all together *)
    IO.Put("Field names block\n");
    fields := MySQL.FetchFields(res);
    FOR i := 0 TO numFields - 1 DO
      IO.Put("fieldname " & fields[i].name & "\n");
      IO.Put("fieldtype "); IO.PutInt(fields[i].type); IO.Put("\n");
    END;
    IO.Put("End field name\n");

    (* display field lengths - have to fetch a row first *)
    row := MySQL.FetchRow(res);

    IO.Put("Field lengths\n");
    lengths := MySQL.FetchLengths(res);
    FOR i := 0 TO numFields - 1 DO
      IO.Put("Len:" & Fmt.Int(lengths[i]) & "\n");
    END;

    (* display rows *)
    WHILE row # NIL DO
      FOR i := 0 TO numFields - 1 DO
        field := MySQL.FetchFieldDirect(res, i);
        IF row[i] = NIL THEN
          IO.Put(" NULL ")
        ELSE
          IO.Put(field.name & " " & row[i] & " : ");
        END;
      END;
      IO.Put("\n");
      row := MySQL.FetchRow(res);
    END;

    numRows := MySQL.NumRows(res);
    IO.Put("Num rows : " & Fmt.LongInt(numRows) & "\n");
    IO.Put("End select query\n");

    (* free mem for select *)
    MySQL.FreeResult(res);

    (* update query test *)
    sql := "UPDATE Temp SET name = \"NONAME\" WHERE id = 3";
    ret := MySQL.Query(conn, sql);
    IF ret # 0 THEN RAISE E("Update query NOT OK"); END;
    IO.Put("UPDATE Query OK\n");
    numRows := MySQL.AffectedRows(conn);
    IO.Put("Updated rows : " & Fmt.LongInt(numRows) & "\n");

    (* delete query test *)
    sql := "DELETE FROM Temp WHERE id > 3 AND id < 7";
    ret := MySQL.Query(conn, sql);
    IF ret # 0 THEN RAISE E("Delete query NOT OK"); END;
    IO.Put("DELETE Query OK\n");
    numRows := MySQL.AffectedRows(conn);
    IO.Put("Deleted rows : " & Fmt.LongInt(numRows) & "\n");

    sql := "DROP TABLE Temp";
    ret := MySQL.Query(conn, sql);
    IF ret # 0 THEN RAISE E("Drop table query NOT OK"); END;
    IO.Put("DROP TABLE Query OK\n");

  END TestQueries;

PROCEDURE TestNonBlockingQueries() RAISES {E}=
  VAR
    ret,status : INTEGER;
    err : MySQL.Int32;
  BEGIN
    ret := MySQL.Options(conn, MySQL.MYSQL_OPT_NONBLOCK, NIL);
    IF ret # 0 THEN RAISE E("Get Options NOT OK"); END;

    sql := "SHOW STATUS";
    status := MySQL.QueryStart(err, conn, sql);
    WHILE status # 0 DO
      (* should poll the events on the socket but just pause for now *)
      Thread.Pause(0.1D0);
      status := MySQL.QueryCont(err, conn, status);
    END;
    IF err > 0 THEN Error("Status query error"); END;
    IO.Put("STATUS Query OK\n");
  END TestNonBlockingQueries;

PROCEDURE DisplayResults() =
  BEGIN
    numFields := MySQL.NumFields(res);
    IO.Put("Num fields : " & Fmt.Int(numFields) & "\n");

    row := MySQL.FetchRow(res);
    WHILE row # NIL DO
      FOR i := 0 TO numFields - 1 DO
        IF row[i] = NIL THEN
          IO.Put(" NULL")
        ELSE
          IO.Put(" : " & row[i]);
        END;

      END;
      IO.Put("\n");
      row := MySQL.FetchRow(res);
    END;
  END DisplayResults;

PROCEDURE ListProcessTest () RAISES {E} =
  BEGIN
    IO.Put("ListProcessesTest\n");
    res := MySQL.ListProcesses(conn);
    IF res # NIL THEN DisplayResults(); ELSE RAISE E("List Processes error"); END;
    MySQL.FreeResult(res);
  END ListProcessTest;

PROCEDURE ListDbsTest () RAISES {E} =
  BEGIN
    IO.Put("ListDbsTest\n");
    res := MySQL.ListDbs(conn, "%");
    IF res # NIL THEN DisplayResults(); ELSE RAISE E("List DBs error"); END;
    MySQL.FreeResult(res);
  END ListDbsTest;

PROCEDURE ListTablesTest () RAISES {E} =
  BEGIN
    IO.Put("ListTablesTest\n");
    res := MySQL.ListTables(conn, "%");
    IF res # NIL THEN DisplayResults(); ELSE RAISE E("List Tables error"); END;
    MySQL.FreeResult(res);
  END ListTablesTest;

PROCEDURE ShowStatusTest () RAISES {E} =
  VAR ret : INTEGER;
  BEGIN
    IO.Put("ShowStatusTest\n");
    sql := "Show Status";
    ret := MySQL.Query(conn, sql);
    IF ret # 0 THEN RAISE E("Show Status query NOT OK"); END;
    res := MySQL.UseResult(conn);
    IF res # NIL THEN DisplayResults(); ELSE RAISE E("Use result error"); END;
    IO.Put("Show Status Query OK\n");
    MySQL.FreeResult(res);
  END ShowStatusTest;

PROCEDURE TestClose () =
  BEGIN
    IO.Put("TestClose\n");
    MySQL.Close(conn);
    IO.Put("Connection closed\n");
  END TestClose;

BEGIN
  TRY
    TestInit();
    TestConnection();
    TestSelectDb();
    TestInfo();
    ListProcessTest();
    ListDbsTest();
    ListTablesTest();
    ShowStatusTest();
    TestQueries();
    TestNonBlockingQueries();
  EXCEPT
  | E(msg) => Error(msg);
  END;
  TestClose();
END Main.
