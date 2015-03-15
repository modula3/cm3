MODULE Main;

IMPORT IO,Fmt,MySQL,MySQLMaps;

VAR
  conn : MySQL.T;
  tcon  : MySQL.T;
  res  : MySQL.ResT;
  field : MySQL.FieldT;
  m3field : MySQLMaps.M3FieldRef;
  m3fields : MySQLMaps.M3FieldArray;

  resi : INTEGER;

  host := "localhost";
  user := "user";
  passwd := "password";
  db := "test";

  numFields : INTEGER;
  numRows : LONGINT;
  fieldCount : CARDINAL;

  lengths : MySQL.RefLengthsT;

  row : REF ARRAY OF TEXT;

(* todo - Add a procedure to create a database drop database and other 
more advanced commands *)

PROCEDURE Test1() =
BEGIN
(*  assumes have created a db test and a table Test with appropriate columns *)

  TRY
    conn := MySQL.Init(NIL);
    tcon := MySQL.RealConnect(conn,host,user,passwd,db,0,NIL,0);

    resi := MySQL.Query(conn,"SELECT name,address,age FROM Test");
    res := MySQL.UseResult(conn);

    IO.Put("Result is OK\n");

    numFields := MySQL.NumFields(res);
    IO.Put("Num fields : " & Fmt.Int(numFields) & "\n");

    fieldCount := MySQL.FieldCount(conn);
    IO.Put("Field count : " & Fmt.Int(fieldCount) & "\n");

    numFields := MySQL.NumFields(res);
    IO.Put("Num fields : " & Fmt.Int(numFields) & "\n");

    field := MySQL.FetchFieldDirect(res,0);

    m3field := MySQLMaps.Field(field);
    IO.Put("0" & m3field.name);

    field := MySQL.FetchFields(res);
    m3fields := MySQLMaps.FieldList(res);

    IO.Put("1" & m3fields[0].name);
    IO.Put("2" & m3fields[1].name);

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

    numRows := MySQL.NumRows(res);
    IO.Put("Num rows : " & Fmt.LongInt(numRows) & "\n");

    res := MySQL.ListProcesses(conn);
    res := MySQL.ListDbs(conn,"%");
    res := MySQL.ListTables(conn,"%");

    IO.Put("ClientInfo : " & MySQL.GetClientInfo() & "\n");
    IO.Put("Server Version : " & Fmt.Int(MySQL.GetServerVersion(conn)) & "\n");
    IO.Put("Client Version : " & Fmt.Int(MySQL.GetClientVersion()) & "\n");
    IO.Put("Host Info : " & MySQL.GetHostInfo(conn) & "\n");


    MySQL.FreeResult(res);

  EXCEPT

  | MySQL.ConnE => 
      IO.Put("Conn Error: " & MySQL.Error(conn) & "\n");
  | MySQL.ResultE => 
      IO.Put("Result Error: num " & Fmt.Int(MySQL.Errno(conn)) & " " & MySQL.Error(conn) & "\n");
  | MySQL.ReturnE =>
      IO.Put("Return Error: num " & Fmt.Int(MySQL.Errno(conn)) & " " & MySQL.Error(conn) & "\n");
  END;

END Test1;

BEGIN

  Test1();

END Main.
