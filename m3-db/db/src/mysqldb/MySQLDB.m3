MODULE MySQLDB EXPORTS MySQLDB;

IMPORT ASCII,
       WeakRef,
       Lex,
       Scan,
       FloatMode,
       Fmt,
       TextRd,
       Rd,
       Thread,
       FmtTime,
       Text,
       IO,
       DB,
       MySQL;

FROM Text IMPORT Sub, FindChar, Length, GetChar;

FROM DB IMPORT Results, ResultDesc, DescList,
               DataType, RefDate, RefTime,
               RefTimestamp, ErrorDesc;


(*------------------------------------------------------- interfaces ---*)

CONST Debug = TRUE;

TYPE
  Interface = DB.Interface OBJECT
              OVERRIDES
                connect          := Connect;
                get_data_sources := GetDataSources;
                get_drivers      := GetDrivers;
              END;


PROCEDURE GetInterface (): DB.Interface =
  BEGIN
    RETURN NEW(Interface, name := "MySQL");
  END GetInterface;


PROCEDURE Connect (<* UNUSED *> interface        : Interface;
                                database         : TEXT;
                                user_id, password: TEXT;
                                server           : TEXT        := NIL):
  DB.T RAISES {DB.Error} =
  VAR t := NEW(T);
  BEGIN
    t.hdbc := MySQL.RealConnect(MySQL.Init(NIL), server, user_id,
                                password, database, 3306, NIL, 0);
    IF t.hdbc = NIL THEN
      CheckErr(t);
    END;
    EVAL WeakRef.FromRef(t, CleanupConnection);
    RETURN t;
  END Connect;

PROCEDURE GetDataSources (<* UNUSED *> this: Interface): DescList =
  BEGIN
    Unimplemented("GetDataSources");
    RETURN NIL;
  END GetDataSources;

PROCEDURE GetDrivers (<* UNUSED *> this: Interface): DescList =
  BEGIN
    Unimplemented("GetDrivers");
    RETURN NIL;
  END GetDrivers;

PROCEDURE CleanupConnection
  (<* UNUSED *> READONLY w: WeakRef.T; ref: REFANY) =
  VAR t := NARROW(ref, T);
  BEGIN
    IF Debug THEN IO.Put("Cleaning a connection\n") END;
    IF (t.hdbc # NIL) THEN MySQL.Close(t.hdbc); t.hdbc := NIL; END;
  END CleanupConnection;

(*------------------------------------------------------- connections ---*)

TYPE
  T = DB.T OBJECT
        hdbc          : MySQL.T;
        auto_commit_on: BOOLEAN   := FALSE;
        used          : BOOLEAN   := FALSE;
      OVERRIDES
        disconnect  := Disconnect;
        new_stmt    := NewStmt;
        auto_commit := AutoCommit;
        commit      := Commit;
        abort       := Abort;
      END;


PROCEDURE Disconnect (t: T) RAISES {DB.Error} =
  BEGIN
    IF (t.hdbc = NIL) THEN Die(1, "DB.T is already disconnected."); END;
    MySQL.Close(t.hdbc);
    CheckErr(t);
    t.hdbc := NIL;
  END Disconnect;

PROCEDURE AutoCommit (t: T; on: BOOLEAN) RAISES {DB.Error} =
  BEGIN
    IF (t.hdbc = NIL) THEN
      Die(2, "Attempted to set AutoCommit on a disconnected DB.T.");
    END;
    SQL(t, "SET AUTOCOMMIT = " & ARRAY BOOLEAN OF TEXT{"0", "1"}[on]);
    t.auto_commit_on := on;
  END AutoCommit;

PROCEDURE Commit (t: T) RAISES {DB.Error} =
  BEGIN
    IF (t.hdbc = NIL) THEN
      Die(3, "Attempted to commit a disconnected DB.T.");
    END;
    SQL(t, "COMMIT");
  END Commit;

PROCEDURE Abort (t: T) RAISES {DB.Error} =
  BEGIN
    IF (t.hdbc = NIL) THEN
      Die(4, "Attempted to abort a disconnected DB.T.");
    END;
    SQL(t, "ROLLBACK");
  END Abort;

PROCEDURE NewStmt (t: T): DB.Stmt RAISES {DB.Error} =
  VAR st := NEW(Stmt);
  BEGIN
    IF (t.hdbc = NIL) THEN
      Die(5, "Attempted to create a new statement on a disconnected DB.T.");
    END;

    st.conn := t;
    st.hstmt := NIL;
    st.prepared := FALSE;
    st.executed := FALSE;

    SQL(t, "BEGIN");
    EVAL WeakRef.FromRef(st, CleanupStmt);

    RETURN st;
  END NewStmt;

PROCEDURE CleanupStmt (<* UNUSED *> READONLY wr: WeakRef.T; ref: REFANY) =
  VAR st := NARROW(ref, Stmt);
  BEGIN
    IF (st.hstmt # NIL) THEN
      IF (st.conn # NIL) AND (st.conn.hdbc # NIL) THEN
        (* Q: Notify database that the statement is deallocated. *)
      END;
      st.conn := NIL;
      st.hstmt := NIL;
      st.prepared := FALSE;
      st.executed := FALSE;
    END;
  END CleanupStmt;

(*-------------------------------------------------------- statements ---*)

TYPE
  (* a SQL database statement (query or update) *)
  Stmt = DB.Stmt OBJECT
           conn    : T;           (* my database connection *)
           hstmt   : TEXT         := NIL;
           prepared: BOOLEAN;
           executed: BOOLEAN;
           col_info: ResultDesc;
           values  : Results;
           current_row: INTEGER;
           fetchable  : BOOLEAN;
           rows       : INTEGER;
           cursor_name: TEXT       := "myportal";
           result     : MySQL.ResultT := NIL;
         OVERRIDES
           prepare         := Prepare;
           execute         := Execute;
           fetch           := Fetch;
           done            := Done;
           close           := Close;
           get_cursor_name := GetCursorName;
           set_cursor_name := SetCursorName;
           num_rows        := NumRows;
           describe_result := DescribeResult;
           connection      := StmtConnection;
         END;

PROCEDURE StmtConnection (st: Stmt): DB.T =
  BEGIN
    RETURN st.conn;
  END StmtConnection;

PROCEDURE Prepare (st: Stmt; operation: TEXT) RAISES {DB.Error} =
  BEGIN
    LOCK st DO
      st.hstmt := operation;
      CheckStmt(st, 12, "prepare", check_exec := FALSE);
      st.prepared := TRUE;
      st.executed := FALSE;
      st.col_info := NIL;
      st.values := NIL;
    END
  END Prepare;

PROCEDURE Execute (st: Stmt; operation: TEXT) RAISES {DB.Error} =
  BEGIN
    LOCK st DO
      CheckStmt(st, 15, "execute", check_exec := FALSE);
      IF (operation = NIL) THEN
        IF st.hstmt = NIL THEN
          Die(6, "Attempted to execute a closed DB.Stmt");
        ELSE
          operation := st.hstmt;
        END
      ELSE
        IF st.hstmt = NIL THEN
          st.hstmt := operation;
        ELSE
          (* <*ASSERT Equal (st.hstmt, operation)*> *)
        END;
      END;
      st.executed := TRUE;
      st.col_info := NIL;
      st.values := NIL;
      st.current_row := 0;

      st.fetchable := Fetchable(st);

      IF NOT st.fetchable THEN
        SQL(st.conn, st.hstmt);
        st.rows := 0;
      ELSE
        SQL(st.conn, st.hstmt);
        st.result := MySQL.UseResult(st.conn.hdbc);
        IF st.result = NIL THEN
          CheckErr(st.conn);
        END;
        st.rows := 0;
      END;
    END;
  END Execute;

PROCEDURE Fetchable (st: Stmt): BOOLEAN =

  PROCEDURE CaseInsensitiveEqual (a, b: TEXT): BOOLEAN =
    BEGIN
      IF Length(a) # Length(b) THEN RETURN FALSE END;
      FOR i := 0 TO Length(a) - 1 DO
        IF ASCII.Upper[GetChar(a, i)] # ASCII.Upper[GetChar(b, i)] THEN
          RETURN FALSE;
        END
      END;
      RETURN TRUE;
    END CaseInsensitiveEqual;

  CONST fetchable_commands = ARRAY OF TEXT{"select"};
  VAR
    cmd  : TEXT;
    index       := FindChar(st.hstmt, ' ');
  BEGIN
    (* One-word commands are not fetchable *)
    IF index = -1 THEN RETURN FALSE END;

    (* Search for the command in the fetchable_commands array. *)
    cmd := Sub(st.hstmt, 0, index);
    FOR i := FIRST(fetchable_commands) TO LAST(fetchable_commands) DO
      IF CaseInsensitiveEqual(fetchable_commands[i], cmd) THEN
        RETURN TRUE;
      END
    END;
    RETURN FALSE;
  END Fetchable;

PROCEDURE Fetch (st: Stmt): Results RAISES {DB.Error} =
  BEGIN
    LOCK st DO
      CheckStmt(st, 18, "fetch from", check_exec := TRUE);
      IF NOT st.fetchable THEN RETURN NIL END;
      IF st.values = NIL THEN BuildValueArea(st) END;
      TRY
        MapValues(st);
        CheckErr(st.conn);
      FINALLY
        INC(st.current_row);
      END;
    END;
    RETURN st.values;
  END Fetch;

PROCEDURE BuildValueArea (st: Stmt) RAISES {DB.Error} =
  (* LL = st.mu *)
  BEGIN                          (* BuildValueArea *)
    IF (st.col_info = NIL) THEN BuildColumnInfo(st); END;
    st.values := NEW(Results, NUMBER(st.col_info^));
  END BuildValueArea;

PROCEDURE MapValues (st: Stmt) RAISES {DB.Error} =

  PROCEDURE BuildTime (valtext: TEXT): RefTime
    RAISES {Lex.Error, Thread.Alerted, Rd.Failure, FloatMode.Trap} =
    VAR
      time := NEW(RefTime);
      rd   := TextRd.New(valtext);
    BEGIN
      time.hour := Lex.Int(rd);
      Lex.Match(rd, ":");
      time.minute := Lex.Int(rd);
      Lex.Match(rd, ":");
      time.second := Lex.Int(rd);
      RETURN time;
      (* Ignore everything after the seconds *)
    END BuildTime;

  PROCEDURE BuildDate (valtext: TEXT): RefDate
    RAISES {Lex.Error, Thread.Alerted, Rd.Failure, FloatMode.Trap} =
    VAR
      date := NEW(RefDate);
      rd   := TextRd.New(valtext);
    BEGIN
      date.month := Lex.Int(rd);
      Lex.Match(rd, "-");
      date.day := Lex.Int(rd);
      Lex.Match(rd, "-");
      date.year := Lex.Int(rd);
      RETURN date;
    END BuildDate;

  PROCEDURE BuildTimestamp (valtext: TEXT): RefTimestamp
    RAISES {Lex.Error, Thread.Alerted, Rd.Failure, FloatMode.Trap} =

    PROCEDURE Month (t: TEXT): CARDINAL RAISES {Lex.Error} =
      BEGIN
        FOR i := FIRST(FmtTime.Month) TO LAST(FmtTime.Month) DO
          IF Text.Equal(FmtTime.Month[i], t) THEN RETURN ORD(i) + 1; END
        END;
        RAISE Lex.Error;
      END Month;

    (* Format: Sun Mar 31 19:53:33 1996 EST *)
    VAR
      ts := NEW(RefTimestamp);
      rd := TextRd.New(valtext);
    BEGIN
      Lex.Skip(rd, Lex.NonBlanks);
      Lex.Match(rd, " ");
      ts.month := Month(Lex.Scan(rd));
      Lex.Match(rd, " ");
      ts.day := Lex.Int(rd);
      Lex.Match(rd, " ");
      ts.hour := Lex.Int(rd);
      Lex.Match(rd, ":");
      ts.minute := Lex.Int(rd);
      Lex.Match(rd, ":");
      ts.second := Lex.Int(rd);
      Lex.Match(rd, " ");
      ts.year := Lex.Int(rd);
      RETURN ts;
    END BuildTimestamp;


  VAR
    row      : REF ARRAY OF TEXT;
    numFields: INTEGER;
  BEGIN
    (* fetch the next row *)
    row := MySQL.FetchRow(st.result);
    IF row = NIL THEN
      (* no more rows *)
      st.values := NIL;
      RETURN;
    END;
    (* map to result set *)
    TRY

      numFields := MySQL.NumFields(st.result);
      FOR i := 0 TO numFields - 1 DO
        WITH info = st.col_info[i], val = st.values[i] DO
          CASE info.type OF
          | DataType.Char, DataType.VarChar, DataType.VarBinary, 
             DataType.LongVarChar, DataType.LongVarBinary => val := row[i];
          | DataType.Decimal =>
              val := NEW(REF INTEGER);
              NARROW(val, REF INTEGER)^ := Scan.Int (row[i]);
          | DataType.Float, DataType.Double, DataType.Real  =>
              val := NEW(REF REAL);
              NARROW(val, REF REAL)^ := Scan.Real (row[i]);
          | DataType.BigInt, DataType.Integer, DataType.SmallInt, DataType.TinyInt  =>
              val := NEW(REF INTEGER);
              NARROW(val, REF INTEGER)^ := Scan.Int(row[i]);
          | DataType.Date =>
              val := BuildDate (row[i]);
          | DataType.Time =>
              val :=  BuildTime(row[i]);
          | DataType.Timestamp =>
              val := BuildTimestamp(row[i]);
          ELSE
            Die (9, "Bad datatype in DB.MapValues: " & Fmt.Int (ORD(info.type)));
          END;
        END;
      END;
    EXCEPT
    | Lex.Error, Rd.Failure, FloatMode.Trap =>
        Die(10, "Bad format in DB.MapValues");
    | Thread.Alerted => Die(11, "Thread alerted");
    END;
  END MapValues;

PROCEDURE Done (st: Stmt) RAISES {DB.Error} =
  BEGIN
    LOCK st DO
      CheckStmt(st, 21, "finish", check_exec := FALSE);
      IF st.result # NIL THEN
        MySQL.FreeResult(st.result);
        CheckErr(st.conn);
        st.result := NIL;
      END;
      st.prepared := FALSE;
      st.executed := FALSE;
    END
  END Done;

PROCEDURE Close (st: Stmt) RAISES {DB.Error} =
  BEGIN
    LOCK st DO
      CheckStmt(st, 24, "close", check_exec := FALSE);
      st.prepared := FALSE;
      st.executed := FALSE;
      st.conn := NIL;
      st.col_info := NIL;
      st.values := NIL;
    END;
  END Close;

PROCEDURE GetCursorName (st: Stmt): TEXT RAISES {DB.Error} =
  BEGIN
    LOCK st DO
      CheckStmt(st, 27, "get the cursor name from", check_exec := FALSE);
      RETURN st.cursor_name;
    END;
  END GetCursorName;

PROCEDURE SetCursorName (st: Stmt; nm: TEXT) RAISES {DB.Error} =
  BEGIN
    LOCK st DO
      CheckStmt(st, 30, "set the cursor name in", check_exec := FALSE);
      st.cursor_name := nm;
    END;
  END SetCursorName;

PROCEDURE NumRows (st: Stmt): INTEGER RAISES {DB.Error} =
  BEGIN
    LOCK st DO
      CheckStmt(st, 33, "get the row count from", check_exec := TRUE);
      RETURN st.rows;
    END;
  END NumRows;

PROCEDURE DescribeResult (st: Stmt): ResultDesc RAISES {DB.Error} =
  VAR res: ResultDesc;
  BEGIN
    LOCK st DO
      CheckStmt(
        st, 36, "get the result description from", check_exec := TRUE);
      IF NOT st.fetchable THEN RETURN NIL END;
      IF (st.col_info = NIL) THEN BuildColumnInfo(st); END;
      res := NEW(ResultDesc, NUMBER(st.col_info^));
      res^ := st.col_info^;
    END;
    RETURN res;                  (* we return a fresh copy so the client
                                    can't screw up our copy. *)
  END DescribeResult;

PROCEDURE BuildColumnInfo (st: Stmt) RAISES {DB.Error} =
  (* LL = st.mu *)
  BEGIN
    IF (st.col_info # NIL) THEN RETURN; END;
    WITH c = MySQL.NumFields(st.result) DO
      st.col_info := NEW (ResultDesc, c); 
      FOR i := 0 TO c-1 DO 

        WITH field = MySQL.FetchField(st.result), info = st.col_info[i] DO 
          info.name := field.name;
          info.type := MapSQLType(field.type);
          info.precision := field.decimals;
          info.scale := 0; 
          info.nullable := DB.Nullable.Unknown;(* FIXME: get nullable from field.flags *) 
        END;
      END; 
    END; 
  END BuildColumnInfo;

PROCEDURE MapSQLType (sqltype: INTEGER): DataType RAISES {DB.Error} =
  VAR dt: DataType := DataType.Null;
  BEGIN
    CASE sqltype OF
    | MySQL.MYSQL_TYPE_ENUM			=> dt := DataType.Null;
    | MySQL.MYSQL_TYPE_DATETIME 		=> dt := DataType.Timestamp;
    | MySQL.MYSQL_TYPE_YEAR			=> dt := DataType.Date;
    | MySQL.MYSQL_TYPE_NEWDATE 			=> dt := DataType.Date;
    | MySQL.MYSQL_TYPE_LONGLONG 		=> dt := DataType.BigInt;
    | MySQL.MYSQL_TYPE_SET			=> dt := DataType.Null;

    | MySQL.MYSQL_TYPE_DECIMAL 			=> dt := DataType.Decimal;
    | MySQL.MYSQL_TYPE_TINY 			=> dt := DataType.TinyInt;
    | MySQL.MYSQL_TYPE_SHORT 			=> dt := DataType.SmallInt;
    | MySQL.MYSQL_TYPE_LONG 			=> dt := DataType.Integer;
    | MySQL.MYSQL_TYPE_FLOAT 			=> dt := DataType.Float;
    | MySQL.MYSQL_TYPE_DOUBLE 			=> dt := DataType.Double;
    | MySQL.MYSQL_TYPE_NULL 			=> dt := DataType.Null;
    | MySQL.MYSQL_TYPE_TIMESTAMP 		=> dt := DataType.Timestamp;
    | MySQL.MYSQL_TYPE_INT24 			=> dt := DataType.Integer;
    | MySQL.MYSQL_TYPE_DATE 			=> dt := DataType.Date;
    | MySQL.MYSQL_TYPE_TIME 			=> dt := DataType.Time;
    | MySQL.MYSQL_TYPE_TINY_BLOB 		=> dt := DataType.Binary;
    | MySQL.MYSQL_TYPE_MEDIUM_BLOB 		=> dt := DataType.Binary;
    | MySQL.MYSQL_TYPE_LONG_BLOB 		=> dt := DataType.Binary;
    | MySQL.MYSQL_TYPE_BLOB 			=> dt := DataType.Binary;
    | MySQL.MYSQL_TYPE_VAR_STRING 		=> dt := DataType.VarChar;
    | MySQL.MYSQL_TYPE_STRING 			=> dt := DataType.Char;
    ELSE
      Die(
        7, "DB.MapDatatype: unknown SQL datatype " & Fmt.Int(ORD(sqltype)));
    END;
    RETURN dt;
  END MapSQLType;

PROCEDURE CheckStmt
  (st: Stmt; err: INTEGER; verb: TEXT; check_exec := FALSE)
  RAISES {DB.Error} =
  BEGIN
    IF (st.hstmt = NIL) THEN
      Die(err, "Attempted to " & verb & " a closed DB.Stmt");
    END;
    IF (check_exec) AND (NOT st.executed) THEN
      Die(err + 1, "Attempted to " & verb & " an unexecuted DB.Stmt");
    END;
    IF (st.conn = NIL) OR (st.conn.hdbc = NIL) THEN
      Die(err + 2,
          "Attempted to " & verb & " a DB.Stmt on a disconnected DB.T.");
    END;
  END CheckStmt;

(*------------------------------------------------------------- DBRep ---*)

<*UNUSED*>PROCEDURE GetHENV (): ADDRESS =
  BEGIN
    RETURN NIL;
  END GetHENV;

<*UNUSED*>PROCEDURE GetHDBC (t: DB.T): ADDRESS =
  BEGIN
    RETURN NARROW(t, T).hdbc;
  END GetHDBC;

<*UNUSED*>PROCEDURE GetHSTMT (st: DB.Stmt): TEXT =
  BEGIN
    RETURN NARROW(st, Stmt).hstmt;
  END GetHSTMT;


(*--------------------------------------------- errors and exceptions ---*)

PROCEDURE CheckErr (conn: T) RAISES {DB.Error} =
  VAR
    desc := NEW(ErrorDesc,
                state := ARRAY OF CHAR{'M', '3', '?', '?', '?', '\000'},
                native_err := -1);
  BEGIN
    IF conn.hdbc = NIL THEN
      desc.description := "invalid connection object";
      RAISE DB.Error(desc);
    END;

    desc.native_err := MySQL.Errno(conn.hdbc);
    IF desc.native_err = 0 THEN RETURN END;
    desc.description := MySQL.Error(conn.hdbc);
    RAISE DB.Error(desc);
  END CheckErr;

PROCEDURE Die (id: [0 .. 999]; msg: TEXT) RAISES {DB.Error} =
  CONST Zero = ORD('0');
  VAR desc := NEW(ErrorDesc);
  BEGIN
    desc.state[5] := '\000';
    desc.state[4] := VAL(Zero + id MOD 10, CHAR);
    id := id DIV 10;
    desc.state[3] := VAL(Zero + id MOD 10, CHAR);
    id := id DIV 10;
    desc.state[2] := VAL(Zero + id MOD 10, CHAR);
    desc.state[1] := '3';
    desc.state[0] := 'M';
    desc.native_err := 0;
    desc.description := "[Modula-3 DB] " & msg;
    RAISE DB.Error(desc);
  END Die;

(*----------------------------------------- misc.  internal functions ---*)

PROCEDURE Unimplemented (<* UNUSED *> msg: TEXT := "") =
  BEGIN
    (* Quietly return! *)
    (* IO.Put (msg & " is not implemented yet\n"); *)
  END Unimplemented;

PROCEDURE SQL (t: T; query: TEXT) RAISES {DB.Error} =
  (* LL = st.mu *)
  VAR
    ret : INTEGER;
  BEGIN
    IF Debug THEN IO.Put("SQL: " & query & "\n") END;
    ret := MySQL.Query(t.hdbc, query);
    IF ret # 0 THEN
      CheckErr(t);
    END;
  END SQL;

BEGIN
END MySQLDB.
