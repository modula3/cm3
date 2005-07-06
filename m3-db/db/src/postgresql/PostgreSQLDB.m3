(* Copyright (C) 1996-2000, Critical Mass, Inc.  All Rights Reserved. *)
(* See file COPYRIGHT-CMASS for details. *)
(* Updated by Darko Volaric September 2002 *)

UNSAFE MODULE PostgreSQLDB EXPORTS PostgreSQLDB, PostgreSQLDBRep;

FROM Text IMPORT Equal, Sub, FindChar, Length, GetChar;
FROM M3toC IMPORT SharedTtoS, FreeSharedS, CopyTtoS, StoT, CopyStoT;
FROM Ctypes IMPORT int;

IMPORT 
	ASCII, PostgreSQL, WeakRef, Scan, Lex, FloatMode, Fmt, TextRd, Rd, Thread,
	FmtTime, Text, IO, DB;

FROM DB IMPORT
	Results, ResultDesc, DescList, ColumnDesc, Nullable, DataType, RefString, 
	RefBigInt, RefDate, RefTime, RefTimestamp, ErrorDesc;

CONST 
  Debug = TRUE;

TYPE
  SQLINTEGER = INTEGER;

TYPE
  OpenArrayRep = RECORD
    data_ptr : ADDRESS;
    n_elts   : INTEGER;
  END;

(*------------------------------------------------------- interfaces  ---*)

TYPE
	Interface  = DB.Interface OBJECT
	OVERRIDES
		connect := Connect;
		get_data_sources := GetDataSources;
		get_drivers := GetDrivers;
	END;

PROCEDURE GetInterface(): DB.Interface =
BEGIN
	RETURN NEW(Interface, name := "PostgreSQL");
END GetInterface;

PROCEDURE Connect (
	interface: Interface; database: TEXT;
  user_id, password: TEXT;
  server: TEXT := NIL): DB.T  RAISES {DB.Error} =
  VAR
    t    := NEW (T);
    pghost, pgport, pgoptions, pgtty: ADDRESS := NIL;
  BEGIN

    t.hdbc := PostgreSQL.PQsetdbLogin (pghost, pgport, pgoptions, pgtty,
    	CopyTtoS (database), CopyTtoS(user_id), CopyTtoS(password) );
    CheckErr (t);
    EVAL WeakRef.FromRef (t, CleanupConnection);
    RETURN t;
  END Connect;

PROCEDURE GetDataSources (this: Interface): DescList  =
  BEGIN
    Unimplemented ("GetDataSources");
    RETURN NIL;
  END GetDataSources;

PROCEDURE GetDrivers (this: Interface): DescList =
  BEGIN
    Unimplemented("GetDrivers");
    RETURN NIL ;
  END GetDrivers;


(*------------------------------------------------------- connections ---*)

TYPE
  T = DB.T OBJECT
    hdbc : PostgreSQL.PGconn;
    last_exec_status : PostgreSQL.PGRS;
    auto_commit_on: BOOLEAN := FALSE;
    used: BOOLEAN := FALSE;
  OVERRIDES
    disconnect  := Disconnect;
    new_stmt    := NewStmt;
    auto_commit := AutoCommit;
    commit      := Commit;
    abort       := Abort;
  END;

PROCEDURE Disconnect (t: T) RAISES {DB.Error} =
  BEGIN
    IF (t.hdbc = NIL) THEN
      Die (1, "DB.T is already disconnected.");
    END;
    PostgreSQL.PQfinish (t.hdbc);
    CheckErr (conn := t);
    t.hdbc := NIL;
  END Disconnect;

PROCEDURE CleanupConnection (<*UNUSED*> READONLY w: WeakRef.T;  ref: REFANY) =
  VAR t := NARROW (ref, T);
  BEGIN
    IF Debug THEN IO.Put ("Cleaning a connection\n") END;
    IF (t.hdbc # NIL) THEN
      TRY IF t.auto_commit_on THEN Commit(t) END EXCEPT ELSE END;
      PostgreSQL.PQfinish (t.hdbc);
      t.hdbc := NIL;
    END;
  END CleanupConnection;

PROCEDURE AutoCommit (t: T; on: BOOLEAN) RAISES {DB.Error} =
  BEGIN
    IF Debug THEN 
      IO.Put ("DB: Warning: Postgres does not implement autocommit.\n");
    END;
    IF (t.hdbc = NIL) THEN
      Die (2, "Attempted to set AutoCommit on a disconnected DB.T.");
    END;
    t.auto_commit_on := on;
  END AutoCommit;

PROCEDURE Commit (t: T) RAISES {DB.Error} =
  BEGIN
    IF (t.hdbc = NIL) THEN
      Die (3, "Attempted to commit a disconnected DB.T.");
    END;
    EVAL SQL (t, "END");
  END Commit;

PROCEDURE Abort (t: T) RAISES {DB.Error} =
  BEGIN
    IF (t.hdbc = NIL) THEN
      Die (4, "Attempted to abort a disconnected DB.T.");
    END;
    EVAL SQL (t, "ABORT");
  END Abort;

PROCEDURE NewStmt (t: T): DB.Stmt RAISES {DB.Error} =
  VAR
    st := NEW (Stmt);
  BEGIN
    IF (t.hdbc = NIL) THEN
      Die (5, "Attempted to create a new statement on a disconnected DB.T.");
    END;

    st.conn     := t;
    st.hstmt    := NIL;
    st.prepared := FALSE;
    st.executed := FALSE;

    EVAL SQL (t, "BEGIN");
    EVAL WeakRef.FromRef (st, CleanupStmt);

    RETURN st;
  END NewStmt;

PROCEDURE CleanupStmt (<*UNUSED*> READONLY wr: WeakRef.T;  ref: REFANY) =
  VAR st := NARROW (ref, Stmt);
  BEGIN
    IF (st.hstmt # NIL) THEN
      IF (st.conn # NIL) AND (st.conn.hdbc # NIL) THEN
        (* Q: Notify database that the statement is deallocated. *)
      END;
      st.conn     := NIL;
      st.hstmt    := NIL;
      st.prepared := FALSE;
      st.executed := FALSE;
    END;
  END CleanupStmt;

(*-------------------------------------------------------- statements ---*)

TYPE
  (* a SQL database statement (query or update) *)
  Stmt = DB.Stmt OBJECT
    conn     : T;  (* my database connection *)
    hstmt    : TEXT := NIL;
    prepared : BOOLEAN;
    executed : BOOLEAN;
    col_info : ResultDesc;
    values   : Results;
    current_row: int;
    fetchable: BOOLEAN;
    rows:      int;
    cursor_name: TEXT := "myportal";
    result   : PostgreSQL.PGresult := NIL;
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
      st.hstmt    := operation;
      CheckStmt (st, 12, "prepare", check_exec := FALSE);
      st.prepared := TRUE;
      st.executed := FALSE;
      st.col_info := NIL;
      st.values   := NIL;
    END
  END Prepare;

PROCEDURE Execute (st: Stmt;  operation: TEXT) RAISES {DB.Error} =
  BEGIN
    LOCK st DO
      CheckStmt (st, 15, "execute", check_exec := FALSE);
      IF (operation = NIL) THEN
        IF st.hstmt = NIL THEN 
          Die (6, "Attempted to execute a closed DB.Stmt");
        ELSE
          operation := st.hstmt;
        END
      ELSE
        IF st.hstmt = NIL THEN
          st.hstmt := operation;
        ELSE
          <*ASSERT Equal (st.hstmt, operation)*>
        END;
      END;
      st.executed := TRUE;
      st.col_info := NIL;
      st.values   := NIL;
      st.current_row := 0;

      st.fetchable := Fetchable(st);

      IF NOT st.fetchable THEN 
        EVAL SQL (st.conn, st.hstmt); 
      ELSE
        EVAL SQL (st.conn, 
                  "DECLARE " &  st.cursor_name & " CURSOR FOR " & st.hstmt);

        st.result := SQL (st.conn, "FETCH ALL in " & st.cursor_name);
        st.rows := PostgreSQL.PQntuples (st.result); CheckErr (st.conn);
      END;
    END;
  END Execute;

PROCEDURE Fetchable (st: Stmt): BOOLEAN =

  PROCEDURE CaseInsensitiveEqual (a, b: TEXT): BOOLEAN =
    BEGIN
      IF Length(a) # Length(b) THEN RETURN FALSE END;
      FOR i := 0 TO Length(a) - 1 DO
        IF ASCII.Upper[GetChar(a, i)] # 
          ASCII.Upper[GetChar(b, i)] THEN
          RETURN FALSE;
        END
      END;
      RETURN TRUE;
    END CaseInsensitiveEqual;

  CONST
    fetchable_commands = ARRAY OF TEXT { "select" };
  VAR
    cmd: TEXT; 
    index := FindChar (st.hstmt, ' ');
  BEGIN
    (* One-word commands are not fetchable *)    
    IF index = -1 THEN RETURN FALSE END; 

    (* Search for the command in the fetchable_commands array. *)
    cmd := Sub (st.hstmt, 0, index);
    FOR i := FIRST(fetchable_commands) TO LAST(fetchable_commands) DO
      IF CaseInsensitiveEqual (fetchable_commands[i], cmd) THEN
        RETURN TRUE;
      END
    END;
    RETURN FALSE;
  END Fetchable;

PROCEDURE Fetch (st: Stmt): Results RAISES {DB.Error} =
  BEGIN
    LOCK st DO
      CheckStmt (st, 18, "fetch from", check_exec := TRUE);
      IF NOT st.fetchable THEN RETURN NIL END;
      IF st.values = NIL THEN BuildValueArea(st) END;

      TRY
	IF st.current_row = st.rows THEN
          EVAL SQL (st.conn, "CLOSE " & st.cursor_name);
          st.result := NIL;
          RETURN NIL 
	END;
        MapValues (st);
        CheckErr (st.conn);
      FINALLY 
        INC(st.current_row);
      END;
    END;
    RETURN st.values;
  END Fetch;

(* These types define the layout of the receive buffer for each DataType. *)
TYPE
  (* => Char, VarChar, LongVarChar, Binary, VarBinary, LongVarBinary *)
  StringPtr = UNTRACED REF StringVal;
  StringVal = RECORD
    data_len : SQLINTEGER;
    array    : OpenArrayRep;
    contents : ARRAY [0..0] OF CHAR;
  END;

(*

  (* => BigInt *)
  BigIntPtr = UNTRACED REF BigIntVal;
  BigIntVal = RECORD
    data_len : SQLINTEGER;
    value    : RECORD lo, hi: SQLINTEGER; END;
  END;

  (* => Integer, SmallInt, TinyInt *)
  IntPtr = UNTRACED REF IntVal;
  IntVal = RECORD
    data_len : SQLINTEGER;
    value    : SQLINTEGER;
  END;

  (* => Numeric, Decimal, Float, Double *)
  FloatPtr = UNTRACED REF FloatVal;
  FloatVal = RECORD
    data_len : SQLINTEGER;
    value    : LDOUBLE;
  END;

*)

(*

  Leftover from SQL types.

TYPE
  LDOUBLE = LONGREAL;
  SFLOAT = REAL;

  (* => Real *)
  RealPtr = UNTRACED REF RealVal;
  RealVal = RECORD
    data_len : SQLINTEGER;
    value    : SFLOAT;
  END;

  (* => Bit *)
  BitPtr = UNTRACED REF BitVal;
  BitVal = RECORD
    data_len : SQLINTEGER;
    value    : SWORD;
  END;

  (* => Date *)
  DatePtr = UNTRACED REF DateVal;
  DateVal = RECORD
    data_len : SQLINTEGER;
    value    : DATE_STRUCT;
  END;

  (* => Time *)
  TimePtr = UNTRACED REF TimeVal;
  TimeVal = RECORD
    data_len : SQLINTEGER;
    value    : TIME_STRUCT;
  END;

  (* => Timestamp *)
  TimestampPtr = UNTRACED REF TimestampVal;
  TimestampVal = RECORD
    data_len : SQLINTEGER;
    value    : Timestamp_STRUCT;
  END;

*)

PROCEDURE BuildValueArea (st: Stmt) RAISES {DB.Error} =
  (* LL = st.mu *)
  BEGIN (* BuildValueArea *)
    IF (st.col_info = NIL) THEN BuildColumnInfo (st); END;
    st.values := NEW (Results, NUMBER (st.col_info^));
  END BuildValueArea;

PROCEDURE MapValues (st: Stmt) RAISES {DB.Error} =

  (* This procedure is pretty yucky since it uses the character mode
     of PQvalues. It is also probably slow. *)
  
  PROCEDURE BuildString (row, col: INTEGER): RefString =
    VAR 
      ref := NEW (RefString);
      str := NEW(StringPtr);
    BEGIN
    (*
      str.array.data_ptr := PostgreSQL.PQgetvalue (st.result, row, col);
      str.array.n_elts   := PostgreSQL.PQgetlength (st.result, row, col);
      ref^ := ADR (str.array);
     *)
     	ref^ := LOOPHOLE(PostgreSQL.PQgetvalue (st.result, row, col), UNTRACED REF ARRAY OF CHAR);
      RETURN ref;
    END BuildString;

    
    PROCEDURE BuildTime(valtext: TEXT): RefTime
        RAISES {Lex.Error, Thread.Alerted, Rd.Failure, FloatMode.Trap} = 
      VAR time := NEW(RefTime);
          rd := TextRd.New(valtext);
      BEGIN
        time.hour := Lex.Int(rd);
        Lex.Match (rd, ":");
        time.minute := Lex.Int(rd);
        Lex.Match (rd, ":");
        time.second := Lex.Int(rd);
        RETURN time;
        (* Ignore everything after the seconds *)
      END BuildTime;

    PROCEDURE BuildDate(valtext: TEXT): RefDate 
        RAISES {Lex.Error, Thread.Alerted, Rd.Failure, FloatMode.Trap} = 
      VAR date := NEW(RefDate);
          rd := TextRd.New(valtext);
      BEGIN
        date.month := Lex.Int(rd);
        Lex.Match (rd, "-");
        date.day := Lex.Int(rd);
        Lex.Match (rd, "-");
        date.year := Lex.Int (rd);
        RETURN date;
      END BuildDate;

    PROCEDURE BuildTimestamp(valtext: TEXT): RefTimestamp 
        RAISES {Lex.Error, Thread.Alerted, Rd.Failure, FloatMode.Trap} = 

      PROCEDURE Month (t: TEXT): CARDINAL RAISES {Lex.Error} = 
        BEGIN
          FOR i := FIRST (FmtTime.Month) TO LAST(FmtTime.Month) DO
            IF Text.Equal (FmtTime.Month[i], t) THEN
              RETURN ORD(i) + 1;
            END
          END;
          RAISE Lex.Error;
        END Month;
            
      (* Format: Sun Mar 31 19:53:33 1996 EST *)
      VAR ts := NEW(RefTimestamp);
          rd := TextRd.New(valtext);
      BEGIN
        Lex.Skip (rd, Lex.NonBlanks);
        Lex.Match (rd, " ");
        ts.month := Month (Lex.Scan(rd));
        Lex.Match (rd, " ");
        ts.day := Lex.Int(rd);
        Lex.Match (rd, " ");
        ts.hour := Lex.Int (rd);
        Lex.Match (rd, ":");
        ts.minute := Lex.Int (rd);
        Lex.Match (rd, ":");
        ts.second := Lex.Int (rd);
        Lex.Match (rd, " ");
        ts.year := Lex.Int (rd);
        RETURN ts;
      END BuildTimestamp;

  BEGIN
    TRY
      FOR i := FIRST(st.values^) TO LAST (st.values^) DO      
        WITH info = st.col_info[i],
             val  = st.values[i] DO
          IF PostgreSQL.PQgetisnull(st.result, st.current_row, i) # 0 THEN 
            val := NIL;
          ELSE
            WITH dbval = PostgreSQL.PQgetvalue (st.result, st.current_row, i),
                 valtext = StoT (dbval) DO
              CASE info.type OF
              | DataType.Char => 
                val := BuildString (st.current_row, i);
              | DataType.VarChar, DataType.VarBinary =>
                val := BuildString (st.current_row, i);
              | DataType.LongVarChar, DataType.LongVarBinary => 
                val := BuildString (st.current_row, i);
              | DataType.Decimal => 
                val := NEW(REF INTEGER);
                NARROW(val, REF INTEGER)^ := Scan.Int (valtext);
              | DataType.Float, DataType.Double        => 
                val := NEW(REF REAL);
                NARROW(val, REF REAL)^ := Scan.Real (valtext);
              | DataType.BigInt, DataType.Integer, DataType.SmallInt, DataType.TinyInt       => 
                val := NEW(REF INTEGER);
                NARROW(val, REF INTEGER)^ := Scan.Int(valtext);
              | DataType.Real          => 
                val := NEW(REF REAL);
                NARROW(val, REF REAL)^ := Scan.Real (valtext);
              | DataType.Date =>
                val := BuildDate (valtext);
              | DataType.Time =>
                val :=  BuildTime(valtext);
              | DataType.Timestamp =>
                val := BuildTimestamp(valtext);
              | DataType.Bit =>
                val := NEW(REF BOOLEAN); 
                NARROW(val, REF BOOLEAN)^ := Text.GetChar(valtext, 0) = 't'; 
              ELSE
                Die (9, "Bad datatype in DB.MapValues: " & Fmt.Int (ORD(info.type)));
              END
            END
          END
        END
      END
    EXCEPT
      | Lex.Error, Rd.Failure, FloatMode.Trap => 
	  Die (10, "Bad format in DB.MapValues");
      | Thread.Alerted =>
          Die (11, "Thread alerted");
    END;
  END MapValues;

PROCEDURE Done (st: Stmt) RAISES {DB.Error} =
  BEGIN
    LOCK st DO
      CheckStmt (st, 21, "finish", check_exec := FALSE);
      IF st.result # NIL THEN 
        PostgreSQL.PQclear(st.result); 
        CheckErr(st.conn);
        st.result := NIL;
        EVAL SQL (st.conn, "CLOSE " & st.cursor_name);
      END;
      st.prepared := FALSE;
      st.executed := FALSE;
    END
  END Done;

PROCEDURE Close (st: Stmt) RAISES {DB.Error} =
  BEGIN
    LOCK st DO
      CheckStmt (st, 24, "close", check_exec := FALSE);
      st.prepared := FALSE;
      st.executed := FALSE;
      st.conn     := NIL;
      st.col_info := NIL;
      st.values := NIL;
    END;
  END Close;

PROCEDURE GetCursorName (st: Stmt): TEXT RAISES {DB.Error} =
  BEGIN
    LOCK st DO
      CheckStmt (st, 27, "get the cursor name from", check_exec := FALSE);
      RETURN st.cursor_name;
    END;
  END GetCursorName;

PROCEDURE SetCursorName (st: Stmt;  nm: TEXT) RAISES {DB.Error} =
  BEGIN
    LOCK st DO
      CheckStmt (st, 30, "set the cursor name in", check_exec := FALSE);
      st.cursor_name := nm;
    END;
  END SetCursorName;

PROCEDURE NumRows (st: Stmt): INTEGER RAISES {DB.Error} =
  BEGIN
    LOCK st DO
      CheckStmt (st, 33, "get the row count from", check_exec := TRUE);
      RETURN st.rows;
    END;
  END NumRows;

PROCEDURE DescribeResult (st: Stmt): ResultDesc  RAISES {DB.Error} =
  VAR res: ResultDesc;
  BEGIN
    LOCK st DO
      CheckStmt (st, 36, "get the result description from", check_exec := TRUE);
      IF NOT st.fetchable THEN RETURN NIL END;
      IF (st.col_info = NIL) THEN BuildColumnInfo (st); END;
      res := NEW (ResultDesc, NUMBER (st.col_info^));
      res^ := st.col_info^;
    END;
    RETURN res; (* we return a fresh copy so the client can't screw up our copy. *)
  END DescribeResult;

PROCEDURE BuildColumnInfo (st: Stmt) RAISES {DB.Error} =
  (* LL = st.mu *)
  BEGIN
    IF (st.col_info # NIL) THEN RETURN; END;
    WITH cnt = PostgreSQL.PQnfields (st.result) DO
      st.col_info := NEW (ResultDesc, cnt);
      FOR i := 0 TO cnt-1 DO
        WITH z = st.col_info[i] DO
          z.name := CopyStoT (PostgreSQL.PQfname (st.result, i));
          z.type := MapSqlType (PostgreSQL.PQftype (st.result, i));
          z.precision := PostgreSQL.PQfsize(st.result, i);
          z.scale := 0;
          z.nullable := Nullable.Unknown;
        END
      END
    END
  END BuildColumnInfo;

PROCEDURE MapSqlType (sqltype: PostgreSQL.Oid): DataType RAISES {DB.Error} =
  VAR dt: DataType := DataType.Null;
  BEGIN
    CASE sqltype OF
(*
    | PostgreSQL.TYPE_NULL     =>  dt := DataType.Null; 
    | PostgreSQL.NUMERIC       =>  dt := DataType.Numeric;
    | PostgreSQL.DECIMAL       =>  dt := DataType.Decimal;
    | PostgreSQL.Double        =>  dt := DataType.Double;
    | PostgreSQL.BINARY        =>  dt := DataType.Binary;
    | PostgreSQL.VARBINARY     =>  dt := DataType.VarBinary;
    | PostgreSQL.LONGVARBINARY =>  dt := DataType.LongVarBinary;
    | PostgreSQL.BIGINT        =>  dt := DataType.BigInt;
    | PostgreSQL.TINYINT       =>  dt := DataType.TinyInt;
    | PostgreSQL.BIT           =>  dt := DataType.Bit;
 *)


    | PostgreSQL.Timestamp     =>  dt := DataType.Timestamp;
    | PostgreSQL.Char, PostgreSQL.Char2, PostgreSQL.Char4,
      PostgreSQL.Char8, PostgreSQL.Char16, PostgreSQL.Bytea,
      PostgreSQL.Bpchar
                             =>  dt := DataType.Char;
    | PostgreSQL.Int           =>  dt := DataType.Integer;
    | PostgreSQL.SmallInt      =>  dt := DataType.SmallInt;
    | PostgreSQL.Float         =>  dt := DataType.Float;
    | PostgreSQL.VarChar       =>  dt := DataType.VarChar;
    | PostgreSQL.Date          =>  dt := DataType.Date;
    | PostgreSQL.Time          =>  dt := DataType.Time;
    | PostgreSQL.Text          =>  dt := DataType.VarChar; 
    | PostgreSQL.Bool          =>  dt := DataType.Bit;
    
    ELSE Die (7, "DB.MapDatatype: unknown SQL datatype " & Fmt.Int(ORD(sqltype)));
    END;
    RETURN dt;
  END MapSqlType;

PROCEDURE CheckStmt (st: Stmt;  err: INTEGER;  verb: TEXT;  check_exec := FALSE)
  RAISES {DB.Error} =
  BEGIN
    IF (st.hstmt = NIL) THEN
      Die (err, "Attempted to " & verb & " a closed DB.Stmt");
    END;
    IF (check_exec) AND (NOT st.executed) THEN
      Die (err+1, "Attempted to " & verb & " an unexecuted DB.Stmt");
    END;
    IF (st.conn = NIL) OR (st.conn.hdbc = NIL) THEN
      Die (err+2, "Attempted to " & verb  & " a DB.Stmt on a disconnected DB.T.");
    END;
  END CheckStmt;

(*------------------------------------------------------------- DBRep ---*)

PROCEDURE GetHENV (): NULL =
  BEGIN
    RETURN NIL;
  END GetHENV;

PROCEDURE GetHDBC (t: DB.T): ADDRESS =
  BEGIN
    RETURN NARROW(t, T).hdbc;
  END GetHDBC;

PROCEDURE GetHSTMT (st: DB.Stmt): TEXT =
  BEGIN
    RETURN NARROW(st, Stmt).hstmt;
  END GetHSTMT;

(*--------------------------------------------- errors and exceptions ---*)

PROCEDURE CheckErr (conn: T) RAISES {DB.Error} =
  VAR
    desc := NEW(ErrorDesc, 
                state := ARRAY OF CHAR {'M', '3', '?', '?', '?', '\000'},
                native_err := -1);
    description: TEXT;
  BEGIN

    CASE PostgreSQL.PQstatus (conn.hdbc) OF
    | PostgreSQL.CONNECTION.OK => RETURN;
    | PostgreSQL.CONNECTION.BAD => description := "bad connection";
    ELSE
      description := "some kind of error in connection";
    END;
    
    CASE conn.last_exec_status OF
    | PostgreSQL.PGRS.COMMAND_OK, PostgreSQL.PGRS.EMPTY_QUERY,
      PostgreSQL.PGRS.TUPLES_OK =>       RETURN;
    | PostgreSQL.PGRS.BAD_RESPONSE =>    description := "bad response";
    | PostgreSQL.PGRS.FATAL_ERROR =>     description := "fatal error";
    | PostgreSQL.PGRS.NONFATAL_ERROR =>  description := "non-fatal error";
    ELSE
      description := "unknown error";
    END;
    
    desc.description := description;
    RAISE DB.Error (desc);
  END CheckErr;

PROCEDURE Die (id: [0..999]; msg: TEXT) RAISES {DB.Error} =
  CONST Zero = ORD ('0');
  VAR desc := NEW (ErrorDesc);
  BEGIN
    desc.state[5]    := '\000';
    desc.state[4]    := VAL (Zero + id MOD 10, CHAR);  id := id DIV 10;
    desc.state[3]    := VAL (Zero + id MOD 10, CHAR);  id := id DIV 10;
    desc.state[2]    := VAL (Zero + id MOD 10, CHAR);
    desc.state[1]    := '3';
    desc.state[0]    := 'M';
    desc.native_err  := 0;
    desc.description := "[Modula-3 DB] " & msg;
    RAISE DB.Error (desc);
  END Die;

(*----------------------------------------- misc. internal functions ---*)


PROCEDURE Unimplemented(<*UNUSED*>msg: TEXT := "") = 
  BEGIN
    (* Quietly return! *)
(*    IO.Put (msg & " is not implemented yet\n"); *)
  END Unimplemented;

PROCEDURE SQL (t: T; query: TEXT): PostgreSQL.PGresult RAISES {DB.Error} = 
  (* LL = st.mu *)
  VAR result: PostgreSQL.PGresult;
  BEGIN
    IF Debug THEN IO.Put ("SQL: " & query & "\n") END;
    VAR str := SharedTtoS(query); BEGIN
      result := PostgreSQL.PQexec (t.hdbc, str);
      FreeSharedS(query, str);
    END;
    IF result = NIL THEN 
      RAISE DB.Error (NEW(ErrorDesc,
                       description := CopyStoT(PostgreSQL.PQerrorMessage(t.hdbc))));
    END;
    t.last_exec_status := PostgreSQL.PQresultStatus(result);
    CheckErr(t);
    RETURN result;
  END SQL;

BEGIN
END PostgreSQLDB.
