(* Copyright (C) 1996, Critical Mass, Inc.  All Rights Reserved. *)
(* See the file COPYRIGHT.CM for a full description              *)

UNSAFE MODULE DB EXPORTS DB, DBRep;

IMPORT WeakRef;
FROM Text IMPORT Equal, Sub, FindChar, Length, GetChar;
IMPORT ASCII;
IMPORT PQ, Postgres;
FROM M3toC IMPORT TtoS, CopyTtoS, StoT, CopyStoT;
FROM Ctypes IMPORT int;
IMPORT Scan, Lex, FloatMode, Fmt, TextRd, Rd, Thread;
IMPORT FmtTime, Text;
IMPORT IO; (* For debugging *)

CONST 
  Debug = TRUE;

TYPE
  SQLINTEGER = INTEGER;

TYPE
  OpenArrayRep = RECORD
    data_ptr : ADDRESS;
    n_elts   : INTEGER;
  END;

(*------------------------------------------------------- connections ---*)

REVEAL
  T = Public BRANDED "DB.T" OBJECT
    hdbc : PQ.PGconn_star;
    last_exec_status : PQ.PGRS;
    auto_commit_on: BOOLEAN := FALSE;
    used: BOOLEAN := FALSE;
  OVERRIDES
    disconnect  := Disconnect;
    new_stmt    := NewStmt;
    auto_commit := AutoCommit;
    commit      := Commit;
    abort       := Abort;
  END;

PROCEDURE Connect (database: TEXT;
                   <*UNUSED*>user_id, password: TEXT): T  RAISES {Error} =
  VAR
    t    := NEW (T);
    pghost, pgport, pgoptions, pgtty: ADDRESS := NIL;
  BEGIN

    t.hdbc := PQ.PQsetdb (pghost, pgport, pgoptions, pgtty,
                          dbName := CopyTtoS (database));
    CheckErr (t);
    EVAL WeakRef.FromRef (t, CleanupConnection);
    RETURN t;
  END Connect;

PROCEDURE Disconnect (t: T) RAISES {Error} =
  BEGIN
    IF (t.hdbc = NIL) THEN
      Die (1, "DB.T is already disconnected.");
    END;
    PQ.PQfinish (t.hdbc);
    CheckErr (conn := t);
    t.hdbc := NIL;
  END Disconnect;

PROCEDURE CleanupConnection (<*UNUSED*> READONLY w: WeakRef.T;  ref: REFANY) =
  VAR t := NARROW (ref, T);
  BEGIN
    IF Debug THEN IO.Put ("Cleaning a connection\n") END;
    IF (t.hdbc # NIL) THEN
      TRY IF t.auto_commit_on THEN Commit(t) END EXCEPT ELSE END;
      PQ.PQfinish (t.hdbc);
      t.hdbc := NIL;
    END;
  END CleanupConnection;

PROCEDURE AutoCommit (t: T; on: BOOLEAN) RAISES {Error} =
  BEGIN
    IF Debug THEN 
      IO.Put ("DB: Warning: Postgres does not implement autocommit.\n");
    END;
    IF (t.hdbc = NIL) THEN
      Die (2, "Attempted to set AutoCommit on a disconnected DB.T.");
    END;
    t.auto_commit_on := on;
  END AutoCommit;

PROCEDURE Commit (t: T) RAISES {Error} =
  BEGIN
    IF (t.hdbc = NIL) THEN
      Die (3, "Attempted to commit a disconnected DB.T.");
    END;
    EVAL SQL (t, "END");
  END Commit;

PROCEDURE Abort (t: T) RAISES {Error} =
  BEGIN
    IF (t.hdbc = NIL) THEN
      Die (4, "Attempted to abort a disconnected DB.T.");
    END;
    EVAL SQL (t, "ABORT");
  END Abort;

PROCEDURE NewStmt (t: T): Stmt RAISES {Error} =
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

REVEAL
  (* a SQL database statement (query or update) *)
  Stmt = StmtPublic BRANDED "DB.Stmt" OBJECT
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
    result   : PQ.PGresult_star := NIL;
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

PROCEDURE StmtConnection (st: Stmt): T =
  BEGIN
    RETURN st.conn;
  END StmtConnection;

PROCEDURE Prepare (st: Stmt; operation: TEXT) RAISES {Error} =
  BEGIN
    LOCK st DO 
      CheckStmt (st, 12, "prepare", check_exec := FALSE);
      st.hstmt    := operation;
      st.prepared := TRUE;
      st.executed := FALSE;
      st.col_info := NIL;
      st.values   := NIL;
    END
  END Prepare;

PROCEDURE Execute (st: Stmt;  operation: TEXT) RAISES {Error} =
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
        st.rows := PQ.PQntuples (st.result); CheckErr (st.conn);
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

PROCEDURE Fetch (st: Stmt): Results RAISES {Error} =
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

PROCEDURE BuildValueArea (st: Stmt) RAISES {Error} =
  (* LL = st.mu *)
  BEGIN (* BuildValueArea *)
    IF (st.col_info = NIL) THEN BuildColumnInfo (st); END;
    st.values := NEW (Results, NUMBER (st.col_info^));
  END BuildValueArea;

PROCEDURE MapValues (st: Stmt) RAISES {Error} =

  (* This procedure is pretty yucky since it uses the character mode
     of PQvalues. It is also probably slow. *)
  
  PROCEDURE BuildString (row, col: INTEGER): RefString =
    VAR 
      ref := NEW (RefString);
      str := NEW(StringPtr);
    BEGIN
      str.array.data_ptr := PQ.PQgetvalue (st.result, row, col);
      str.array.n_elts   := PQ.PQgetlength (st.result, row, col);
      ref^ := ADR (str.array);
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
          WITH dbval = PQ.PQgetvalue (st.result, st.current_row, i),
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
            ELSE
              Die (9, "Bad datatype in DB.MapValues: " & Fmt.Int (ORD(info.type)));
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

PROCEDURE Done (st: Stmt) RAISES {Error} =
  BEGIN
    LOCK st DO
      CheckStmt (st, 21, "finish", check_exec := FALSE);
      IF st.result # NIL THEN 
        PQ.PQclear(st.result); 
        CheckErr(st.conn);
        st.result := NIL;
        EVAL SQL (st.conn, "CLOSE " & st.cursor_name);
      END;
      st.prepared := FALSE;
      st.executed := FALSE;
    END
  END Done;

PROCEDURE Close (st: Stmt) RAISES {Error} =
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

PROCEDURE GetCursorName (st: Stmt): TEXT RAISES {Error} =
  BEGIN
    LOCK st DO
      CheckStmt (st, 27, "get the cursor name from", check_exec := FALSE);
      RETURN st.cursor_name;
    END;
  END GetCursorName;

PROCEDURE SetCursorName (st: Stmt;  nm: TEXT) RAISES {Error} =
  BEGIN
    LOCK st DO
      CheckStmt (st, 30, "set the cursor name in", check_exec := FALSE);
      st.cursor_name := nm;
    END;
  END SetCursorName;

PROCEDURE NumRows (st: Stmt): INTEGER RAISES {Error} =
  BEGIN
    LOCK st DO
      CheckStmt (st, 33, "get the row count from", check_exec := TRUE);
      RETURN st.rows;
    END;
  END NumRows;

PROCEDURE DescribeResult (st: Stmt): ResultDesc  RAISES {Error} =
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

PROCEDURE BuildColumnInfo (st: Stmt) RAISES {Error} =
  (* LL = st.mu *)
  BEGIN
    IF (st.col_info # NIL) THEN RETURN; END;
    WITH cnt = PQ.PQnfields (st.result) DO
      st.col_info := NEW (ResultDesc, cnt);
      FOR i := 0 TO cnt-1 DO
        WITH z = st.col_info[i] DO
          z.name := CopyStoT (PQ.PQfname (st.result, i));
          z.type := MapSqlType (PQ.PQftype (st.result, i));
          z.precision := PQ.PQfsize(st.result, i);
          z.scale := 0;
          z.nullable := Nullable.Unknown;
        END
      END
    END
  END BuildColumnInfo;

PROCEDURE MapSqlType (sqltype: Postgres.Oid): DataType RAISES {Error} =
  VAR dt: DataType := DataType.Null;
  BEGIN
    CASE sqltype OF
(*
    | Postgres.TYPE_NULL     =>  dt := DataType.Null; 
    | Postgres.NUMERIC       =>  dt := DataType.Numeric;
    | Postgres.DECIMAL       =>  dt := DataType.Decimal;
    | Postgres.Double        =>  dt := DataType.Double;
    | Postgres.BINARY        =>  dt := DataType.Binary;
    | Postgres.VARBINARY     =>  dt := DataType.VarBinary;
    | Postgres.LONGVARBINARY =>  dt := DataType.LongVarBinary;
    | Postgres.BIGINT        =>  dt := DataType.BigInt;
    | Postgres.TINYINT       =>  dt := DataType.TinyInt;
    | Postgres.BIT           =>  dt := DataType.Bit;
 *)


    | Postgres.Timestamp     =>  dt := DataType.Timestamp;
    | Postgres.Char, Postgres.Char2, Postgres.Char4,
      Postgres.Char8, Postgres.Char16, Postgres.Bytea,
      Postgres.Bpchar
                             =>  dt := DataType.Char;
    | Postgres.Int           =>  dt := DataType.Integer;
    | Postgres.SmallInt      =>  dt := DataType.SmallInt;
    | Postgres.Float         =>  dt := DataType.Float;
    | Postgres.VarChar       =>  dt := DataType.VarChar;
    | Postgres.Date          =>  dt := DataType.Date;
    | Postgres.Time          =>  dt := DataType.Time;
    | Postgres.Text          =>  dt := DataType.VarChar; 

    ELSE Die (7, "DB.MapDatatype: unknown SQL datatype " & Fmt.Int(ORD(sqltype)));
    END;
    RETURN dt;
  END MapSqlType;

PROCEDURE CheckStmt (st: Stmt;  err: INTEGER;  verb: TEXT;  check_exec := FALSE)
  RAISES {Error} =
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

(*--------------------------------------- misc. DB server information ---*)

PROCEDURE GetDataSources (): DescList  =
  BEGIN
    Unimplemented ("GetDataSources");
    RETURN NIL;
  END GetDataSources;

PROCEDURE GetDrivers (): DescList =
  BEGIN
    Unimplemented("GetDrivers");
    RETURN NIL ;
  END GetDrivers;

(*------------------------------------------------------------- DBRep ---*)

PROCEDURE GetHENV (): NULL =
  BEGIN
    RETURN NIL;
  END GetHENV;

PROCEDURE GetHDBC (t: T): ADDRESS =
  BEGIN
    RETURN t.hdbc;
  END GetHDBC;

PROCEDURE GetHSTMT (st: Stmt): TEXT =
  BEGIN
    RETURN st.hstmt;
  END GetHSTMT;

(*--------------------------------------------- errors and exceptions ---*)

PROCEDURE CheckErr (conn: T) RAISES {Error} =
  VAR
    desc := NEW(ErrorDesc, 
                state := ARRAY OF CHAR {'M', '3', '?', '?', '?', '\000'},
                native_err := -1);
    description: TEXT;
  BEGIN

    CASE PQ.PQstatus (conn.hdbc) OF
    | PQ.CONNECTION.OK => RETURN;
    | PQ.CONNECTION.BAD => description := "bad connection";
    ELSE
      description := "some kind of error in connection";
    END;
    
    CASE conn.last_exec_status OF
    | PQ.PGRS.COMMAND_OK, PQ.PGRS.EMPTY_QUERY,
      PQ.PGRS.TUPLES_OK =>       RETURN;
    | PQ.PGRS.BAD_RESPONSE =>    description := "bad response";
    | PQ.PGRS.FATAL_ERROR =>     description := "fatal error";
    | PQ.PGRS.NONFATAL_ERROR =>  description := "non-fatal error";
    ELSE
      description := "unknown error";
    END;
    
    desc.description := description;
    RAISE Error (desc);
  END CheckErr;

PROCEDURE Die (id: [0..999]; msg: TEXT) RAISES {Error} =
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
    RAISE Error (desc);
  END Die;

(*----------------------------------------- misc. internal functions ---*)


PROCEDURE Unimplemented(<*UNUSED*>msg: TEXT := "") = 
  BEGIN
    (* Quietly return! *)
(*    IO.Put (msg & " is not implemented yet\n"); *)
  END Unimplemented;

PROCEDURE SQL (t: T; query: TEXT): PQ.PGresult_star RAISES {Error} = 
  (* LL = st.mu *)
  VAR result: PQ.PGresult_star;
  BEGIN
    IF Debug THEN IO.Put ("SQL: " & query & "\n") END;
    result := PQ.PQexec (t.hdbc, TtoS(query));
    IF result = NIL THEN 
      RAISE Error (NEW(ErrorDesc,
                       description := CopyStoT(PQ.PQerrorMessage(t.hdbc))));
    END;
    t.last_exec_status := PQ.PQresultStatus(result);
    CheckErr(t);
    RETURN result;
  END SQL;

BEGIN
END DB.
