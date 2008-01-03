(* Copyright (C) 1996-2000, Critical Mass, Inc.  All Rights Reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE ODBCDB EXPORTS ODBCDBRep, ODBCDB;

IMPORT Fmt, Process, SQL, SQLext, Text, WeakRef, Word, M3toC, Ctypes, DB;

FROM SQLtypes IMPORT
  SQLHENV, SQLRETURN, SQLSMALLINT, SQLHDBC, SQLHSTMT, SQLINTEGER,
  SQLUINTEGER, LDOUBLE, DATE_STRUCT, TIME_STRUCT, TIMESTAMP_STRUCT,
  SFLOAT, SWORD, SQLCHAR_star;

FROM DB IMPORT
	Results, ResultDesc, DescList, ColumnDesc, Nullable, DataType, RefString, 
	RefBigInt, RefDate, RefTime, RefTimestamp, ErrorDesc, Error;


(* This module (ab)uses two compiler-dependent features of the code:

|    1) It imports TextF and assumes that TEXT values are implemented
|       as open arrays of CHAR.

|    2) It assumes that 1-D open arrays are implemented as 2-word
|       records (e.g. OpenArrayRep).

*)

TYPE
  OpenArrayRep = RECORD
    data_ptr : ADDRESS;
    n_elts   : INTEGER;
  END;

VAR
  mu   : MUTEX   := NEW (MUTEX);
  henv : SQLHENV := SQL.SQL_NULL_HENV;

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
	RETURN NEW(Interface, name := "ODBC");
END GetInterface;

(*------------------------------------------------------- connections ---*)

TYPE
  T = DB.T OBJECT
    hdbc : SQLHDBC;
  OVERRIDES
    disconnect  := Disconnect;
    new_stmt    := NewStmt;
    auto_commit := AutoCommit;
    commit      := Commit;
    abort       := Abort;
  END;

PROCEDURE Connect (
	<*UNUSED*>this: Interface; 
	database, user_id, password: TEXT; 
	server: TEXT := NIL
): DB.T  RAISES {Error} =
  VAR
    t    := NEW (T);
    err  : SQLRETURN;
    database_c := M3toC.SharedTtoS(database);
    user_id_c  := M3toC.SharedTtoS(user_id);
    password_c := M3toC.SharedTtoS(password);
  BEGIN
    Init ();

    err := SQL.SQLAllocConnect (henv, ADR (t.hdbc));
    IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, NIL, t) END;

    err := SQL.SQLConnect (t.hdbc,
             LOOPHOLE(database_c, SQLCHAR_star),  Text.Length(database),
             LOOPHOLE(user_id_c, SQLCHAR_star),   Text.Length(user_id),
             LOOPHOLE(password_c, SQLCHAR_star),  Text.Length(password));
    M3toC.FreeSharedS(database, database_c);
    M3toC.FreeSharedS(user_id, user_id_c);
    M3toC.FreeSharedS(password, password_c);
    IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, NIL, t); END;

    EVAL WeakRef.FromRef (t, CleanupConnection);
    RETURN t;
  END Connect;

PROCEDURE Disconnect (t: T) RAISES {Error} =
  VAR err: SQLRETURN;
  BEGIN
    IF (t.hdbc = SQL.SQL_NULL_HDBC) THEN
      Die (1, "DB.T is already disconnected.");
    END;
    err := SQL.SQLDisconnect (t.hdbc);
    IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, NIL, t); END;
    err := SQL.SQLFreeConnect (t.hdbc);
    IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, NIL, t); END;
    t.hdbc := SQL.SQL_NULL_HDBC;
  END Disconnect;

PROCEDURE CleanupConnection (<*UNUSED*> READONLY w: WeakRef.T;  ref: REFANY) =
  VAR t := NARROW (ref, T);
  BEGIN
    IF (t.hdbc # SQL.SQL_NULL_HDBC) THEN
      EVAL SQL.SQLDisconnect (t.hdbc);
      EVAL SQL.SQLFreeConnect (t.hdbc);
      t.hdbc := SQL.SQL_NULL_HDBC;
    END;
  END CleanupConnection;

PROCEDURE AutoCommit (t: T;  on: BOOLEAN)  RAISES {Error} =
  CONST flag = ARRAY BOOLEAN OF INTEGER {SQL.SQL_AUTOCOMMIT_OFF,
                                         SQL.SQL_AUTOCOMMIT_ON};
  VAR err : SQLRETURN;
  BEGIN
    IF (t.hdbc = SQL.SQL_NULL_HDBC) THEN
      Die (2, "Attempted to set AutoCommit on a disconnected DB.T.");
    END;
    err := SQL.SQLSetConnectOption (t.hdbc, SQL.SQL_AUTOCOMMIT, flag[on]);
    IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, NIL, t); END;
  END AutoCommit;

PROCEDURE Commit (t: T) RAISES {Error} =
  VAR err : SQLRETURN;
  BEGIN
    IF (t.hdbc = SQL.SQL_NULL_HDBC) THEN
      Die (3, "Attempted to commit a disconnected DB.T.");
    END;
    err := SQL.SQLTransact (henv, t.hdbc, SQL.SQL_COMMIT);
    IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, NIL, t); END;
  END Commit;

PROCEDURE Abort (t: T) RAISES {Error} =
  VAR err : SQLRETURN;
  BEGIN
    IF (t.hdbc = SQL.SQL_NULL_HDBC) THEN
      Die (4, "Attempted to abort a disconnected DB.T.");
    END;
    err := SQL.SQLTransact (henv, t.hdbc, SQL.SQL_ROLLBACK);
    IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, NIL, t); END;
  END Abort;

PROCEDURE NewStmt (t: T): DB.Stmt RAISES {Error} =
  VAR
    st := NEW (Stmt);
    err : SQLRETURN;
  BEGIN
    IF (t.hdbc = SQL.SQL_NULL_HDBC) THEN
      Die (5, "Attempted to create a new statement on a disconnected DB.T.");
    END;

    st.conn     := t;
    st.hstmt    := SQL.SQL_NULL_HSTMT;
    st.prepared := FALSE;
    st.executed := FALSE;

    err := SQL.SQLAllocStmt (t.hdbc, ADR (st.hstmt));
    IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;

    EVAL WeakRef.FromRef (st, CleanupStmt);
    RETURN st;
  END NewStmt;

PROCEDURE CleanupStmt (<*UNUSED*> READONLY wr: WeakRef.T;  ref: REFANY) =
  VAR st := NARROW (ref, Stmt);
  BEGIN
    IF (st.hstmt # SQL.SQL_NULL_HSTMT) THEN
      IF (st.conn # NIL) AND (st.conn.hdbc # SQL.SQL_NULL_HDBC) THEN
        EVAL SQL.SQLFreeStmt (st.hstmt, SQL.SQL_DROP);
        (* otherwise, the DB.T connection is already broken... *)
      END;
      DisposeStmt (st);
    END;
  END CleanupStmt;

PROCEDURE DisposeStmt (st: Stmt) =
  BEGIN
    CleanValueInfo (st);
    st.conn     := NIL;
    st.hstmt    := SQL.SQL_NULL_HSTMT;
    st.prepared := FALSE;
    st.executed := FALSE;
    st.col_info := NIL;
    st.values   := NIL;
    st.val_info := NIL;
    IF (st.buffer # NIL) THEN DISPOSE (st.buffer) END;
  END DisposeStmt;

PROCEDURE CleanValueInfo (st: Stmt) =
  BEGIN
    IF (st.val_info # NIL) THEN
      (* free any existing string buffers *)
      FOR i := 0 TO LAST (st.val_info^) DO
        WITH z = st.val_info[i] DO
          IF (z.buffer # NIL) THEN DISPOSE (z.buffer); END;
          z.buffer := NIL;
        END;
      END;
    END;
  END CleanValueInfo;

(*-------------------------------------------------------- statements ---*)

TYPE
  (* a SQL database statement (query or update) *)
  Stmt = DB.Stmt OBJECT
    conn          : T;  (* my database connection *)
    hstmt         : SQLHSTMT;
    prepared      : BOOLEAN;
    executed      : BOOLEAN;
    col_info      : ResultDesc;
    values        : Results;
    val_info      : ValueDesc;
    buffer        : Buffer;
    first_getdata : INTEGER;
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

PROCEDURE Prepare (st: Stmt;  operation: TEXT) RAISES {Error} =
  VAR 
    err: SQLRETURN;
    operation_c := M3toC.SharedTtoS(operation);
  BEGIN
    LOCK st DO
      CheckStmt (st, 12, "prepare", check_exec := FALSE);
      err := SQL.SQLPrepare (st.hstmt, LOOPHOLE(operation_c, SQLCHAR_star), 
                             Text.Length (operation));
      M3toC.FreeSharedS(operation, operation_c);
      IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
      st.prepared := TRUE;
      st.executed := FALSE;
      st.col_info := NIL;
      st.values   := NIL;
    END;
  END Prepare;

PROCEDURE Execute (st: Stmt;  operation: TEXT) RAISES {Error} =
  VAR
    err: SQLRETURN;
    operation_c: Ctypes.char_star;
  BEGIN
    LOCK st DO
      CheckStmt (st, 15, "execute", check_exec := FALSE);
      IF (operation = NIL) THEN
        (* use the prepared statement *)
        IF (NOT st.prepared) THEN
          Die (6, "Attempted to execute an unspecified or prepared DB.Stmt");
        END;
        err := SQL.SQLExecute (st.hstmt);
        IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
      ELSE
        st.prepared := FALSE;
        operation_c := M3toC.SharedTtoS(operation);
        err := SQL.SQLExecDirect (st.hstmt, LOOPHOLE(operation_c, SQLCHAR_star),
                                  Text.Length (operation));
        M3toC.FreeSharedS(operation, operation_c);
        IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
      END;
      st.executed := TRUE;
      st.col_info := NIL;
      st.values   := NIL;
    END;
  END Execute;

PROCEDURE Fetch (st: Stmt): Results RAISES {Error} =
  VAR err: SQLRETURN;
  BEGIN
    LOCK st DO
      CheckStmt (st, 18, "fetch from", check_exec := TRUE);
      IF (st.values = NIL) THEN BuildValueArea (st); END;
      err := SQL.SQLFetch (st.hstmt);
      IF (err = SQL.SQL_SUCCESS) THEN RETURN MapValues (st); END;
      IF (err = SQL.SQL_NO_DATA_FOUND) THEN RETURN NIL; END;
      CheckErr (err, st);
    END;
    RETURN NIL;
  END Fetch;

TYPE
  Buffer = UNTRACED REF ARRAY OF CHAR;

TYPE
  ValueDesc = REF ARRAY OF ValueInfo;
  ValueInfo = RECORD
    ref      : REFANY;
    ptr      : ADDRESS;  (* == ADR (receive_buffer [offset]) *)
    offset   : INTEGER;  (* byte offset into the receive buffer *)
    datatype : DataType;
    buffer   : Buffer;   (* holding area for string data *)
  END;

(* We don't seem to be able to get reliable size data for string
   or binary fields from the ODBC drivers.  Perhaps we (Farshad & Bill)
   don't understand the spec.  So, we don't call SQLBindCol() to fix their
   locations, instead we use SQLGetData() to retrieve the string in chunks.  *)

(* These types define the layout of the receive buffer for each DataType. *)
TYPE
  (* => Char, VarChar, LongVarChar, Binary, VarBinary, LongVarBinary *)
  StringPtr = UNTRACED REF StringVal;
  StringVal = OpenArrayRep;

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

  (* => TimeStamp *)
  TimeStampPtr = UNTRACED REF TimeStampVal;
  TimeStampVal = RECORD
    data_len : SQLINTEGER;
    value    : TIMESTAMP_STRUCT;
  END;

PROCEDURE BuildValueArea (st: Stmt) RAISES {Error} =
  (* LL = st.mu *)
  CONST
    Mask = Word.Not (BYTESIZE (LONGREAL) - 1);
    Bump = BYTESIZE (LONGREAL) - 1;
  VAR
    next_buf     : INTEGER;
    len          : INTEGER;
    err          : SQLRETURN;
    seen_getdata : BOOLEAN;
  BEGIN
    IF (st.col_info = NIL) THEN BuildColumnInfo (st); END;
    st.values   := NEW (Results, NUMBER (st.col_info^));
    st.val_info := NEW (ValueDesc, NUMBER (st.col_info^));
    st.first_getdata := NUMBER(st.col_info^);

    (* assign buffer offsets *)
    next_buf := 0;
    seen_getdata := FALSE;
    FOR i := 0 TO LAST (st.values^) DO
      WITH z = st.val_info[i] DO
        next_buf   := Word.And (next_buf + Bump, Mask); (* keep it aligned! *)
        z.offset   := next_buf;
        z.datatype := st.col_info[i].type;
        CASE z.datatype OF
        | DataType.Null          => len := 0;
        | DataType.Char,
          DataType.Binary,
          DataType.VarChar,
          DataType.VarBinary,
          DataType.LongVarChar,
          DataType.LongVarBinary => len := BYTESIZE (StringVal); 
                                    IF NOT seen_getdata THEN
                                       seen_getdata := TRUE;
                                       st.first_getdata := i;
                                    END;
        | DataType.Numeric,
          DataType.Decimal,
          DataType.Float,
          DataType.Double        => len := BYTESIZE (FloatVal);
        | DataType.BigInt        => len := BYTESIZE (BigIntVal);
        | DataType.Integer,
          DataType.SmallInt,
          DataType.TinyInt       => len := BYTESIZE (IntVal);
        | DataType.Real          => len := BYTESIZE (RealVal);
        | DataType.Bit           => len := BYTESIZE (BitVal);
        | DataType.Date          => len := BYTESIZE (DateVal);
        | DataType.Time          => len := BYTESIZE (TimeVal);
        | DataType.Timestamp     => len := BYTESIZE (TimeStampVal);
        END;
        INC (next_buf, len);
      END; (* WITH z *)
    END;

    (* release any existing bindings so they don't confuse us *)
    IF (st.buffer # NIL) THEN
      err := SQL.SQLFreeStmt (st.hstmt, SQL.SQL_UNBIND);
      IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
    END;

    (* free any existing string buffers *)
    CleanValueInfo (st);

    (* allocate an immovable buffer to hold the incoming data *)
    IF (st.buffer = NIL) OR (BYTESIZE (st.buffer^) < next_buf) THEN
      IF (st.buffer # NIL) THEN DISPOSE (st.buffer); END;
      st.buffer := NEW (Buffer, Word.And (next_buf + 16_fff, Word.Not (16_fff)));
    END;


    (* Allocate all fields. *)

    FOR i := 0  TO LAST(st.values^) DO
      WITH  z = st.val_info[i] DO
        z.ptr := ADR (st.buffer [z.offset]);
        CASE z.datatype OF
        | DataType.Null =>                z.ref := NIL;
        | DataType.Char,
          DataType.Binary,
          DataType.VarChar,
          DataType.VarBinary,
          DataType.LongVarChar,
          DataType.LongVarBinary =>       z.ref := NEW (RefString);
        | DataType.BigInt =>              z.ref := NEW (RefBigInt);
        | DataType.Integer,
          DataType.SmallInt,
          DataType.TinyInt =>             z.ref := NEW (REF INTEGER);
        | DataType.Numeric,
          DataType.Decimal,
          DataType.Float,
          DataType.Double =>              z.ref := NEW (REF LONGREAL);
        | DataType.Real =>                z.ref := NEW (REF REAL);
        | DataType.Bit =>                 z.ref := NEW (REF BOOLEAN);
        | DataType.Date =>                z.ref := NEW (RefDate);
        | DataType.Time =>                z.ref := NEW (RefTime);
        | DataType.Timestamp =>           z.ref := NEW (RefTimestamp);
        END; (* CASE *)
      END; (* WITH z *)
    END;

    (* Bind the result columns to their location in the buffer.
       We only bind until we see the first column that requires
       a SQLGetData, to workaround a restriction in some ODBC drivers.
       The rest of the columns will be mapped using SQLGetData
       in MapValues. *)

    FOR i := 0 TO st.first_getdata-1 DO
      WITH  z = st.val_info[i],  col = i+1 DO
        z.ptr := ADR (st.buffer [z.offset]);
        CASE z.datatype OF
        | DataType.Null =>  (* do nothing *)

        | DataType.Char,
          DataType.Binary,
          DataType.VarChar,
          DataType.VarBinary,
          DataType.LongVarChar,
          DataType.LongVarBinary =>

	    <* ASSERT FALSE *> 
            (* We must not have hit a string in this loop. *)
		
        | DataType.BigInt =>
            VAR big: BigIntPtr := z.ptr;  BEGIN
              err := SQL.SQLBindCol (st.hstmt, col, SQL.SQL_BIGINT,
                     ADR (big.value), BYTESIZE (big.value), ADR (big.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
            END;

        | DataType.Integer,
          DataType.SmallInt,
          DataType.TinyInt =>
            VAR int: IntPtr := z.ptr;  BEGIN
              err := SQL.SQLBindCol (st.hstmt, col, SQL.SQL_C_SLONG,
                     ADR (int.value), BYTESIZE (int.value), ADR (int.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
            END;

        | DataType.Numeric,
          DataType.Decimal,
          DataType.Float,
          DataType.Double =>
            VAR flt: FloatPtr := z.ptr;  BEGIN
              err := SQL.SQLBindCol (st.hstmt, col, SQL.SQL_C_DOUBLE,
                     ADR (flt.value), BYTESIZE (flt.value), ADR (flt.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
            END;

        | DataType.Real =>
            VAR flt: RealPtr := z.ptr;  BEGIN
              err := SQL.SQLBindCol (st.hstmt, col, SQL.SQL_C_FLOAT,
                     ADR (flt.value), BYTESIZE (flt.value), ADR (flt.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
            END;

        | DataType.Bit =>
            VAR bit: BitPtr := z.ptr;  BEGIN
              err := SQL.SQLBindCol (st.hstmt, col, SQL.SQL_C_SHORT,
                     ADR (bit.value), BYTESIZE (bit.value), ADR (bit.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
            END;

        | DataType.Date =>
            VAR dat: DatePtr := z.ptr;  BEGIN
              err := SQL.SQLBindCol (st.hstmt, col, SQL.SQL_C_DATE,
                     ADR (dat.value), BYTESIZE (dat.value), ADR (dat.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
            END;

        | DataType.Time =>
            VAR tim: TimePtr := z.ptr;  BEGIN
              err := SQL.SQLBindCol (st.hstmt, col, SQL.SQL_C_TIME,
                     ADR (tim.value), BYTESIZE (tim.value), ADR (tim.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
            END;

        | DataType.Timestamp =>
            VAR ts: TimeStampPtr := z.ptr;  BEGIN
              err := SQL.SQLBindCol (st.hstmt, col, SQL.SQL_C_TIMESTAMP,
                     ADR (ts.value), BYTESIZE (ts.value), ADR (ts.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
            END;

        END; (* CASE *)
      END; (* WITH z *)
    END;

  END BuildValueArea;

PROCEDURE MapValues (st: Stmt): Results  RAISES {Error} =
  (* Convert each value in the result buffer into its Modula-3 REF value.
     Detect and return NIL for nullified values.  LL = st.mu *)
  VAR ref: REFANY;
  VAR err: SQLRETURN;
  BEGIN


    (* First, map all the values up that were bound using SQLBindData. *)

    FOR i := 0 TO st.first_getdata-1 DO
      WITH  z = st.val_info[i] DO
        ref := NIL;
        CASE z.datatype OF
        | DataType.Null =>
            (* no value *)
            ref := NIL;

        | DataType.Char,
          DataType.Binary,
          DataType.VarChar,
          DataType.VarBinary,
          DataType.LongVarChar,
          DataType.LongVarBinary =>
            ref := z.ref; 

        | DataType.BigInt =>
            VAR big: BigIntPtr := z.ptr;  rr: RefBigInt;  BEGIN
              IF (big.data_len > 0) THEN
                rr    := z.ref;
                rr.lo := big.value.lo;
                rr.hi := big.value.hi;
                ref   := rr;
              END;
            END;

        | DataType.Integer,
          DataType.SmallInt,
          DataType.TinyInt =>
            VAR int: IntPtr := z.ptr;  rr: REF INTEGER;  BEGIN
              IF (int.data_len > 0) THEN
                rr  := z.ref;
                rr^ := int.value;
                ref := rr;
              END;
            END;

        | DataType.Numeric,
          DataType.Decimal,
          DataType.Float,
          DataType.Double =>
            VAR flt: FloatPtr := z.ptr;  rr: REF LONGREAL;  BEGIN
              IF (flt.data_len > 0) THEN
                rr  := z.ref;
                rr^ := flt.value;
                ref := rr;
              END;
            END;

        | DataType.Real =>
            VAR flt: RealPtr := z.ptr;  rr: REF REAL;  BEGIN
              IF (flt.data_len > 0) THEN
                rr  := z.ref;
                rr^ := flt.value;
                ref := rr;
              END;
            END;

        | DataType.Bit =>
            VAR bit: BitPtr := z.ptr;  rr: REF BOOLEAN;  BEGIN
              IF (bit.data_len > 0) THEN
                rr  := z.ref;
                rr^ := (bit.value # 0);
                ref := rr;
              END;
            END;

        | DataType.Date =>
            VAR dat: DatePtr := z.ptr;  rr: RefDate;  BEGIN
              IF (dat.data_len > 0) THEN
                rr       := z.ref;
                rr.year  := dat.value.year;
                rr.month := dat.value.month;
                rr.day   := dat.value.day;
                ref      := rr;
              END;
            END;

        | DataType.Time =>
            VAR tim: TimePtr := z.ptr;  rr: RefTime;  BEGIN
              IF (tim.data_len > 0) THEN
                rr        := z.ref;
                rr.hour   := tim.value.hour;
                rr.minute := tim.value.minute;
                rr.second := tim.value.second;
                ref       := rr;
              END;
            END;

        | DataType.Timestamp =>
            VAR ts: TimeStampPtr := z.ptr;  rr: RefTimestamp;  BEGIN
              IF (ts.data_len > 0) THEN
                rr          := z.ref;
                rr.year     := ts.value.year;
                rr.month    := ts.value.month;
                rr.day      := ts.value.day;
                rr.hour     := ts.value.hour;
                rr.minute   := ts.value.minute;
                rr.second   := ts.value.second;
                rr.fraction := ts.value.fraction;
                ref         := rr;
              END;
            END;

        END; (* CASE *)
        st.values [i] := ref;
      END; (* WITH z *)
    END;
    
    (* Map all the values that use GetData. *)

    FOR i := st.first_getdata TO LAST (st.values^) DO

      WITH  z = st.val_info[i], col = i+1  DO
        ref := NIL;
        CASE z.datatype OF
        | DataType.Null =>
            (* no value *)
            ref := NIL;


        | DataType.Char,
          DataType.Binary,
          DataType.VarChar,
          DataType.VarBinary,
          DataType.LongVarChar,
          DataType.LongVarBinary =>
	    MapString (st, z, col);
            ref := z.ref; 

        | DataType.BigInt =>
            VAR big: BigIntPtr := z.ptr;  rr: RefBigInt;  BEGIN
              err := SQL.SQLGetData (st.hstmt, col, SQL.SQL_BIGINT,
                     ADR (big.value), BYTESIZE (big.value), ADR (big.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
              IF (big.data_len > 0) THEN
                rr    := z.ref;
                rr.lo := big.value.lo;
                rr.hi := big.value.hi;
                ref   := rr;
              END;
            END;

        | DataType.Integer,
          DataType.SmallInt,
          DataType.TinyInt =>
            VAR int: IntPtr := z.ptr;  rr: REF INTEGER;  BEGIN
              err := SQL.SQLGetData (st.hstmt, col, SQL.SQL_C_SLONG,
                     ADR (int.value), BYTESIZE (int.value), ADR (int.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
              IF (int.data_len > 0) THEN
                rr  := z.ref;
                rr^ := int.value;
                ref := rr;
              END;
            END;

        | DataType.Numeric,
          DataType.Decimal,
          DataType.Float,
          DataType.Double =>
            VAR flt: FloatPtr := z.ptr;  rr: REF LONGREAL;  BEGIN
              err := SQL.SQLGetData (st.hstmt, col, SQL.SQL_C_DOUBLE,
                     ADR (flt.value), BYTESIZE (flt.value), ADR (flt.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
              IF (flt.data_len > 0) THEN
                rr  := z.ref;
                rr^ := flt.value;
                ref := rr;
              END;
            END;

        | DataType.Real =>
            VAR flt: RealPtr := z.ptr;  rr: REF REAL;  BEGIN
              err := SQL.SQLGetData (st.hstmt, col, SQL.SQL_C_FLOAT,
                     ADR (flt.value), BYTESIZE (flt.value), ADR (flt.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
              IF (flt.data_len > 0) THEN
                rr  := z.ref;
                rr^ := flt.value;
                ref := rr;
              END;
            END;

        | DataType.Bit =>
            VAR bit: BitPtr := z.ptr;  rr: REF BOOLEAN;  BEGIN
              err := SQL.SQLGetData (st.hstmt, col, SQL.SQL_C_SHORT,
                     ADR (bit.value), BYTESIZE (bit.value), ADR (bit.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
              IF (bit.data_len > 0) THEN
                rr  := z.ref;
                rr^ := (bit.value # 0);
                ref := rr;
              END;
            END;

        | DataType.Date =>
            VAR dat: DatePtr := z.ptr;  rr: RefDate;  BEGIN
              IF (dat.data_len > 0) THEN
              err := SQL.SQLGetData (st.hstmt, col, SQL.SQL_C_DATE,
                     ADR (dat.value), BYTESIZE (dat.value), ADR (dat.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
                rr       := z.ref;
                rr.year  := dat.value.year;
                rr.month := dat.value.month;
                rr.day   := dat.value.day;
                ref      := rr;
              END;
            END;

        | DataType.Time =>
            VAR tim: TimePtr := z.ptr;  rr: RefTime;  BEGIN
              err := SQL.SQLGetData (st.hstmt, col, SQL.SQL_C_TIME,
                     ADR (tim.value), BYTESIZE (tim.value), ADR (tim.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
              IF (tim.data_len > 0) THEN
                rr        := z.ref;
                rr.hour   := tim.value.hour;
                rr.minute := tim.value.minute;
                rr.second := tim.value.second;
                ref       := rr;
              END;
            END;

        | DataType.Timestamp =>
            VAR ts: TimeStampPtr := z.ptr;  rr: RefTimestamp;  BEGIN
              err := SQL.SQLGetData (st.hstmt, col, SQL.SQL_C_TIMESTAMP,
                     ADR (ts.value), BYTESIZE (ts.value), ADR (ts.data_len));
              IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
              IF (ts.data_len > 0) THEN
                rr          := z.ref;
                rr.year     := ts.value.year;
                rr.month    := ts.value.month;
                rr.day      := ts.value.day;
                rr.hour     := ts.value.hour;
                rr.minute   := ts.value.minute;
                rr.second   := ts.value.second;
                rr.fraction := ts.value.fraction;
                ref         := rr;
              END;
            END;

        END; (* CASE *)
        st.values [i] := ref;
      END; (* WITH z *)
    END;

    RETURN st.values;
  END MapValues;

TYPE
  RcvBuffer = ARRAY [0..4095] OF CHAR;

PROCEDURE MapString (st: Stmt;  VAR z: ValueInfo;  col: CARDINAL) RAISES {Error} =
  VAR
    rr     : RefString := z.ref;
    str    : StringPtr := z.ptr;
    offset : CARDINAL  := 0;
    len    : SQLINTEGER;
    err    : INTEGER;
    buf    : RcvBuffer;
  BEGIN
    LOOP 
      err := SQL.SQLGetData (st.hstmt, col, SQL.SQL_C_BINARY,
                             ADR(buf[0]), BYTESIZE (buf), ADR (len));


		
      IF (err = SQL.SQL_SUCCESS) THEN
        (* ok, got all the remaining data *)
        CopyData (z, offset, buf, len);
        EXIT;
      ELSIF (err = SQL.SQL_SUCCESS_WITH_INFO)
        AND ((len > BYTESIZE (buf)) OR (len = SQL.SQL_NO_TOTAL)) THEN
        (* assume it's a "data truncated" error and continue... *)
        CopyData (z, offset, buf, len);
      ELSE (* must be a "real" error *)
        CheckErr (err, st);
      END;
    END;

    IF (z.buffer = NIL) THEN
      str.data_ptr := NIL;
      str.n_elts   := 0;
    ELSE
      str.data_ptr := ADR (z.buffer[0]);
      str.n_elts   := offset;
    END;

    rr^ := LOOPHOLE (str.data_ptr, UNTRACED REF ARRAY OF CHAR); (* Hacked! *)
    (* rr^ := ADR(str^) also works, but is more puzzling. *)


  END MapString;

PROCEDURE CopyData (VAR z: ValueInfo;  VAR offset: CARDINAL;
                    READONLY buf: RcvBuffer;  len: INTEGER) =
  BEGIN
    IF (len = SQL.SQL_NO_TOTAL) THEN
      (* ODBC isn't telling how much data we're going to get! *)
      len := BYTESIZE (buf);
    END;

    IF (z.buffer = NIL) OR (NUMBER (z.buffer^) < offset + len) THEN
      IF (z.buffer # NIL) THEN DISPOSE (z.buffer); END;
      z.buffer := NEW (Buffer, Word.And (offset + len + 16_ff, Word.Not (16_ff)));
    END;

    len := MAX (0, MIN (len, BYTESIZE (buf)));
    SUBARRAY (z.buffer^, offset, len) := SUBARRAY (buf, 0, len);
    INC (offset, len);
  END CopyData;

PROCEDURE Done (st: Stmt) RAISES {Error} =
  VAR err: SQLRETURN;
  BEGIN
    LOCK st DO
      CheckStmt (st, 21, "finish", check_exec := FALSE);
      err := SQL.SQLFreeStmt (st.hstmt, SQL.SQL_CLOSE);
      IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
      st.prepared := FALSE;
      st.executed := FALSE;
    END;
  END Done;

PROCEDURE Close (st: Stmt) RAISES {Error} =
  VAR err: SQLRETURN;
  BEGIN
    LOCK st DO
      CheckStmt (st, 24, "close", check_exec := FALSE);
      err := SQL.SQLFreeStmt (st.hstmt, SQL.SQL_DROP);
      IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
      DisposeStmt (st);
    END;
  END Close;

PROCEDURE GetCursorName (st: Stmt): TEXT RAISES {Error} =
  VAR err: SQLRETURN;  len: SQLSMALLINT;  buf: ARRAY [0..255] OF CHAR;
  BEGIN
    LOCK st DO
      CheckStmt (st, 27, "get the cursor name from", check_exec := FALSE);
      err := SQL.SQLGetCursorName (st.hstmt, ADR (buf[0]), BYTESIZE (buf),
                                   ADR (len));
      IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
    END;
    len := MAX (0, MIN (len, NUMBER (buf)));
    RETURN Text.FromChars (SUBARRAY (buf, 0, len));
  END GetCursorName;

PROCEDURE SetCursorName (st: Stmt;  nm: TEXT) RAISES {Error} =
  VAR 
    err: SQLRETURN;
    nm_c := M3toC.SharedTtoS(nm);
  BEGIN
    LOCK st DO
      CheckStmt (st, 30, "set the cursor name in", check_exec := FALSE);
      err := SQL.SQLSetCursorName (st.hstmt, LOOPHOLE(nm_c, SQLCHAR_star),
                                   Text.Length (nm));
      M3toC.FreeSharedS(nm, nm_c);
      IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
    END;
  END SetCursorName;

PROCEDURE NumRows (st: Stmt): INTEGER RAISES {Error} =
  VAR err: SQLRETURN;  cnt: SQLINTEGER;
  BEGIN
    LOCK st DO
      CheckStmt (st, 33, "get the row count from", check_exec := TRUE);
      err := SQL.SQLRowCount (st.hstmt, ADR (cnt));
      IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
    END;
    RETURN cnt;
  END NumRows;

PROCEDURE DescribeResult (st: Stmt): ResultDesc  RAISES {Error} =
  VAR res: ResultDesc;
  BEGIN
    LOCK st DO
      CheckStmt (st, 36, "get the result description from", check_exec := TRUE);
      IF (st.col_info = NIL) THEN BuildColumnInfo (st); END;
      res := NEW (ResultDesc, NUMBER (st.col_info^));
      res^ := st.col_info^;
    END;
    RETURN res; (* we return a fresh copy so the client can't screw up our copy. *)
  END DescribeResult;

PROCEDURE BuildColumnInfo (st: Stmt) RAISES {Error} =
  (* LL = st.mu *)
  VAR
    err     : SQLRETURN;
    cnt     : SQLSMALLINT;
    nm      : ARRAY [0..255] OF CHAR;
    nm_len  : SQLSMALLINT;
    sqltype : SQLSMALLINT;
    coldef  : SQLUINTEGER;
    scale   : SQLSMALLINT;
    nullable: SQLSMALLINT;
  BEGIN
    IF (st.col_info # NIL) THEN RETURN; END;

    err := SQL.SQLNumResultCols (st.hstmt, ADR (cnt));
    IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
    st.col_info := NEW (ResultDesc, cnt);

    FOR i := 0 TO cnt-1 DO
      err := SQL.SQLDescribeCol (st.hstmt, i+1,
                                 ADR (nm[0]), BYTESIZE (nm), ADR (nm_len),
                                 ADR (sqltype), ADR(coldef), ADR(scale),
                                 ADR (nullable));
      IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, st); END;
      nm_len := MAX (0, MIN (nm_len, NUMBER (nm)));
      WITH z = st.col_info[i] DO
        z.name      := Text.FromChars (SUBARRAY (nm, 0, nm_len));
        z.type      := MapSqlType (sqltype);
        z.precision := coldef;
        z.scale     := scale;
        z.nullable  := MapNullable (nullable);
      END;
    END;
  END BuildColumnInfo;

PROCEDURE MapSqlType (sqltype: INTEGER): DataType RAISES {Error} =
  VAR dt: DataType;
  BEGIN
    CASE sqltype OF
    | SQL.SQL_TYPE_NULL     =>  dt := DataType.Null;
    | SQL.SQL_CHAR          =>  dt := DataType.Char;
    | SQL.SQL_NUMERIC       =>  dt := DataType.Numeric;
    | SQL.SQL_DECIMAL       =>  dt := DataType.Decimal;
    | SQL.SQL_INTEGER       =>  dt := DataType.Integer;
    | SQL.SQL_SMALLINT      =>  dt := DataType.SmallInt;
    | SQL.SQL_FLOAT         =>  dt := DataType.Float;
    | SQL.SQL_REAL          =>  dt := DataType.Real;
    | SQL.SQL_DOUBLE        =>  dt := DataType.Double;
    | SQL.SQL_VARCHAR       =>  dt := DataType.VarChar;
    | SQL.SQL_DATE          =>  dt := DataType.Date;
    | SQL.SQL_TIME          =>  dt := DataType.Time;
    | SQL.SQL_TIMESTAMP     =>  dt := DataType.Timestamp;
    | SQL.SQL_LONGVARCHAR   =>  dt := DataType.LongVarChar;
    | SQL.SQL_BINARY        =>  dt := DataType.Binary;
    | SQL.SQL_VARBINARY     =>  dt := DataType.VarBinary;
    | SQL.SQL_LONGVARBINARY =>  dt := DataType.LongVarBinary;
    | SQL.SQL_BIGINT        =>  dt := DataType.BigInt;
    | SQL.SQL_TINYINT       =>  dt := DataType.TinyInt;
    | SQL.SQL_BIT           =>  dt := DataType.Bit;
    ELSE Die (7, "DB.MapDatatype: unknown SQL datatype: " & Fmt.Int(ORD(sqltype)));
    END;
    RETURN dt;
  END MapSqlType;

PROCEDURE MapNullable (nullable: INTEGER): Nullable RAISES {Error} =
  VAR nn: Nullable;
  BEGIN
    IF    (nullable = SQL.SQL_NULLABLE)          THEN nn := Nullable.Yes;
    ELSIF (nullable = SQL.SQL_NO_NULLS)          THEN nn := Nullable.No;
    ELSIF (nullable = SQL.SQL_NULLABLE_UNKNOWN)  THEN nn := Nullable.Unknown;
    ELSE Die (8, "DB.MapNullable:  unknown 'nullable' value");
    END;
    RETURN nn;
  END MapNullable;

PROCEDURE CheckStmt (st: Stmt;  err: INTEGER;  verb: TEXT;  check_exec := FALSE)
  RAISES {Error} =
  BEGIN
    IF (st.hstmt = SQL.SQL_NULL_HSTMT) THEN
      Die (err, "Attempted to " & verb & " a closed DB.Stmt");
    END;
    IF (check_exec) AND (NOT st.executed) THEN
      Die (err+1, "Attempted to " & verb & " an unexecuted DB.Stmt");
    END;
    IF (st.conn = NIL) OR (st.conn.hdbc = SQL.SQL_NULL_HDBC) THEN
      Die (err+2, "Attempted to " & verb  & " a DB.Stmt on a disconnected DB.T.");
    END;
  END CheckStmt;

(*--------------------------------------- misc. DB server information ---*)

PROCEDURE GetDataSources (<*UNUSED*>this: Interface): DescList  RAISES {Error} =
  VAR
    direction := SQL.SQL_FETCH_FIRST;
    err       : SQLRETURN;
    srclen    : SQLSMALLINT;
    desclen   : SQLSMALLINT;
    source    : ARRAY [0..255] OF CHAR;
    desc      : ARRAY [0..255] OF CHAR;
    results   : DescList := NIL;
    a, b      : DescList;
  BEGIN
    Init ();

    LOOP
      err := SQL.SQLDataSources (henv, direction,
                                 ADR (source[0]), BYTESIZE (source), ADR(srclen),
                                 ADR (desc[0]), BYTESIZE (desc), ADR(desclen));
      IF (err = SQL.SQL_NO_DATA_FOUND) THEN EXIT; END;
      IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, NIL); END;
      a := NEW (DescList);
      srclen := MAX (0, MIN (srclen, NUMBER (source)));
      a.name := Text.FromChars (SUBARRAY (source, 0, srclen));
      desclen := MAX (0, MIN (desclen, NUMBER (source)));
      a.description := Text.FromChars (SUBARRAY (desc, 0, desclen));
      a.next := results;  results := a;
      direction := SQL.SQL_FETCH_NEXT;
    END; (*loop*)

    (* put the results back in the order that the database returned them... *)
    a := results;  b := NIL;  results := NIL;
    WHILE (a # NIL) DO
      b := a.next;
      a.next := results;
      results := a;
      a := b;
    END;

    RETURN results;
  END GetDataSources;

PROCEDURE GetDrivers (<*UNUSED*>this: Interface): DescList  RAISES {Error} =
  VAR
    direction := SQL.SQL_FETCH_FIRST;
    err       : SQLRETURN;
    srclen    : SQLSMALLINT;
    desclen   : SQLSMALLINT;
    source    : ARRAY [0..255] OF CHAR;
    desc      : ARRAY [0..255] OF CHAR;
    results   : DescList := NIL;
    a, b      : DescList;
  BEGIN
    Init ();

    LOOP
      err := SQLext.SQLDrivers (henv, direction,
                                ADR (source[0]), BYTESIZE (source), ADR(srclen),
                                ADR (desc[0]), BYTESIZE (desc), ADR(desclen));
      IF (err = SQL.SQL_NO_DATA_FOUND) THEN EXIT; END;
      IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, NIL); END;
      a := NEW (DescList);
      srclen := MAX (0, MIN (srclen, NUMBER (source)));
      a.name := Text.FromChars (SUBARRAY (source, 0, srclen));
      desclen := MAX (0, MIN (desclen, NUMBER (source)));
      a.description := Text.FromChars (SUBARRAY (desc, 0, desclen));
      a.next := results;  results := a;
      direction := SQL.SQL_FETCH_NEXT;
    END; (*loop*)

    (* put the results back in the order that the database returned them... *)
    a := results;  b := NIL;  results := NIL;
    WHILE (a # NIL) DO
      b := a.next;
      a.next := results;
      results := a;
      a := b;
    END;

    RETURN results;
  END GetDrivers;

(*------------------------------------------------------------- DBRep ---*)

PROCEDURE GetHENV (): SQLHENV =
  BEGIN
    RETURN henv;
  END GetHENV;

PROCEDURE GetHDBC (t: DB.T): SQLHDBC =
  BEGIN
    RETURN NARROW(t, T).hdbc;
  END GetHDBC;

PROCEDURE GetHSTMT (st: DB.Stmt): SQLHSTMT =
  BEGIN
    RETURN NARROW(st, Stmt).hstmt;
  END GetHSTMT;

(*--------------------------------------------- errors and exceptions ---*)

PROCEDURE CheckErr (err: SQLRETURN;  stmt: Stmt;  conn: T := NIL) RAISES {Error} =
  VAR
    HDBC  := SQL.SQL_NULL_HDBC;
    hstmt := SQL.SQL_NULL_HSTMT;
    desc  := NEW (ErrorDesc);
    msg   : ARRAY [0..255] OF CHAR;
    len   : SQLSMALLINT;
    xxx   : SQLRETURN;
  BEGIN
    IF (err = SQL.SQL_SUCCESS) OR (err = SQL.SQL_SUCCESS_WITH_INFO) THEN
      RETURN;
    END;
    IF (stmt # NIL) THEN  hstmt := stmt.hstmt;  conn := stmt.conn;  END;
    IF (conn # NIL) THEN  HDBC := conn.hdbc;  END;
    xxx := SQL.SQLError (henv, HDBC, hstmt, ADR (desc.state), ADR (desc.native_err),
                         ADR (msg[0]), BYTESIZE (msg), ADR (len));
    IF (len > 0) THEN
      len := MIN (len, NUMBER (msg));
      desc.description := Text.FromChars (SUBARRAY (msg, 0, len));
    END;
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
    desc.description := "[Modula-3 DB]" & msg;
    RAISE Error (desc);
  END Die;

(*----------------------------------------- misc. internal functions ---*)

PROCEDURE Init () RAISES {Error} =
  VAR err: SQLRETURN;
  BEGIN
    IF (henv = SQL.SQL_NULL_HENV) THEN
      LOCK mu DO
        IF (henv = SQL.SQL_NULL_HENV) THEN
          err := SQL.SQLAllocEnv (ADR (henv));
          IF (err # SQL.SQL_SUCCESS) THEN CheckErr (err, NIL); END;
          Process.RegisterExitor (ShutDown);
        END;
      END;
    END;
  END Init;

PROCEDURE ShutDown () =
  BEGIN
    IF (henv # SQL.SQL_NULL_HENV) THEN
      EVAL SQL.SQLFreeEnv (henv);  (* ignore the errors! *)
      henv := SQL.SQL_NULL_HENV;
    END;
  END ShutDown;

BEGIN
END ODBCDB.
