(* Copyright (C) 1996, Critical Mass, Inc.  All Rights Reserved. *)
(* See the file COPYRIGHT.CM for a full description              *)

(* 
   DB is the safe Modula-3 interface for relational databases.  It
   defines two primary abstractions: database connections and
   statements.
*)

INTERFACE DB;

(*------------------------------------------------------- connections ---*)
(*
   A "DB.T", or connection, represents a single connection to a database.
   Multiple connections may exist within one application and each may be
   used concurrently by multiple threads.
*)

PROCEDURE Connect (database, user_id, password: TEXT): T  RAISES {Error};
(* Establish a connection to the named "database" using "user_id" and
  "password" as authentication credentials. *)

TYPE
  (* a database connection *)
  T <: Public;  Public = OBJECT METHODS
    disconnect ()              RAISES {Error};
    new_stmt (): Stmt          RAISES {Error};
    auto_commit (on: BOOLEAN)  RAISES {Error};
    commit ()                  RAISES {Error};
    abort ()                   RAISES {Error};
  END;

(*
  Given a database connection "db",

  "db.disconnect()" closes the connection to "db".  Further attempts
  to use the connection are checked runtime errors.  If a connection
  is garbage collected, the runtine will attempt to disconnect it.
  But, users should not rely on timely garbage collection to close
  their connections.

  "db.new_stmt()" returns a new statement that can be used to query or
  update the database "db".

  The "current transaction" is any sequence of one or more statements
  that has been executed, but not commited or aborted.

  "db.auto_commit(on)" enables (disables) automatic commits on "db"
  after each statement is executed if "on" is "TRUE" ("FALSE").
  Initially connections have auto commiting enabled.  When auto
  commiting is disabled, "db.commit()" or "db.abort()" must be
  explicitly called to finish the current transaction.

  "db.commit()" commits the current transaction on "db".

  "db.abort()" aborts the current transaction on "db".
*)

(*-------------------------------------------------------- statements ---*)
(*
  A "db.Stmt", or statement, represents a database query or update.
  Each statement is bound to and executes against one connection.  A
  single connection may have multiple outstanding statements.
  Operations on a statement are serialized.
*)

TYPE
  (* a SQL database statement (query or update) *)
  Stmt <: StmtPublic;  StmtPublic = MUTEX OBJECT METHODS
    prepare (operation: TEXT)       RAISES {Error};
    execute (operation: TEXT)       RAISES {Error};
    fetch ():  Results              RAISES {Error};
    done ()                         RAISES {Error};
    close ()                        RAISES {Error};
    get_cursor_name (): TEXT        RAISES {Error};
    set_cursor_name (nm: TEXT)      RAISES {Error};
    num_rows (): INTEGER            RAISES {Error};
    describe_result (): ResultDesc  RAISES {Error};
    connection (): T;
  END;

  Results = REF ARRAY OF REFANY;

  ResultDesc = REF ARRAY OF ColumnDesc;
  ColumnDesc = RECORD
    name      : TEXT;
    type      : DataType;
    precision : INTEGER;  (* total number of digits *)
    scale     : INTEGER;  (* number of digits right of the decimal place *)
    nullable  : Nullable;
  END;

  Nullable = { Yes, No, Unknown };

(*
  ANSI SQL and its extensions define a set of primitive data types.
  The values stored in a column of the database are of a single type.
  For each SQL type, this interface defines a corresponding Modula-3
  reference type.   The vector of results returned by "st.fetch()"
  will contain values corresponding to these Modula-3 types.
*)

TYPE
  (* ANSI and extended SQL datatypes  *)
  DataType = {     (* Modula-3 type in the result vector *)
    Null,          (* NULL *)
    Char,          (* RefString *)
    VarChar,       (* RefString *)
    LongVarChar,   (* RefString *)
    Numeric,       (* REF LONGREAL -- ?? *)
    Decimal,       (* REF LONGREAL -- ?? *)
    BigInt,        (* RefBigInt    (64-bit signed integer) *)
    Integer,       (* REF INTEGER  (32-bit signed integer) *)
    SmallInt,      (* REF INTEGER  (16-bit signed integer) *)
    TinyInt,       (* REF INTEGER  (8-bit signed integer) *)
    Real,          (* REF REAL *)
    Float,         (* REF LONGREAL *)
    Double,        (* REF LONGREAL *)
    Bit,           (* REF BOOLEAN *)
    LongVarBinary, (* RefString *)
    VarBinary,     (* RefString *)
    Binary,        (* RefString *)
    Date,          (* RefDate *)
    Time,          (* RefTime *)
    Timestamp      (* RefTimestamp *)
  };
  RefString = REF UNTRACED REF ARRAY OF CHAR;
  RefBigInt = REF RECORD lo, hi: INTEGER END; (* == 2^32*hi + lo *)
  RefDate   = REF RECORD year, month, day: INTEGER END;
  RefTime   = REF RECORD hour, minute, second: INTEGER END;
  RefTimestamp = REF RECORD
                  year, month, day: INTEGER;
                  hour, minute, second, fraction: INTEGER;
                 END;

(*
  Given a statement "st",

  "st.prepare(s)" will prepare the SQL statement "s" for execution.

  "st.execute(NIL)" will excute or reexecute the statment previously
  prepared by "st.prepare(s)".  It is a checked runtime error if no
  statement is prepared.

  "st.execute(s)" will execute the SQL statement "s" once.  Any prepared
  statements are lost.

  "st.fetch()" returns a non-NIL array of references to the values
  corresponding to the next row of the result of the last statement
  executed by "st".  If there are no more results, NIL is returned.
  Note, the array of references and the reference values themselves
  are reused by subsequent calls to "fetch".  It is the programmer's
  responsibility to copy values out of the result vector if they're
  needed across multiple calls to "fetch".  The result elements
  corresponding to columns containing "nullified" values will be
  "NIL".  It is a checked runtime error to call "fetch" before the
  statement has been executed.

  "st.done()" finishes a single statement execution and result fetching
  cycle.  Then, "st" can be reused for another cycle.

  "st.close()" releases the resources used by "st".  It is a checked
  runtime error to reuse a closed statement.  If a statement is garbage
  collected, the runtine will attempt to disconnect it.  Users should
  not rely on timely garbage collection to close their statements.

  "st.get_cursor_name()" returns the name attached to "st"s cursor.
  If no name is attached, "NIL" is returned.  Attached cursor names
  can be referenced by other statements to coordinate multi-statement
  transactions.

  "st.set_cursor_name(nm)" attaches the name "nm" to "st"s cursor.

  "st.connection()" returns the underlying database connection used by "st".
*)

(*--------------------------------------- misc. DB server information ---*)

PROCEDURE GetDataSources (): DescList  RAISES {Error};
(* Returns the names and descriptions of the data sources that are
   available from the local server. *)

PROCEDURE GetDrivers (): DescList  RAISES {Error};
(* Returns the names and descriptions of the drivers that are
   available from the local server. *)

TYPE DescList = REF RECORD  name, description: TEXT;  next: DescList;  END;


(*--------------------------------------------- errors and exceptions ---*)

EXCEPTION Error (ErrorDesc);
(* 
  Routines in this interface raise "DB.Error" whenever an operation
  fails.  The failure may be due to a failure in the underlying
  database or network, or the failure may be due to programming
  errors.  Blatant programming errors detected by this implementation
  will trigger checked runtime errors.  That is, your program will
  crash.  
*)

TYPE
  ErrorDesc = REF RECORD
    state       : ARRAY [0..5] OF CHAR;  (* e.g. "M1001" with zero termination *)
    description : TEXT;
    native_err  : INTEGER; (* lower level driver or DBMS error code *)
  END;

END DB.

