
MODULE Demo3 EXPORTS Main;

IMPORT DB, IO, Text, Fmt;

PROCEDURE DumpError (err: DB.ErrorDesc) =
  BEGIN
    IO.Put ("\n*** DB.Error ***\n");
    IO.Put ("  state:  ");
      IO.Put (Text.FromChars (SUBARRAY (err.state, 0, 5)));
      IO.Put ("\n");
    IO.Put ("  native: ");
      IO.PutInt (err.native_err);
      IO.Put ("\n");
    IO.Put ("  desc:   ");
      IF (err.description # NIL) THEN IO.Put (err.description); END;
      IO.Put ("\n");
    IO.Put ("*****************\n");
  END DumpError;

PROCEDURE Trim (txt: TEXT): TEXT =
  VAR start := 0;  len := Text.Length (txt);  ch: CHAR;
  BEGIN
    WHILE (len > 0) DO
      ch := Text.GetChar (txt, start);
      IF (ch # ' ') AND (ch # '\t') AND (ch # '\r') AND (ch # '\n') THEN EXIT; END;
      INC (start);
      DEC (len);
    END;
    WHILE (len > 0) DO
      ch := Text.GetChar (txt, start + len - 1);
      IF (ch # ' ') AND (ch # '\t') AND (ch # '\r') AND (ch # '\n') THEN EXIT; END;
      DEC (len);
    END;
    RETURN Text.Sub (txt, start, len);
  END Trim;

PROCEDURE Prompt (tag: TEXT;  default: TEXT := NIL): TEXT =
  <*FATAL IO.Error*>
  VAR txt: TEXT;
  BEGIN
    IO.Put (tag);
    IF (default # NIL) THEN
      IO.Put (" [");
      IO.Put (default);
      IO.Put ("]");
    END;
    IO.Put (": ");
    txt := Trim (IO.GetLine ());
    IF (Text.Length (txt) > 0) THEN
      RETURN txt;
    ELSIF (default # NIL) THEN
      RETURN default;
    ELSE
      RETURN "";
    END;
  END Prompt;


PROCEDURE DoIt () RAISES {DB.Error} =
  VAR
    name, user, pass: TEXT;
    db: DB.T;
    stmt: DB.Stmt := NIL;
    cmd: TEXT;
  BEGIN
    name := Prompt ("database", "test");
    user := Prompt ("user", "admin");
    pass := Prompt ("password", "foobar");
    db := DB.Connect (name, user, pass);

    (****
    stmt.set_cursor_name (Prompt ("cursor name"));
    IO.Put ("cursor name: ");
    IO.Put (stmt.get_cursor_name ());
    IO.Put ("\n");
    ****)

    LOOP
      IF (stmt = NIL) THEN stmt := db.new_stmt (); END;

      cmd := Prompt ("cmd");
      IF Text.Length (cmd) <= 0
        OR Text.Equal (cmd, "?")
        OR Text.Equal (cmd, "help") THEN
        IO.Put ("enter:\n");
        IO.Put ("  stmt   -- to execute the SQL statement \"stmt\"\n");
        IO.Put ("  !stmt  -- to prepare \"stmt\"\n");
        IO.Put ("  *      -- to execute or reexecute the prepared statement\n");
        IO.Put ("  **     -- to execute and finish the prepared statement\n");
        IO.Put ("  help   -- to see this help\n");
        IO.Put ("  quit   -- to exit the program\n");
      ELSIF Text.Equal (cmd, "quit") THEN
        EXIT;
      ELSIF Text.GetChar (cmd, 0) = '!' THEN
        TRY
          stmt.prepare (Text.Sub (cmd, 1));
          IO.Put ("prepared.");
        EXCEPT DB.Error (err) => DumpError (err);
        END;
      ELSIF Text.Equal (cmd, "*") THEN
        DoExec (stmt, NIL, FALSE);
      ELSIF Text.Equal (cmd, "**") THEN
        DoExec (stmt, NIL, TRUE);
        stmt.close ();
        stmt := NIL;
      ELSE
        DoExec (stmt, cmd, TRUE);
        stmt.close ();
        stmt := NIL;
      END;
      IO.Put ("\n");
    END;

    IF (stmt # NIL) THEN stmt.close (); END;
    db.disconnect ();
  END DoIt;

PROCEDURE DoExec (stmt: DB.Stmt;  cmd: TEXT;  finish: BOOLEAN) =
  VAR res: DB.Results;
  BEGIN
    TRY
      stmt.execute (cmd);
      IO.Put ("# rows updated = "); IO.PutInt (stmt.num_rows()); IO.Put("\n");
      IF PrintDesc (stmt.describe_result()) THEN
        IO.Put ("-- results ---\n");
        LOOP
          res := stmt.fetch ();
          IF res = NIL THEN EXIT; END;
          PrintResults (res);
        END;
        IO.Put ("\n");
      END;
      IF finish THEN stmt.done (); END;
    EXCEPT DB.Error (err) => DumpError (err);
    END;
  END DoExec;

PROCEDURE PrintDesc (desc: DB.ResultDesc): BOOLEAN =
  BEGIN
    IF (NUMBER (desc^) <= 0) THEN RETURN FALSE; END;
    IO.Put ("-- result description --\n");
    FOR i := FIRST (desc^) TO LAST (desc^) DO
      WITH z = desc[i] DO
        IO.Put ("col #");  IO.PutInt (i+1);
        IO.Put ("  \""); IO.Put (z.name);  IO.Put ("\": ");
        IO.Put (DataTypeName [z.type]);
        IO.Put (" (prec=");  IO.PutInt (z.precision);
        IO.Put (", scale=");  IO.PutInt (z.scale);
        IO.Put (", nulls=");  IO.Put (NullableName [z.nullable]); IO.Put(")");
        IO.Put ("\n");
      END;
    END;
    IO.Put ("\n");
    RETURN TRUE;
  END PrintDesc;

CONST
  DataTypeName = ARRAY DB.DataType OF TEXT {
    "Null",          (* NULL *)
    "Char",          (* RefString *)
    "VarChar",       (* RefString *)
    "LongVarChar",   (* RefString *)
    "Numeric",       (* REF ARRAY OF CHAR (p,s) -- ?? *)
    "Decimal",       (* REF ARRAY OF CHAR (p,s) -- ?? *)
    "BigInt",        (* RefBigInt    (64-bit signed integer) *)
    "Integer",       (* REF INTEGER  (32-bit signed integer) *)
    "SmallInt",      (* REF INTEGER  (16-bit signed integer) *)
    "TinyInt",       (* REF INTEGER  (8-bit signed integer) *)
    "Real",          (* REF REAL *)
    "Float",         (* REF LONGREAL *)
    "Double",        (* REF LONGREAL *)
    "Bit",           (* REF BOOLEAN *)
    "LongVarBinary", (* RefString *)
    "VarBinary",     (* RefString *)
    "Binary",        (* RefString *)
    "Date",          (* RefDate *)
    "Time",          (* RefTime *)
    "TimeStamp"      (* RefTimestamp *)
  };

CONST
  NullableName = ARRAY DB.Nullable OF TEXT { "Yes", "No", "Unknown" };

PROCEDURE PrintResults (res: DB.Results) =
  BEGIN
    FOR i := FIRST (res^) TO LAST (res^) DO
      IO.Put ("col #");  IO.PutInt (i+1);  IO.Put ("  ");
      TYPECASE res[i] OF
      | NULL =>
          IO.Put ("NIL");
      | DB.RefString (str) =>
          IO.Put ("STRING (");
          IO.PutInt (NUMBER (str^^));
          IO.Put (") = \"");
          IO.Put (Text.FromChars (str^^));
          IO.Put ("\"");
      | REF LONGREAL (lr) =>
          IO.Put ("LONGREAL ");  IO.Put (Fmt.LongReal (lr^));
      | REF INTEGER (ri) =>
          IO.Put ("INTEGER ");  IO.PutInt (ri^);
      | DB.RefBigInt (bi) =>
          IO.Put ("BIGINT ");  IO.PutInt (bi.hi);  IO.PutInt (bi.lo);
      | REF REAL (rr) =>
          IO.Put ("REAL ");  IO.PutReal (rr^);
      | REF BOOLEAN (rb) =>
          IO.Put ("BIT ");  IF rb^ THEN IO.Put ("ON"); ELSE IO.Put ("OFF"); END;
      | DB.RefDate (dat) =>
          IO.Put ("Date ");
          IO.Put ("  y/m/d: ");  IO.PutInt (dat.year);
          IO.Put ("/");  IO.PutInt (dat.month);
          IO.Put ("/");  IO.PutInt (dat.day);
      | DB.RefTime (tim) =>
          IO.Put ("Time ");
          IO.Put ("  h:m:s  ");  IO.PutInt (tim.hour);
          IO.Put (":");  IO.PutInt (tim.minute);
          IO.Put (":");  IO.PutInt (tim.second);
      | DB.RefTimestamp (ts) =>
          IO.Put ("Timestamp ");
          IO.Put ("  y/m/d: ");  IO.PutInt (ts.year);
          IO.Put ("/");  IO.PutInt (ts.month);
          IO.Put ("/");  IO.PutInt (ts.day);
          IO.Put ("  h:m:s.f ");  IO.PutInt (ts.hour);
          IO.Put (":");  IO.PutInt (ts.minute);
          IO.Put (":");  IO.PutInt (ts.second);
          IO.Put (".");  IO.PutInt (ts.fraction);
      ELSE
        IO.Put ("!! Unexpected return value (TYPECODE = ");
        IO.PutInt (TYPECODE (res[i]));
        IO.Put (") !!\n");
      END;
      IO.Put ("\n");
    END;
    IF (NUMBER (res^) > 1) THEN IO.Put ("\n"); END;
  END PrintResults;

BEGIN
  TRY DoIt ();
  EXCEPT DB.Error (err) => DumpError (err);
  END;
END Demo3.
