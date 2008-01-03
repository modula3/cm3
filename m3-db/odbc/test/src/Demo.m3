UNSAFE MODULE Demo EXPORTS Main;

IMPORT IO, Fmt, Process, SQL, Text;
FROM SQLtypes IMPORT SQLRETURN, SQLHENV, SQLSMALLINT;

PROCEDURE Check (err: SQLRETURN) =
  BEGIN
    IO.Put ("=> ");
    CASE err OF
    | SQL.SQL_INVALID_HANDLE    =>  IO.Put ("invalid handle\n");
    | SQL.SQL_ERROR             =>  IO.Put ("error\n");
    | SQL.SQL_SUCCESS_WITH_INFO =>  IO.Put ("ok (with info)\n"); RETURN;
    | SQL.SQL_SUCCESS           =>  IO.Put ("ok\n");             RETURN;
    | SQL.SQL_NO_DATA_FOUND     =>  IO.Put ("no data found\n");  RETURN;
    ELSE  IO.Put ("unrecognized return code: " & Fmt.Int (err) & "\n");
    END;
    Process.Exit (1);
  END Check;

PROCEDURE DoIt () =
  VAR
    henv:  SQLHENV;
  BEGIN
    IO.Put ("allocating environment...\n");
    Check (SQL.SQLAllocEnv (ADR (henv)));

    EnumerateDatabases (henv);
    
    IO.Put ("freeing environment...\n");
    Check (SQL.SQLFreeEnv (henv));

    IO.Put ("done.\n");
  END DoIt;

PROCEDURE EnumerateDatabases (henv: SQLHENV) =
  VAR
    direction := SQL.SQL_FETCH_FIRST;
    err       : SQLRETURN;
    srclen    : SQLSMALLINT;
    desclen   : SQLSMALLINT;
    source    : ARRAY [0..255] OF CHAR;
    desc      : ARRAY [0..255] OF CHAR;
  BEGIN
    IO.Put ("available databases:\n");
    LOOP
      err := SQL.SQLDataSources (henv, direction,
                                 ADR (source[0]), BYTESIZE (source), ADR(srclen),
                                 ADR (desc[0]), BYTESIZE (desc), ADR(desclen));
      Check (err);
      IF (err = SQL.SQL_NO_DATA_FOUND) THEN EXIT; END;
      IO.Put (" source: " & Text.FromChars (SUBARRAY (source, 0, srclen)) & "\n");
      IO.Put (" desc:   " & Text.FromChars (SUBARRAY (desc, 0, desclen)) & "\n");
      direction := SQL.SQL_FETCH_NEXT;
    END; (*loop*)
    IO.Put ("\n");
  END EnumerateDatabases;

BEGIN
  DoIt ();
END Demo.
