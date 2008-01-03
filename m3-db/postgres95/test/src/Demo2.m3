
UNSAFE MODULE Demo2 EXPORTS Main;
FROM PQ IMPORT PGconn_star, PGresult_star, PQfinish, PQexec, PQsetdb, PQstatus,
               PQerrorMessage, PQresultStatus, PQclear, PQnfields, PQfname,
               PQgetvalue, PQntuples, PQftype,
               CONNECTION, PGRS;
IMPORT Process, IO, Fmt, Params;
FROM M3toC IMPORT TtoS, StoT, CopyStoT;
FROM Ctypes IMPORT char_star, int;
IMPORT Word;


PROCEDURE exit_nicely (c: PGconn_star) =
  BEGIN
    PQfinish (c);
    Process.Exit (0);
  END exit_nicely;
  
VAR
  Null := LOOPHOLE(0,ADDRESS);

VAR
  pghost, pgport, pgoptions, pgtty: char_star := Null;
  m_dbname := "test";
  query := "select * from pg_database";
  dbName: char_star;
  nFields: int;
  conn: PGconn_star;
  res: PGresult_star;
  
BEGIN

  IF Params.Count # 3 THEN
    IO.Put ("syntax: Demo2 <dbname> <query>\n");
    Process.Exit(1);
  END;
  
  m_dbname := Params.Get(1);
  query := Params.Get(2);

  dbName := TtoS (m_dbname);

  (* begin, by setting the parameters for a backend connection
     if the parameters are Null, then the system will try to use
     reasonable defaults by looking up environment variables 
     or, failing that, using hardwired constants *)

  IO.Put(" make a connection to the database \n");
  conn := PQsetdb(pghost, pgport, pgoptions, pgtty, dbName);
  
  (* check to see that the backend connection was successfully made *)
  IF (PQstatus(conn) = CONNECTION.BAD) THEN
    IO.Put ("Connection to database " & m_dbname & "failed.\n");
    IO.Put (CopyStoT(PQerrorMessage(conn)));
    exit_nicely(conn);
  END;

  IO.Put(" start a transaction block \n");
  res := PQexec(conn, TtoS("BEGIN")); 
  IF (PQresultStatus(res) # PGRS.COMMAND_OK) THEN
    IO.Put("BEGIN command failed\n");
    PQclear(res);
    exit_nicely(conn);
  END;
  (* should PQclear PGresult whenever it is no longer needed to avoid
     memory leaks *)
  PQclear(res); 

  IO.Put(" fetch instances for query " & query & "\n");
  res := PQexec(conn,TtoS("DECLARE myportal CURSOR FOR " & query));
  
  IO.Put("result: 0x");
  IO.Put(Fmt.Unsigned(LOOPHOLE(res,Word.T)));
  IO.Put ("\n");

  IF LOOPHOLE(res,Word.T) = 0 THEN 
    exit_nicely(conn);
  END;

  IO.Put ("....");
  IO.PutInt (ORD(PQresultStatus(res)));
  IO.Put ("=?");
  IO.PutInt (ORD(PGRS.COMMAND_OK));
  IO.Put ("\n");

  IF (PQresultStatus(res) # PGRS.COMMAND_OK) THEN
    IO.Put ("DECLARE CURSOR command failed\n");
    PQclear(res);
    exit_nicely(conn);
  END;
  PQclear(res);

  res := PQexec(conn,TtoS("FETCH ALL in myportal"));
  IF (PQresultStatus(res) # PGRS.TUPLES_OK) THEN
    IO.Put ("FETCH ALL command didn't return tuples properly\n");
    PQclear(res);
    exit_nicely(conn);
  END;
 
  (* first, print out the attribute names *)
  IO.Put("Names:  ");
  nFields := PQnfields(res);
  FOR i := 0 TO nFields - 1 DO 
    IO.Put (Fmt.F("%10s", StoT(PQfname(res,i))));
  END;
  IO.Put("\n");

  (* second, print out the Oids for each name *)
  IO.Put("Oids:   ");
  FOR i := 0 TO nFields - 1 DO 
    IO.Put (Fmt.F("%10s", 
                  Fmt.Int(LOOPHOLE(PQftype(res,i), Word.T))));
  END;
  IO.Put("\n");

  IO.Put("        ");
  FOR i := 0 TO nFields-1 DO
    IO.Put (Fmt.F("%10s", "------------"));
  END;
  IO.Put("\n\n");

  (* next, print out the instances *)
  FOR i := 0 TO PQntuples(res)-1 DO 
    IO.Put("        ");
    FOR j := 0 TO nFields-1 DO 
      IO.Put(Fmt.F("%10s", CopyStoT(PQgetvalue(res,i,j))));
    END;
    IO.Put("\n");
  END;

  PQclear(res);
  
  IO.Put(" close the portal \n");
  res := PQexec(conn, TtoS("CLOSE myportal"));
  PQclear(res);

  IO.Put(" end the transaction \n");
  res := PQexec(conn, TtoS("END"));
  PQclear(res);

  IO.Put(" close the connection to the database and cleanup \n");
  PQfinish(conn);

END Demo2.
  

