
UNSAFE MODULE Demo1 EXPORTS Main;
FROM PQ IMPORT PGconn_star, PGresult_star, PQfinish, PQexec, PQsetdb, PQstatus,
               PQerrorMessage, PQresultStatus, PQclear, PQnfields, PQfname,
               PQgetvalue, PQntuples,
               CONNECTION, PGRS;
IMPORT Process, IO, Fmt;
FROM M3toC IMPORT TtoS, StoT, CopyStoT;
FROM Ctypes IMPORT char_star, int;


PROCEDURE exit_nicely (c: PGconn_star) =
  BEGIN
    PQfinish (c);
    Process.Exit (0);
  END exit_nicely;

  
VAR
  Null := LOOPHOLE(0,ADDRESS);

VAR
  pghost, pgport, pgoptions, pgtty: char_star := Null;
  m_dbname := "template1";
  dbName: char_star := TtoS (m_dbname);
  nFields: int;
  conn: PGconn_star;
  res: PGresult_star;
  
BEGIN

  (* begin, by setting the parameters for a backend connection
     if the parameters are Null, then the system will try to use
     reasonable defaults by looking up environment variables 
     or, failing that, using hardwired constants *)

  (* make a connection to the database *)
  conn := PQsetdb(pghost, pgport, pgoptions, pgtty, dbName);
  
  (* check to see that the backend connection was successfully made *)
  IF (PQstatus(conn) = CONNECTION.BAD) THEN
    IO.Put ("Connection to database " & m_dbname & "failed.\n");
    IO.Put (CopyStoT(PQerrorMessage(conn)));
    exit_nicely(conn);
  END;

  (* start a transaction block *)
  res := PQexec(conn, TtoS("BEGIN")); 
  IF (PQresultStatus(res) # PGRS.COMMAND_OK) THEN
    IO.Put("BEGIN command failed\n");
    PQclear(res);
    exit_nicely(conn);
  END;
  (* should PQclear PGresult whenever it is no longer needed to avoid
     memory leaks *)
  PQclear(res); 

  (* fetch instances from the pg_database, the system catalog of databases*)
  res := PQexec(conn,TtoS("DECLARE myportal CURSOR FOR select * from pg_database"));
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
  nFields := PQnfields(res);
  FOR i := 0 TO nFields - 1 DO 
    IO.Put (Fmt.F("%15s", StoT(PQfname(res,i))));
  END;
  IO.Put("\n\n");

  (* next, print out the instances *)
  FOR i := 0 TO PQntuples(res)-1 DO 
    FOR j := 0 TO nFields-1 DO 
      IO.Put(Fmt.F("%15s", CopyStoT(PQgetvalue(res,i,j))));
    END;
    IO.Put("\n");
  END;

  PQclear(res);
  
  (* close the portal *)
  res := PQexec(conn, TtoS("CLOSE myportal"));
  PQclear(res);

  (* end the transaction *)
  res := PQexec(conn, TtoS("END"));
  PQclear(res);

  (* close the connection to the database and cleanup *)
  PQfinish(conn);

END Demo1.
  

