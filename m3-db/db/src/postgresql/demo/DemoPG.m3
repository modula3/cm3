
UNSAFE MODULE DemoPG EXPORTS Main;

FROM PostgreSQL IMPORT 
	PGconn, PGresult, PQfinish, PQexec, PQsetdbLogin, PQstatus,
  PQerrorMessage, PQresultStatus, PQclear, PQnfields, PQfname,
  PQgetvalue, PQntuples, PQftype, CONNECTION, PGRS;

FROM M3toC IMPORT 
	CopyTtoS, StoT, CopyStoT;

FROM Ctypes IMPORT 
	char_star, int;

IMPORT 
	IO, Fmt, Word;

VAR
  Null := LOOPHOLE(0,ADDRESS);
  pghost, dbName, login, pwd : char_star := Null;


PROCEDURE Demo(query: TEXT) =
VAR
  pgport, pgoptions, pgtty: char_star := Null;
  nFields: int;
  conn: PGconn;
  res: PGresult;
BEGIN
  
	pgport := CopyTtoS("5432");

  IO.Put(" make a connection to the database \n");
  conn := PQsetdbLogin(pghost, pgport, pgoptions, pgtty, dbName, login, pwd);
  
  (* check to see that the backend connection was successfully made *)
  IF (PQstatus(conn) = CONNECTION.BAD) THEN
    IO.Put ("Connection to database  failed.\n");
    IO.Put (CopyStoT(PQerrorMessage(conn)));
    PQfinish(conn);
  END;

  IO.Put(" start a transaction block \n");
  res := PQexec(conn, CopyTtoS("BEGIN")); 
  IF (PQresultStatus(res) # PGRS.COMMAND_OK) THEN
    IO.Put("BEGIN command failed\n");
    IO.Put (CopyStoT(PQerrorMessage(conn)));
    PQclear(res);
    PQfinish(conn);
    RETURN;
  END;

  (* should PQclear PGresult whenever it is no longer needed to avoid
     memory leaks *)
  PQclear(res); 

  IO.Put(" fetch instances for query " & query & "\n");
  res := PQexec(conn,CopyTtoS("DECLARE myportal CURSOR FOR " & query));
  
  (*
  IO.Put("result: 0x");
  IO.Put(Fmt.Unsigned(LOOPHOLE(res,Word.T)));
  IO.Put ("\n");
	*)
	
  IF LOOPHOLE(res,Word.T) = 0 THEN 
    PQfinish(conn);
    RETURN;
  END;
  
	(*
  IO.Put ("....");
  IO.PutInt (ORD(PQresultStatus(res)));
  IO.Put ("=?");
  IO.PutInt (ORD(PGRS.COMMAND_OK));
  IO.Put ("\n");
  *)

  IF (PQresultStatus(res) # PGRS.COMMAND_OK) THEN
    IO.Put ("DECLARE CURSOR command failed\n");
    PQclear(res);
    PQfinish(conn);
    RETURN;
  END;
  PQclear(res);

  res := PQexec(conn,CopyTtoS("FETCH ALL in myportal"));
  IF (PQresultStatus(res) # PGRS.TUPLES_OK) THEN
    IO.Put ("FETCH ALL command didn't return tuples properly\n");
    PQclear(res);
    PQfinish(conn);
    RETURN;
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
  res := PQexec(conn, CopyTtoS("CLOSE myportal"));
  PQclear(res);

  IO.Put(" end the transaction \n");
  res := PQexec(conn, CopyTtoS("END"));
  PQclear(res);

  IO.Put(" close the connection to the database and cleanup \n");
  PQfinish(conn);
END Demo;

BEGIN
	pghost := CopyTtoS("10.0.1.4");
  dbName := CopyTtoS("test");
  login := CopyTtoS("postgres");
	Demo("select * from test");
END DemoPG.
  

