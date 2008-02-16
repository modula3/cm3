UNSAFE MODULE PQ;

FROM Ctypes IMPORT char_star;

PROCEDURE PQsetdb (pghost: char_star;
                   pgport: char_star; 
                   pgoptions: char_star;
                   pgtty: char_star;
                   dbName: char_star): PGconn_star =
  VAR null := LOOPHOLE(0, char_star);
  BEGIN
    RETURN PQsetdbLogin(pghost, pgport, pgoptions, pgtty, dbName, null, null);
  END PQsetdb;

BEGIN
END PQ.
