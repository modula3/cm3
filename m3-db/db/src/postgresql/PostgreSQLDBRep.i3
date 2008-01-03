
(*
   DBRep provides access to the underlying database handles.
   Using these handles concurrently via the DB interface
   may cause unchecked runtime errors.
*)

UNSAFE INTERFACE PostgreSQLDBRep;
IMPORT DB;

PROCEDURE GetHENV (): NULL;
PROCEDURE GetHDBC (t: DB.T): ADDRESS;
PROCEDURE GetHSTMT (st: DB.Stmt): TEXT;

END PostgreSQLDBRep.
