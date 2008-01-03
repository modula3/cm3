
(*
   DBRep provides access to the underlying ODBC handles.
   Using these handles concurrently via the DB interface
   may cause unchecked runtime errors (ie. we can't say
   what the ODBC drivers might be doing!).  Similarly,
   if the Modula-3 REFs are garbage collected, the ODBC
   handles may be closed and released, unexpectedly.
*)

UNSAFE INTERFACE ODBCDBRep;

IMPORT DB, SQLtypes;

PROCEDURE GetHENV (): SQLtypes.SQLHENV;
(* Returns the currently open environment handle.  If no environment
   is open, returns SQL.SQL_NULL_HENV. *)

PROCEDURE GetHDBC (t: DB.T): SQLtypes.SQLHDBC;
(* Returns the ODBC connection handle associated with 't'. *)

PROCEDURE GetHSTMT (st: DB.Stmt): SQLtypes.SQLHSTMT;
(* Returns the ODBC statement handle associated with 'st'. *)

END ODBCDBRep.
