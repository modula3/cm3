MODULE ODBCDBStub EXPORTS ODBCDB;

(*
	This is a "stub" that gets compiled in instead of the real db interface if
	the requisit system library is not declared in the cm3.cfg file. See the
	m3makefile for the library names.
*)

IMPORT DB;

PROCEDURE GetInterface(): DB.Interface =
BEGIN
	RETURN NIL;
END GetInterface;

BEGIN
END ODBCDBStub.
