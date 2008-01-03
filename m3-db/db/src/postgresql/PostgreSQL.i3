(*
	Based on Postgres95 interfaces in CM3 distribution and
	libpq-fe.h (and some related headers) from PostgreSQL 7.2
	Darko Volaric September 2002 darko@peter.com.au

 Copyright (c) 1994, Regents of the University of California
 Copyright (c) 1996-2001, PostgreSQL Global Development Group
 Copyright (c) 2002 Darko Volaric
 
*)


UNSAFE INTERFACE PostgreSQL;

FROM Ctypes IMPORT char_star, int, char, short, int_star;
IMPORT Word;


CONST 
	MAX_MESSAGE_LEN = 8193;
	BYTELEN = 8;
	MAX_FIELDS = 512;
	NAMEDATALEN = 32;
	CMDSTATUS_LEN = 40;

  Bool        : Word.T = 16;
  Bytea       : Word.T = 17;
  Char        : Word.T =  18;
  Char16      : Word.T =  19;
  Char2       : Word.T = 409;
  Char4       : Word.T = 410; 
  Char8       : Word.T = 411;

(* 

  DT??        : Word.T = 16_014;
  Int2        : Word.T = 16_015;
  Int28       : Word.T = 16_016;
  Int4        : Word.T = 16_017;

*)

  SmallInt    : Word.T = 16_015;
  Int         : Word.T = 16_017;
  Text        : Word.T = 16_019;
  VarChar     : Word.T = 16_413;
(*  Char        : Word.T = 16_412; Conflict? *)
  Real        : Word.T = 16_2bc;
  Float       : Word.T = 16_2bc;
  Date        : Word.T = 16_43a;
  Time        : Word.T = 16_43b;
  Timestamp   : Word.T = 702;
  Bpchar      : Word.T = 1042;



TYPE
  TUPLE = ADDRESS;
  FILE = ADDRESS;
  Dllist = ADDRESS;
  Port = ADDRESS;
  int2 = short;
  Oid = Word.T;

(*  POSTGRES backend dependent Constants. *)
(* ERROR_MSG_LENGTH should really be the same as ELOG_MAXLEN in utils/elog.h*)

CONST ERROR_MSG_LENGTH = 4096;
CONST COMMAND_LENGTH = 20;
CONST REMARK_LENGTH = 80;
CONST PORTAL_NAME_LENGTH = 16;


(*** Enumerations ***)

TYPE
	CONNECTION = {ZERO, OK, BAD, STARTED, MADE, AWAITING_RESPONSE, AUTH_OK, SETENV};
	ConnStatusType = CONNECTION;
	
	PGRES_POLLING = {FAILED, READING, WRITING, OK, ACTIVE};
	PostgresPollingStatusType = PGRES_POLLING;
	
  PGRS = {EMPTY_QUERY, COMMAND_OK, TUPLES_OK, COPY_OUT, COPY_IN, BAD_RESPONSE, NONFATAL_ERROR,
  	FATAL_ERROR};
  ExecStatusType = PGRS;


(*** Data Structures ***)

TYPE
  PGconn = ADDRESS;
  PGresult = ADDRESS;

  PGnotify = RECORD
    relname: ARRAY [0..NAMEDATALEN-1] OF char;
    be_pid: int;
  END;
  PGnotify_star = UNTRACED REF PGnotify;

(*
PQnoticeProcessor 
pqbool
PQprintOpt
PQconninfoOption
*)

  PQArgBlock = RECORD
    len: int;
    isint: int;  (* Q: Should this be BOOLEAN? *)
    u: int;  
  END;
  PQArgBlock_star = UNTRACED REF PQArgBlock;


(*** Libaray Functions ***)

	(* 
	<*EXTERNAL*> PROCEDURE PQconnectStart
	<*EXTERNAL*> PROCEDURE PQconnectPoll
	<*EXTERNAL*> PROCEDURE PQconnectdb
	*)
<*EXTERNAL*> PROCEDURE PQsetdbLogin(
	pghost: char_star; 
	pgport: char_star; 
	pgoptions: char_star; 
	pgtty: char_star; 
	dbName: char_star; 
	login: char_star; 
	pwd: char_star
): PGconn;
<*EXTERNAL*> PROCEDURE PQfinish (conn: PGconn);
	(*
	<*EXTERNAL*> PROCEDURE PQconndefaults
	<*EXTERNAL*> PROCEDURE PQconninfoFree
	<*EXTERNAL*> PROCEDURE PQresetStart
	<*EXTERNAL*> PROCEDURE PQresetPoll
	*)
<*EXTERNAL*> PROCEDURE PQreset (conn: PGconn); (* close the current connection and restablish a new one with the same parameters *)
	(*
	<*EXTERNAL*> PROCEDURE PQrequestCancel
	*)
<*EXTERNAL*> PROCEDURE PQdb(conn: PGconn): char_star;
<*EXTERNAL*> PROCEDURE PQuser(conn: PGconn): char_star;
<*EXTERNAL*> PROCEDURE PQpass(conn: PGconn): char_star;
<*EXTERNAL*> PROCEDURE PQhost(conn: PGconn): char_star;
<*EXTERNAL*> PROCEDURE PQport(conn: PGconn): char_star;
<*EXTERNAL*> PROCEDURE PQtty(conn: PGconn): char_star;
<*EXTERNAL*> PROCEDURE PQoptions(conn: PGconn): char_star;
<*EXTERNAL*> PROCEDURE PQstatus (conn: PGconn): ConnStatusType;
<*EXTERNAL*> PROCEDURE PQerrorMessage(conn: PGconn): char_star;
	(*
	<*EXTERNAL*> PROCEDURE PQsocket
	<*EXTERNAL*> PROCEDURE PQbackendPID
	<*EXTERNAL*> PROCEDURE PQclientEncoding
	<*EXTERNAL*> PROCEDURE PQsetClientEncoding
	*)
<*EXTERNAL*> PROCEDURE PQtrace (conn: PGconn; debug_port: FILE);
<*EXTERNAL*> PROCEDURE PQuntrace (conn: PGconn);
	(*
	<*EXTERNAL*> PROCEDURE PQsetNoticeProcessor
	<*EXTERNAL*> PROCEDURE PQescapeString
	<*EXTERNAL*> PROCEDURE PQescapeBytea
	*)
<*EXTERNAL*> PROCEDURE PQexec (conn: PGconn;  query: char_star): PGresult;
<*EXTERNAL*> PROCEDURE PQnotifies(conn: PGconn): PGnotify_star;
	(*
	<*EXTERNAL*> PROCEDURE PQfreeNotify
	<*EXTERNAL*> PROCEDURE PQsendQuery
	<*EXTERNAL*> PROCEDURE PQgetResult
	<*EXTERNAL*> PROCEDURE PQisBusy
	<*EXTERNAL*> PROCEDURE PQconsumeInput
	*)
<*EXTERNAL*> PROCEDURE PQgetline (conn: PGconn; string: char_star; length: int): int;
<*EXTERNAL*> PROCEDURE PQputline (conn: PGconn;  string: char_star);
	(*
	<*EXTERNAL*> PROCEDURE PQgetlineAsync
	<*EXTERNAL*> PROCEDURE PQputnbytes
	*)
<*EXTERNAL*> PROCEDURE PQendcopy (conn: PGconn): int;
	(*
	<*EXTERNAL*> PROCEDURE PQsetnonblocking
	<*EXTERNAL*> PROCEDURE PQisnonblocking
	<*EXTERNAL*> PROCEDURE PQflush
	*)
<*EXTERNAL*> PROCEDURE PQfn(conn: PGconn; fnid: int;  result_buf: int_star; result_len: int_star;
	result_is_int: int; args: PQArgBlock_star; nargs: int): PGresult;
<*EXTERNAL*> PROCEDURE PQresultStatus (res: PGresult): ExecStatusType;
	(*
	<*EXTERNAL*> PROCEDURE PQresStatus
	<*EXTERNAL*> PROCEDURE PQresultErrorMessage
	*)
<*EXTERNAL*> PROCEDURE PQntuples (res: PGresult): int;
<*EXTERNAL*> PROCEDURE PQnfields (res: PGresult): int;
	(*
	<*EXTERNAL*> PROCEDURE PQbinaryTuples
	*)
<*EXTERNAL*> PROCEDURE PQfname (res: PGresult; field_num: int): char_star;
<*EXTERNAL*> PROCEDURE PQfnumber (res: PGresult;  field_name: char_star): int;
<*EXTERNAL*> PROCEDURE PQftype (res: PGresult; field_num: int): Oid;
<*EXTERNAL*> PROCEDURE PQfsize (res: PGresult; field_num: int): int2;
	(*
	<*EXTERNAL*> PROCEDURE PQfmod
	*)
<*EXTERNAL*> PROCEDURE PQcmdStatus (res: PGresult): char_star;
<*EXTERNAL*> PROCEDURE PQoidStatus (res: PGresult): char_star;
	(*
	<*EXTERNAL*> PROCEDURE PQoidValue
	<*EXTERNAL*> PROCEDURE PQcmdTuples
	*)
<*EXTERNAL*> PROCEDURE PQgetvalue (res: PGresult; tup_num: int; field_num: int): char_star;
<*EXTERNAL*> PROCEDURE PQgetlength(res: PGresult; tup_num: int; field_num: int): int;
<*EXTERNAL*> PROCEDURE PQgetisnull(res: PGresult; tup_num: int; field_num: int): int;

<*EXTERNAL*> PROCEDURE PQclear(res: PGresult);
	(*
	<*EXTERNAL*> PROCEDURE PQmakeEmptyPGresult
	<*EXTERNAL*> PROCEDURE PQprint
	<*EXTERNAL*> PROCEDURE PQdisplayTuples
	*)
<*EXTERNAL*> PROCEDURE PQprintTuples(res: PGresult; fout: FILE; printAttName: int; 
	terseOutput: int;  width: int);
<*EXTERNAL*> PROCEDURE lo_open(conn: PGconn;  lobjId: Oid;  mode: int): PGconn;
<*EXTERNAL*> PROCEDURE lo_close(conn: PGconn; fd: int): int;
<*EXTERNAL*> PROCEDURE lo_read(conn: PGconn; fd: int;  buf: char_star; len: int): int;
<*EXTERNAL*> PROCEDURE lo_write(conn: PGconn;  fd: int; buf: char_star; len: int): int;
<*EXTERNAL*> PROCEDURE lo_lseek(conn: PGconn;  fd: int;  offset: int;  whence: int): int;
<*EXTERNAL*> PROCEDURE lo_creat(conn: PGconn;  mode: int): Oid;
<*EXTERNAL*> PROCEDURE lo_tell(conn: PGconn; fd: int): int;
<*EXTERNAL*> PROCEDURE lo_unlink(conn: PGconn; lobjId: Oid): int;
<*EXTERNAL*> PROCEDURE lo_import(conn: PGconn; filename: char_star): Oid;
<*EXTERNAL*> PROCEDURE lo_export(conn: PGconn; lobjId: Oid;  filename: char_star): int;
	(*
	<*EXTERNAL*> PROCEDURE PQmblen
	<*EXTERNAL*> PROCEDURE PQenv2encoding
	*)

END PostgreSQL.
