(*
	Based on MySQL headers mysql.h and mysql_com.h from MySQL 3.23.51
	Darko Volaric September 2002 darko@peter.com.au

	Copyright (C) 2000 MySQL AB & MySQL Finland AB & TCX DataKonsult AB
	Copyright (c) 2002 Darko Volaric
 
*)


UNSAFE INTERFACE MySQL;


FROM Ctypes IMPORT
	char, unsigned_char, char_star, unsigned_int, unsigned_long, int, unsigned_long_star, unsigned_char_star, 
	unsigned_int_star;

IMPORT
	Word;

TYPE

	Field = RECORD
	  name:				char_star;
	  table:			char_star;
	  def:				char_star;
	  type:				enum_field_types;
	  length:			unsigned_int;
	  max_length:	unsigned_int;
	  flags:			unsigned_int;
	  decimals:		unsigned_int;
	END;
	FieldRef = UNTRACED REF Field;
	
	Row = UNTRACED REF ARRAY[0..32767] OF char_star; (* 'char **' (array of strings) *)
	
	FieldOffset = unsigned_int;
	
	my_ulonglong = BITS 64 FOR RECORD hi, lo: Word.T END; (* 64 bit integer *)
	
	my_bool = char;
	
	Rows = RECORD
	  next: RowsRef;
	  data: Row;
	END;
	RowsRef = UNTRACED REF Rows;

	RowOffset = RowsRef;

	Data = RECORD
	  rows:			my_ulonglong ;
	  fields:		unsigned_int ;
	  data:			RowsRef ;
	  alloc:		MemRoot ;
	END;
	DataRef = UNTRACED REF Data;

	MemRoot = RECORD
	  free, used, pre_alloc: ADDRESS; (* 'USED_MEM*' *)
	 	min_malloc, block_size: unsigned_int;
	  error_handler: ADDRESS; (* 'void ( *error_handler )(void)' *)
	END;

	TRec = RECORD
	  net: Net;
	  connector_fd: char_star;
	  host, user, passwd, unix_socket, server_version, host_info, info, db: char_star;
	  port, client_flag, server_capabilities: unsigned_int;
	  protocol_version: unsigned_int;
	  field_count: unsigned_int;
	  server_status: unsigned_int;
	  thread_id: unsigned_long;
	  affected_rows: my_ulonglong;
	  insert_id: my_ulonglong;
	  extra_info: my_ulonglong;
	  packet_length: unsigned_long;
	  status: mysql_status;
	  fields: FieldRef;
	  field_alloc: MemRoot;
	  free_me: my_bool;
	  reconnect: my_bool;
	  options: OptionsRec;
	  scramble_buff: char_star;  (* 'char[9]' *)
	  charset: ADDRESS; (* 'struct charset_info_st *' *)
	  server_language: unsigned_int;
	END;
	T = UNTRACED REF TRec;

	Res = RECORD
	  row_count: my_ulonglong ;
	  field_count, current_field: unsigned_int	;
	  fields: FieldRef ;
	  data: DataRef ;
	  data_cursor: RowsRef ;
	  field_alloc: MemRoot ;
	  row: Row	;
	  current_row: Row	;
	  lengths: unsigned_long_star ;
	  handle: T ;
	  eof: my_bool	;
	END;
	ResRef = UNTRACED REF Res;

	OptionsRec = RECORD
		connect_timeout,client_flag: unsigned_int;
		compress, named_pipe: my_bool ;
		port: unsigned_int;
		host, init_command, user, password, unix_socket, db: char_star;
		my_cnf_file, my_cnf_group, charset_dir, charset_name: char_star;
		use_ssl: my_bool ;
		ssl_key,ssl_cert,ssl_ca,ssl_capath: char_star;
	END;

	Net = RECORD
	  vio: ADDRESS; (* 'Vio*' *)
	  fd: int ; (* 'my_socket' *)
	  fcntl: int ;
	  buff, buff_end, write_pos, read_pos: unsigned_char_star;
	  last_error: char_star; (* 'char[MYSQL_ERRMSG_SIZE]' *)
		last_errno,max_packet,timeout,pkt_nr:  unsigned_int ;
	  error: unsigned_char ;
	  return_errno,compress: my_bool ;
	  no_send_ok: my_bool ;
	  remain_in_buf,length, buf_length, where_b: unsigned_long ;
	  return_status: unsigned_int_star ;
	  reading_or_writing: unsigned_char ;
	  save_char: char ;
	END;



(* Enumerations *)

TYPE
	mysql_option = INTEGER;

CONST
	MYSQL_OPT_CONNECT_TIMEOUT = 1;
	MYSQL_OPT_COMPRESS = 2;
  MYSQL_OPT_NAMED_PIPE = 3;
  MYSQL_INIT_COMMAND = 4;
  MYSQL_READ_DEFAULT_FILE = 5;
  MYSQL_READ_DEFAULT_GROUP = 6;
  MYSQL_SET_CHARSET_DIR = 7;
  MYSQL_SET_CHARSET_NAME = 8;
  MYSQL_OPT_LOCAL_INFILE = 9;

TYPE
	mysql_status = INTEGER;

CONST
	MYSQL_STATUS_READY = 1;
	MYSQL_STATUS_GET_RESULT = 2;
	MYSQL_STATUS_USE_RESULT = 3;


TYPE
	enum_field_types = INTEGER;

CONST
	FIELD_TYPE_DECIMAL = 1; 
	FIELD_TYPE_TINY = 2;
	FIELD_TYPE_SHORT = 3;  
	FIELD_TYPE_LONG = 4;
	FIELD_TYPE_FLOAT = 5;  
	FIELD_TYPE_DOUBLE = 6;
	FIELD_TYPE_NULL = 7;   
	FIELD_TYPE_TIMESTAMP = 8;
	FIELD_TYPE_LONGLONG = 9;
	FIELD_TYPE_INT24 = 10;
	FIELD_TYPE_DATE = 11;   
	FIELD_TYPE_TIME = 12;
	FIELD_TYPE_DATETIME = 13; 
	FIELD_TYPE_YEAR = 14;
	FIELD_TYPE_NEWDATE = 15;
	FIELD_TYPE_ENUM = 247;
	FIELD_TYPE_SET = 248;
	FIELD_TYPE_TINY_BLOB = 249;
	FIELD_TYPE_MEDIUM_BLOB = 250;
	FIELD_TYPE_LONG_BLOB = 251;
	FIELD_TYPE_BLOB = 252;
	FIELD_TYPE_VAR_STRING = 253;
	FIELD_TYPE_STRING = 254;



(* Functions *)

<*EXTERNAL mysql_num_rows*> PROCEDURE NumRows(res: ResRef): my_ulonglong;
<*EXTERNAL mysql_num_fields*> PROCEDURE NumFields(res: ResRef): unsigned_int;
<*EXTERNAL mysql_eof*> PROCEDURE EOF(res: ResRef): my_bool;
<*EXTERNAL mysql_fetch_field_direct*> PROCEDURE FetchFieldFirect(res: ResRef; fieldnr: unsigned_int): FieldRef;
<*EXTERNAL mysql_fetch_fields*> PROCEDURE FetchFields(res: ResRef): FieldRef;
<*EXTERNAL mysql_row_tell*> PROCEDURE RowTell(res: ResRef): RowsRef;
<*EXTERNAL mysql_field_tell*> PROCEDURE FieldTell(res: ResRef): unsigned_int;

<*EXTERNAL mysql_field_count*> PROCEDURE FieldCount(mysql: T): unsigned_int;
<*EXTERNAL mysql_affected_rows*> PROCEDURE AffectedRows(mysql: T): my_ulonglong;
<*EXTERNAL mysql_insert_id*> PROCEDURE InsertId(mysql: T): my_ulonglong;
<*EXTERNAL mysql_errno*> PROCEDURE ErrNo(mysql: T): unsigned_int;
<*EXTERNAL mysql_error*> PROCEDURE Error(mysql: T): char_star;
<*EXTERNAL mysql_info*> PROCEDURE Info(mysql: T): char_star;
<*EXTERNAL mysql_thread_id*> PROCEDURE ThreadId(mysql: T): unsigned_long;
<*EXTERNAL mysql_character_set_name*> PROCEDURE CharacterSetName(mysql: T): char_star;

<*EXTERNAL mysql_init*> PROCEDURE Init(mysql: T): T;
<*EXTERNAL mysql_connect*> PROCEDURE Connect(mysql: T; host: char_star; 
	user: char_star; passwd: char_star): T;
<*EXTERNAL mysql_change_user*> PROCEDURE ChangeUser(mysql: T; user: char_star; 
	passwd: char_star; db: char_star): my_bool;
<*EXTERNAL mysql_real_connect*> PROCEDURE RealConnect(
	mysql: T; 
	host: char_star;
	user: char_star;
	passwd: char_star;
	db: char_star;
	port: unsigned_int;
	unix_socket: char_star;
	clientflag: unsigned_int): T;
<*EXTERNAL mysql_close*> PROCEDURE Close(sock: T);
<*EXTERNAL mysql_select_db*> PROCEDURE SelectDB(mysql: T; db: char_star): int;
<*EXTERNAL mysql_query*> PROCEDURE Query(mysql: T; q: char_star): int;
<*EXTERNAL mysql_send_query*> PROCEDURE SendQuery(mysql: T; q: char_star; length: unsigned_int): int;
<*EXTERNAL mysql_read_query_result*> PROCEDURE ReadQueryResult(mysql: T): int;
<*EXTERNAL mysql_real_query*> PROCEDURE RealQuery(mysql: T; q: char_star; length: unsigned_int): int;
<*EXTERNAL mysql_create_db*> PROCEDURE CreateDB(mysql: T; DB: char_star): int;
<*EXTERNAL mysql_drop_db*> PROCEDURE DropDB(mysql: T; DB: char_star): int;
<*EXTERNAL mysql_shutdown*> PROCEDURE Shutdown(mysql: T): int;
<*EXTERNAL mysql_dump_debug_info*> PROCEDURE DumpDebugInfo(mysql: T): int;
<*EXTERNAL mysql_refresh*> PROCEDURE Refresh(mysql: T; refresh_options: unsigned_int): int;
<*EXTERNAL mysql_kill*> PROCEDURE Kill(mysql: T; pid: unsigned_long): int;
<*EXTERNAL mysql_ping*> PROCEDURE Ping(mysql: T): int;
<*EXTERNAL mysql_stat*> PROCEDURE Stat(mysql: T): char_star;
<*EXTERNAL mysql_get_server_info*> PROCEDURE GetServerInfo(mysql: T): char_star;
<*EXTERNAL mysql_get_client_info*> PROCEDURE GetClientInfo(): char_star;
<*EXTERNAL mysql_get_host_info*> PROCEDURE GetHostInfo(mysql: T): char_star;
<*EXTERNAL mysql_get_proto_info*> PROCEDURE GetProtoInfo(mysql: T): unsigned_int;
<*EXTERNAL mysql_list_dbs*> PROCEDURE ListDBs(mysql: T; wild: char_star): ResRef;
<*EXTERNAL mysql_list_tables*> PROCEDURE ListTables(mysql: T; wild: char_star): ResRef;
<*EXTERNAL mysql_list_fields*> PROCEDURE ListFields(mysql: T; table: char_star; wild: char_star): ResRef;
<*EXTERNAL mysql_list_processes*> PROCEDURE ListProcesses(mysql: T): ResRef;
<*EXTERNAL mysql_store_result*> PROCEDURE StoreResult(mysql: T): ResRef;
<*EXTERNAL mysql_use_result*> PROCEDURE UseResult(mysql: T): ResRef;
<*EXTERNAL mysql_options*> PROCEDURE Options(mysql: T; option: mysql_option; arg: char_star): int;

<*EXTERNAL mysql_free_result*> PROCEDURE FreeResult(result: ResRef);
<*EXTERNAL mysql_data_seek*> PROCEDURE DataSeek(result: ResRef; offset: my_ulonglong);
<*EXTERNAL mysql_row_seek*> PROCEDURE RowSeek(result: ResRef; offset: RowOffset): RowOffset;
<*EXTERNAL mysql_field_seek*> PROCEDURE FieldSeek(result: ResRef; offset: FieldOffset): FieldOffset;
<*EXTERNAL mysql_fetch_row*> PROCEDURE FetchRow(result: ResRef): Row;
<*EXTERNAL mysql_fetch_lengths*> PROCEDURE FetchLengths(result: ResRef): unsigned_long_star;
<*EXTERNAL mysql_fetch_field*> PROCEDURE FetchField(result: ResRef): FieldRef;
<*EXTERNAL mysql_escape_string*> PROCEDURE EscapeString(to: char_star; from: char_star; from_length: unsigned_long): unsigned_long;
<*EXTERNAL mysql_real_escape_string*> PROCEDURE RealEscapeString(
	mysql: T;
	to: char_star;
	from: char_star;
	length: unsigned_long): unsigned_long;
<*EXTERNAL mysql_debug*> PROCEDURE Debug(debug: char_star);

END MySQL.
