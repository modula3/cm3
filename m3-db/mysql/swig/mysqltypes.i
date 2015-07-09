// mysqltypes.i

/*
  Here are the definitions of the major data structures from the mysql.h and mysqlcom.h
  files hand translated into M3 definitions

*/

#define NOT_NULL_FLAG	1		/* Field can't be NULL */
#define PRI_KEY_FLAG	2		/* Field is part of a primary key */
#define UNIQUE_KEY_FLAG 4		/* Field is part of a unique key */
#define MULTIPLE_KEY_FLAG 8		/* Field is part of a key */
#define BLOB_FLAG	16		/* Field is a blob */
#define UNSIGNED_FLAG	32		/* Field is unsigned */
#define ZEROFILL_FLAG	64		/* Field is zerofill */
#define BINARY_FLAG	128		/* Field is binary   */

/* The following are only sent to new clients */
#define ENUM_FLAG	256		/* field is an enum */
#define AUTO_INCREMENT_FLAG 512		/* field is a autoincrement field */
#define TIMESTAMP_FLAG	1024		/* Field is a timestamp */
#define SET_FLAG	2048		/* field is a set */
#define NO_DEFAULT_VALUE_FLAG 4096	/* Field doesn't have default value */
#define NUM_FLAG	32768		/* Field is num (for clients) */


%insert(m3rawintf) %{
TYPE
  my_ulonglong = C.unsigned_long_long;
  my_bool = C.char;
  gptr = C.char_star;

  OPAQUE = RECORD END;
  
  DYNAMIC_ARRAY = UNTRACED BRANDED "DYNAMIC_ARRAY" REF OPAQUE;
  USED_MEM = UNTRACED BRANDED "USED_MEM" REF OPAQUE;
  MEM_ROOT = UNTRACED BRANDED "MEM_ROOT" REF OPAQUE;
  LIST = UNTRACED BRANDED "LIST" REF OPAQUE;
  NET = UNTRACED BRANDED "NET" REF OPAQUE;

  MYSQL_FIELD_REC = RECORD
    name          : C.char_star;
    org_name      : C.char_star;
    table         : C.char_star;
    org_table     : C.char_star;
    db            : C.char_star;
    catalog       : C.char_star;
    def           : C.char_star;
    length        : C.unsigned_long;
    max_length    : C.unsigned_long;
    name_length   : C.unsigned_int;
    org_name_length : C.unsigned_int;
    table_length  : C.unsigned_int;
    org_table_length : C.unsigned_int;
    db_length     : C.unsigned_int;
    catalog_length : C.unsigned_int;
    def_length    : C.unsigned_int;
    flags         : C.unsigned_int;
    decimals      : C.unsigned_int;
    charsetnr     : C.unsigned_int;
    type          : C.unsigned_int;
    extension     : C.void_star;    
  END;
  MYSQL_FIELD = UNTRACED BRANDED "MYSQL_FIELD" REF MYSQL_FIELD_REC;

  MYSQL_ROW = C.char_star_star;
  MYSQL_FIELD_OFFSET = C.unsigned_int;

  MYSQL_ROW_OFFSET = UNTRACED BRANDED "ROW_OFFSET" REF OPAQUE;
  MYSQL_DATA = UNTRACED BRANDED "MYSQL_DATA" REF OPAQUE;
  
  MYSQL = UNTRACED BRANDED "MYSQL" REF OPAQUE;
  
  MYSQL_RES_REC = RECORD
    row_count   : my_ulonglong;
    fields      : MYSQL_FIELD;
    data        : MYSQL_DATA;
    data_cursor : MYSQL_ROW_OFFSET;
    lengths     : C.unsigned_long_star;
    handle      : MYSQL;
    field_alloc : MEM_ROOT;
    field_count : C.unsigned_int;
    current_field : C.unsigned_int;
    row         : MYSQL_ROW;
    current_row : MYSQL_ROW;
    eof         : my_bool;
    unbuffered_fetch_cancelled : my_bool;
    methods     : MYSQL_METHODS;
  END;  
  MYSQL_RES = UNTRACED BRANDED "MYSQL_RES" REF MYSQL_RES_REC;
  
  MYSQL_PARAMETERS = UNTRACED BRANDED "MYSQL_PARAMETERS" REF OPAQUE;
  MYSQL_BIND = UNTRACED BRANDED "MYSQL_BIND" REF OPAQUE;
  MYSQL_STMT = UNTRACED BRANDED "MYSQL_STMT" REF OPAQUE;
  MYSQL_OPTIONS = UNTRACED BRANDED "MYSQL_OPTIONS" REF OPAQUE;
  MYSQL_CHARSET_INFO = UNTRACED BRANDED "MYSQL_CHARSET_INFO" REF OPAQUE;
  MYSQL_METHODS = UNTRACED BRANDED "MYSQL_METHODS" REF OPAQUE;
  
  InitRawCBT = PROCEDURE(p1 : REF C.void_star; p2 : C.char_star; p3 : C.void_star) : C.int;
  ReadRawCBT = PROCEDURE(p1 : C.void_star; p2 : C.char_star; p3 : C.unsigned_int) : C.int;
  ErrorRawCBT = PROCEDURE(p1 : C.void_star; p2 : C.char_star; p3 : C.unsigned_int) : C.int;
  EndRawCBT = PROCEDURE(p1 : C.void_star);

  ExtendRawCBT = PROCEDURE(p1 : C.void_star; p2 : C.char_star; p3 : REF C.unsigned_long) : C.char_star;
%}

enum mysql_option 
{
  MYSQL_OPT_CONNECT_TIMEOUT, MYSQL_OPT_COMPRESS, MYSQL_OPT_NAMED_PIPE,
  MYSQL_INIT_COMMAND, MYSQL_READ_DEFAULT_FILE, MYSQL_READ_DEFAULT_GROUP,
  MYSQL_SET_CHARSET_DIR, MYSQL_SET_CHARSET_NAME, MYSQL_OPT_LOCAL_INFILE,
  MYSQL_OPT_PROTOCOL, MYSQL_SHARED_MEMORY_BASE_NAME, MYSQL_OPT_READ_TIMEOUT,
  MYSQL_OPT_WRITE_TIMEOUT, MYSQL_OPT_USE_RESULT,
  MYSQL_OPT_USE_REMOTE_CONNECTION, MYSQL_OPT_USE_EMBEDDED_CONNECTION,
  MYSQL_OPT_GUESS_CONNECTION, MYSQL_SET_CLIENT_IP, MYSQL_SECURE_AUTH,
  MYSQL_REPORT_DATA_TRUNCATION, MYSQL_OPT_RECONNECT
};

enum mysql_status 
{
  MYSQL_STATUS_READY,MYSQL_STATUS_GET_RESULT,MYSQL_STATUS_USE_RESULT
};

enum mysql_protocol_type 
{
  MYSQL_PROTOCOL_DEFAULT, MYSQL_PROTOCOL_TCP, MYSQL_PROTOCOL_SOCKET,
  MYSQL_PROTOCOL_PIPE, MYSQL_PROTOCOL_MEMORY
};

enum enum_mysql_stmt_state
{
  MYSQL_STMT_INIT_DONE= 1, MYSQL_STMT_PREPARE_DONE, MYSQL_STMT_EXECUTE_DONE,
  MYSQL_STMT_FETCH_DONE
};

enum enum_stmt_attr_type
{
  /*
    When doing mysql_stmt_store_result calculate max_length attribute
    of statement metadata. This is to be consistent with the old API, 
    where this was done automatically.
    In the new API we do that only by request because it slows down
    mysql_stmt_store_result sufficiently.
  */
  STMT_ATTR_UPDATE_MAX_LENGTH,
  /*
    unsigned long with combination of cursor flags (read only, for update,
    etc)
  */
  STMT_ATTR_CURSOR_TYPE,
  /*
    Amount of rows to retrieve from server per one fetch if using cursors.
    Accepts unsigned long attribute in the range 1 - ulong_max
  */
  STMT_ATTR_PREFETCH_ROWS
};

enum enum_field_types { MYSQL_TYPE_DECIMAL, MYSQL_TYPE_TINY,
                        MYSQL_TYPE_SHORT,  MYSQL_TYPE_LONG,
                        MYSQL_TYPE_FLOAT,  MYSQL_TYPE_DOUBLE,
                        MYSQL_TYPE_NULL,   MYSQL_TYPE_TIMESTAMP,
                        MYSQL_TYPE_LONGLONG,MYSQL_TYPE_INT24,
                        MYSQL_TYPE_DATE,   MYSQL_TYPE_TIME,
                        MYSQL_TYPE_DATETIME, MYSQL_TYPE_YEAR,
                        MYSQL_TYPE_NEWDATE, MYSQL_TYPE_VARCHAR,
                        MYSQL_TYPE_BIT,
                        MYSQL_TYPE_NEWDECIMAL=246,
                        MYSQL_TYPE_ENUM=247,
                        MYSQL_TYPE_SET=248,
                        MYSQL_TYPE_TINY_BLOB=249,
                        MYSQL_TYPE_MEDIUM_BLOB=250,
                        MYSQL_TYPE_LONG_BLOB=251,
                        MYSQL_TYPE_BLOB=252,
                        MYSQL_TYPE_VAR_STRING=253,
                        MYSQL_TYPE_STRING=254,
                        MYSQL_TYPE_GEOMETRY=255

};

%insert(m3wrapintf) %{
CONST

  CLIENT_NET_READ_TIMEOUT  = 365*24*3600;
  CLIENT_NET_WRITE_TIMEOUT = 365*24*3600;
  
 (* status return codes *)
  MYSQL_NO_DATA        = 100;
  MYSQL_DATA_TRUNCATED = 101;

TYPE
  InitCBT  = PROCEDURE(p1 : REF ADDRESS; p2 : TEXT; p3 : ADDRESS) : INTEGER;
  ReadCBT  = PROCEDURE(p1 : ADDRESS; p2 : TEXT; p3 : CARDINAL) : INTEGER;
  ErrorCBT = PROCEDURE(p1 : ADDRESS; p2 : TEXT; p3 : CARDINAL) : INTEGER;
  EndCBT   = PROCEDURE(p1 : ADDRESS);
  ExtendCBT = PROCEDURE(p1 : ADDRESS; p2 : TEXT; p3 : REF LONGINT) : TEXT;

%}