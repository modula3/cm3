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
#define PART_KEY_FLAG	16384		/* Intern; Part of some key */
#define GROUP_FLAG	32768		/* Intern: Group field */
#define UNIQUE_FLAG	65536		/* Intern: Used by sql_yacc */
#define BINCMP_FLAG	131072		/* Intern: Used by sql_yacc */


%insert(m3rawintf) %{
TYPE
  my_ulonglong = C.unsigned_long_long;
  my_bool = C.char;
  gptr = C.char_star;
%}

/*
typedef struct st_dynamic_array
{
  char *buffer;
  uint elements,max_element;
  uint alloc_increment;
  uint size_of_element;
} DYNAMIC_ARRAY;
*/

%insert(m3rawintf) %{
TYPE

  DYNAMIC_ARRAY = RECORD
    buffer : C.char_star;
    elements,max_element : C.unsigned_int;
    alloc_imcrement : C.unsigned_int;
    size_of_element : C.unsigned_int;
  END;
  RefDynamicArrayT = UNTRACED BRANDED REF DYNAMIC_ARRAY;
%}

/*
typedef struct st_used_mem
{                                  // struct for once_alloc (block) 
  struct st_used_mem *next;        // Next block in use 
  unsigned int  left;              // memory left in block  
  unsigned int  size;              // size of block 
} USED_MEM;


typedef struct st_mem_root
{
  USED_MEM *free;                  // blocks with free memory in it 
  USED_MEM *used;                  // blocks almost without free memory 
  USED_MEM *pre_alloc;             // preallocated block 
  // if block have less memory it will be put in 'used' list 
  unsigned int min_malloc;
  unsigned int block_size;         // initial block size 
  unsigned int block_num;          // allocated blocks counter 
  //
  //   first free block in queue test counter (if it exceed
  //   MAX_BLOCK_USAGE_BEFORE_DROP block will be dropped in 'used' list)
  //
  unsigned int first_block_usage;

  void (*error_handler)(void);
} MEM_ROOT;
*/

%insert(m3rawintf) %{
TYPE

  USED_MEM = RECORD
    next : RefUsedMemT;
    left : C.unsigned_int;
    size : C.unsigned_int;
  END;
  RefUsedMemT = UNTRACED BRANDED REF USED_MEM;

  MEM_ROOT = RECORD
    free : RefUsedMemT;
    used : RefUsedMemT;
    pre_alloc : RefUsedMemT;
    min_malloc : C.unsigned_int;
    block_size : C.unsigned_int; 
    block_num : C.unsigned_int;
    first_block_usage : C.unsigned_int;
    error_handler : PROCEDURE();
  END;
  RefMemRootT = UNTRACED BRANDED REF MEM_ROOT;
%}

/*
typedef struct st_list {
  struct st_list *prev,*next;
  void *data;
} LIST;
*/

%insert(m3rawintf) %{
TYPE

  LIST = RECORD
    next,prev : RefListT;
    data      : ADDRESS;
  END;
  RefListT = UNTRACED BRANDED REF LIST;
%}

/*
typedef struct st_net {
#if !defined(CHECK_EMBEDDED_DIFFERENCES) || !defined(EMBEDDED_LIBRARY)
  Vio* vio;
  unsigned char *buff,*buff_end,*write_pos,*read_pos;
  my_socket fd;                                 // For Perl DBI/dbd
  unsigned long max_packet,max_packet_size;
  unsigned int pkt_nr,compress_pkt_nr;
  unsigned int write_timeout, read_timeout, retry_count;
  int fcntl;
  my_bool compress;
  //
  //  The following variable is set if we are doing several queries in one
  //  command ( as in LOAD TABLE ... FROM MASTER ),
  //  and do not want to confuse the client with OK at the wrong time
  //
  unsigned long remain_in_buf,length, buf_length, where_b;
  unsigned int *return_status;
  unsigned char reading_or_writing;
  char save_char;
  my_bool no_send_ok;  // For SPs and other things that do multiple stmts
  my_bool no_send_eof; // For SPs' first version read-only cursors
  //
  //  Set if OK packet is already sent, and we do not need to send error
  //  messages
  //
  my_bool no_send_error;
  //
  //  Pointer to query object in query cache, do not equal NULL (0) for
  //  queries in cache that have not stored its results yet
  //
#endif
  char last_error[MYSQL_ERRMSG_SIZE], sqlstate[SQLSTATE_LENGTH+1];
  unsigned int last_errno;
  unsigned char error;

  //
  //  'query_cache_query' should be accessed only via query cache
  //  functions and methods to maintain proper locking.
  //
  gptr query_cache_query;

  my_bool report_error; // We should report error (we have unreported error)
  my_bool return_errno;
} NET;
*/

%insert(m3rawintf) %{
TYPE

  NET = RECORD
    vio: ADDRESS; (* 'Vio*' *)
    buff, buff_end, write_pos, read_pos: C.unsigned_char_star;
    fd: C.int ; (* 'my_socket' *)
    max_packet, max_packet_size : C.unsigned_long;
    pkt_nr,compress_pkt_nr : C.unsigned_int;
    write_timeout,read_timeout,retry_count : C.unsigned_int;
    fcntl: C.int;
    compress : my_bool;
    remain_in_buf,length, buf_length, where_b: C.unsigned_long ;
    return_status: C.unsigned_int_star ;
    reading_or_writing: C.unsigned_char ;
    save_char: C.char ;
    no_send_ok: my_bool ;
    no_send_eof: my_bool ;
    no_send_error: my_bool ;
    last_error: C.char_star; (* 'char[MYSQL_ERRMSG_SIZE]' *)
    last_errno:  C.unsigned_int ;
    error: C.unsigned_char ;
    query_cache_ptr : gptr;
    report_error,return_errno: my_bool ;
  END;
  RefNetT = UNTRACED BRANDED REF NET;
%}

/*
typedef struct st_mysql_field {
  char *name;                 // Name of column 
  char *org_name;             // Original column name, if an alias 
  char *table;                // Table of column if column was a field 
  char *org_table;            // Org table name, if table was an alias 
  char *db;                   // Database for table 
  char *catalog;	      // Catalog for table 
  char *def;                  // Default value (set by mysql_list_fields) 
  unsigned long length;       // Width of column (create length) 
  unsigned long max_length;   // Max width for selected set 
  unsigned int name_length;
  unsigned int org_name_length;
  unsigned int table_length;
  unsigned int org_table_length;
  unsigned int db_length;
  unsigned int catalog_length;
  unsigned int def_length;
  unsigned int flags;         // Div flags 
  unsigned int decimals;      // Number of decimals in field 
  unsigned int charsetnr;     // Character set 
  enum enum_field_types type; // Type of field. See mysql_com.h for types 
} MYSQL_FIELD;
*/

%insert(m3rawintf) %{
TYPE

  MYSQL_FIELD = RECORD
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
    type          : C.unsigned_int; (* should be enumeration but difficult to map *)

  END;
  RefMysqlFieldT = UNTRACED BRANDED REF MYSQL_FIELD;

%}

/*
typedef char **MYSQL_ROW;		// return data as array of strings
typedef unsigned int MYSQL_FIELD_OFFSET; // offset to current field
*/

%insert(m3rawintf) %{
TYPE
  MYSQL_ROW = C.char_star_star;
  MYSQL_FIELD_OFFSET = C.unsigned_int;
%}

/*
typedef struct st_mysql_rows {
  struct st_mysql_rows *next;		// list of rows 
  MYSQL_ROW data;
  unsigned long length;
} MYSQL_ROWS;

typedef MYSQL_ROWS *MYSQL_ROW_OFFSET;	// offset to current row
*/

%insert(m3rawintf) %{
TYPE

  MYSQL_ROWS = RECORD
    next : RefMysqlRowsT;
    data : MYSQL_ROW;
    length : C.unsigned_long;
  END;
  RefMysqlRowsT = UNTRACED BRANDED REF MYSQL_ROWS;

  MYSQL_ROW_OFFSET = RefMysqlRowsT;
%}

/*
typedef struct embedded_query_result EMBEDDED_QUERY_RESULT;
typedef struct st_mysql_data {
  my_ulonglong rows;
  unsigned int fields;
  MYSQL_ROWS *data;
  MEM_ROOT alloc;
  // extra info for embedded library
  struct embedded_query_result *embedded_info;
} MYSQL_DATA;
*/

%insert(m3rawintf) %{
TYPE

  MYSQL_DATA = RECORD
    rows : my_ulonglong;
    fields : C.unsigned_int;
    data : RefMysqlRowsT;
    alloc : MEM_ROOT;
    embedded_info : ADDRESS;
(*
fixme with proper type
  // extra info for embedded library
  struct embedded_query_result *embedded_info;
*)
  END;
  RefMysqlDataT = UNTRACED BRANDED REF MYSQL_DATA;
%}

/*
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
*/

%insert(m3rawintf) %{
TYPE
  mysql_option = {
    MYSQL_OPT_CONNECT_TIMEOUT, MYSQL_OPT_COMPRESS, MYSQL_OPT_NAMED_PIPE,
    MYSQL_INIT_COMMAND, MYSQL_READ_DEFAULT_FILE, MYSQL_READ_DEFAULT_GROUP,
    MYSQL_SET_CHARSET_DIR, MYSQL_SET_CHARSET_NAME, MYSQL_OPT_LOCAL_INFILE,
    MYSQL_OPT_PROTOCOL, MYSQL_SHARED_MEMORY_BASE_NAME, MYSQL_OPT_READ_TIMEOUT,
    MYSQL_OPT_WRITE_TIMEOUT, MYSQL_OPT_USE_RESULT,
    MYSQL_OPT_USE_REMOTE_CONNECTION, MYSQL_OPT_USE_EMBEDDED_CONNECTION,
    MYSQL_OPT_GUESS_CONNECTION, MYSQL_SET_CLIENT_IP, MYSQL_SECURE_AUTH,
    MYSQL_REPORT_DATA_TRUNCATION, MYSQL_OPT_RECONNECT
  };
%}

/*
struct st_mysql_options {
  unsigned int connect_timeout, read_timeout, write_timeout;
  unsigned int port, protocol;
  unsigned long client_flag;
  char *host,*user,*password,*unix_socket,*db;
  struct st_dynamic_array *init_commands;
  char *my_cnf_file,*my_cnf_group, *charset_dir, *charset_name;
  char *ssl_key;				// PEM key file 
  char *ssl_cert;				// PEM cert file 
  char *ssl_ca;					// PEM CA file 
  char *ssl_capath;				// PEM directory of CA-s? 
  char *ssl_cipher;				// cipher to use 
  char *shared_memory_base_name;
  unsigned long max_allowed_packet;
  my_bool use_ssl;				// if to use SSL or not 
  my_bool compress,named_pipe;
 //
 //  On connect, find out the replication role of the server, and
 //  establish connections to all the peers

   my_bool rpl_probe;
 //
 //  Each call to mysql_real_query() will parse it to tell if it is a read
 //  or a write, and direct it to the slave or the master

  my_bool rpl_parse;
 //
 //  If set, never read from a master, only from slave, when doing
 //  a read that is replication-aware

  my_bool no_master_reads;
#if !defined(CHECK_EMBEDDED_DIFFERENCES) || defined(EMBEDDED_LIBRARY)
  my_bool separate_thread;
#endif
  enum mysql_option methods_to_use;
  char *client_ip;
  // Refuse client connecting to server if it uses old (pre-4.1.1) protocol 
  my_bool secure_auth;
  // 0 - never report, 1 - always report (default) 
  my_bool report_data_truncation;

  // function pointers for local infile support 
  int (*local_infile_init)(void **, const char *, void *);
  int (*local_infile_read)(void *, char *, unsigned int);
  void (*local_infile_end)(void *);
  int (*local_infile_error)(void *, char *, unsigned int);
  void *local_infile_userdata;
};
*/

%insert(m3rawintf) %{
TYPE
  InitRawCBT = PROCEDURE(p1 : REF C.void_star; p2 : C.char_star; p3 : C.void_star) : C.int;
  ReadRawCBT = PROCEDURE(p1 : C.void_star; p2 : C.char_star; p3 : C.unsigned_int) : C.int;
  ErrorRawCBT = PROCEDURE(p1 : C.void_star; p2 : C.char_star; p3 : C.unsigned_int) : C.int;
  EndRawCBT = PROCEDURE(p1 : C.void_star);

  ExtendRawCBT = PROCEDURE(p1 : C.void_star; p2 : C.char_star; p3 : REF C.unsigned_long) : C.char_star;

  MYSQL_OPTIONS = RECORD
    connect_timeout, read_timeout, write_timeout, port, protocol: C.unsigned_int;
    client_flag: C.unsigned_long;
    host, user, password, unix_socket, db: C.char_star;
    init_commands: RefDynamicArrayT;
    my_cnf_file, my_cnf_group, charset_dir, charset_name : C.char_star;
    ssl_key, ssl_cert, ssl_ca : C.char_star;
    ssl_capath, ssl_cipher, shared_memory_base_name: C.char_star;
    max_allowed_packet: C.unsigned_long;
    use_ssl : my_bool;
    compress,named_pipe : my_bool;
    rpl_probe : my_bool;
    rpl_parse : my_bool;
    no_master_reads : my_bool;

    separate_thread : my_bool;

    methods_to_use : mysql_option;
    client_ip : C.char_star;
    secure_auth : my_bool;
    report_data_truncation : my_bool;

    (* function pointers for local infile support  *)

    local_infile_init : InitRawCBT;
    local_infile_read : ReadRawCBT;
    local_infile_end : EndRawCBT;
    local_infile_error : ErrorRawCBT;

    local_infile_userdata : ADDRESS;
  END;
  RefMysqlOptionsT = UNTRACED BRANDED REF MYSQL_OPTIONS;
%}

/*
enum mysql_status 
{
  MYSQL_STATUS_READY,MYSQL_STATUS_GET_RESULT,MYSQL_STATUS_USE_RESULT
};

enum mysql_protocol_type 
{
  MYSQL_PROTOCOL_DEFAULT, MYSQL_PROTOCOL_TCP, MYSQL_PROTOCOL_SOCKET,
  MYSQL_PROTOCOL_PIPE, MYSQL_PROTOCOL_MEMORY
};
//
//  There are three types of queries - the ones that have to go to
//  the master, the ones that go to a slave, and the adminstrative
//  type which must happen on the pivot connectioin
//
enum mysql_rpl_type 
{
  MYSQL_RPL_MASTER, MYSQL_RPL_SLAVE, MYSQL_RPL_ADMIN
};
*/

%insert(m3rawintf) %{
TYPE
  mysql_status = {
    MYSQL_STATUS_READY,MYSQL_STATUS_GET_RESULT,MYSQL_STATUS_USE_RESULT
  };

  mysql_protocol_type = {
    MYSQL_PROTOCOL_DEFAULT, MYSQL_PROTOCOL_TCP, MYSQL_PROTOCOL_SOCKET,
    MYSQL_PROTOCOL_PIPE, MYSQL_PROTOCOL_MEMORY
  };

  mysql_rpl_type = {
    MYSQL_RPL_MASTER, MYSQL_RPL_SLAVE, MYSQL_RPL_ADMIN
  };
%}

/*
typedef struct character_set
{
  unsigned int      number;     // character set number              
  unsigned int      state;      // character set state               
  const char        *csname;    // collation name                    
  const char        *name;      // character set name                
  const char        *comment;   // comment                           
  const char        *dir;       // character set directory           
  unsigned int      mbminlen;   // min. length for multibyte strings 
  unsigned int      mbmaxlen;   // max. length for multibyte strings 
} MY_CHARSET_INFO;
*/

%insert(m3rawintf) %{
TYPE

  MYSQL_CHARSET_INFO = RECORD
    number : C.unsigned_int;
    state : C.unsigned_int;
    csname : C.char_star;
    name : C.char_star;
    comment : C.char_star;
    dir : C.char_star;
    mbminlen: C.unsigned_int;
    mbmaxlen : C.unsigned_int;
  END;
  RefMysqlCharsT = UNTRACED BRANDED REF MYSQL_CHARSET_INFO;
%}

/*
typedef struct st_mysql
{
  NET		net;			// Communication parameters
  gptr		connector_fd;		// ConnectorFd for SSL
  char		*host,*user,*passwd,*unix_socket,*server_version,*host_info,*info;
  char          *db;
  struct charset_info_st *charset;
  MYSQL_FIELD	*fields;
  MEM_ROOT	field_alloc;
  my_ulonglong affected_rows;
  my_ulonglong insert_id;		// id if insert on table with NEXTNR
  my_ulonglong extra_info;		// Not used
  unsigned long thread_id;		// Id for connection in server
  unsigned long packet_length;
  unsigned int	port;
  unsigned long client_flag,server_capabilities;
  unsigned int	protocol_version;
  unsigned int	field_count;
  unsigned int 	server_status;
  unsigned int  server_language;
  unsigned int	warning_count;
  struct st_mysql_options options;
  enum mysql_status status;
  my_bool	free_me;		// If free in mysql_close
  my_bool	reconnect;		// set to 1 if automatic reconnect

  // session-wide random string 
  char	        scramble[SCRAMBLE_LENGTH+1];

   //Set if this is the original connection, not a master or a slave we have
  // added though mysql_rpl_probe() or mysql_set_master()/ mysql_add_slave()

  my_bool rpl_pivot;

  //  Pointers to the master, and the next slave connections, points to
  //  itself if lone connection.

  struct st_mysql* master, *next_slave;

  struct st_mysql* last_used_slave; // needed for round-robin slave pick
  // needed for send/read/store/use result to work correctly with replication
  struct st_mysql* last_used_con;

  LIST  *stmts;                     // list of all statements
  const struct st_mysql_methods *methods;
  void *thd;

  //  Points to boolean flag in MYSQL_RES  or MYSQL_STMT. We set this flag 
  //  from mysql_stmt_close if close had to cancel result set of this object.

  my_bool *unbuffered_fetch_owner;
#if defined(EMBEDDED_LIBRARY) || defined(EMBEDDED_LIBRARY_COMPATIBLE) || MYSQL_VERSION_ID >= 50100
  // needed for embedded server - no net buffer to store the 'info'
  char *info_buffer;
#endif
} MYSQL;
*/

/* statement state 
enum enum_mysql_stmt_state
{
  MYSQL_STMT_INIT_DONE= 1, MYSQL_STMT_PREPARE_DONE, MYSQL_STMT_EXECUTE_DONE,
  MYSQL_STMT_FETCH_DONE
};
*/

%insert(m3rawintf) %{
(* mysql_stmt_state *)
CONST
  MYSQL_STMT_INIT_DONE    = 1;
  MYSQL_STMT_PREPARE_DONE = 2;
  MYSQL_STMT_EXECUTE_DONE = 3;
  MYSQL_STMT_FETCH_DONE   = 4;
%}

%insert(m3rawintf) %{

CONST
  SCRAMBLE_LENGTH = 20; (* check these from com.h *)

TYPE

  MYSQL = RECORD
    net                 : RefNetT;
    connector_fd        : gptr;
    host,user,passwd,unix_socket,server_version,host_info,info : C.char_star;
    db                  : C.char_star;
    charset             : RefMysqlCharsT;
    fields              : RefMysqlFieldT;
    field_alloc         : MEM_ROOT;
    affected_rows       : my_ulonglong;
    insert_id           : my_ulonglong;
    extra_info          : my_ulonglong;
    thread_id           : C.unsigned_long;
    packet_length       : C.unsigned_long;
    port                : C.unsigned_int;
    client_flag         : C.unsigned_long;
    server_capabilities : C.unsigned_long;
    protocol_version    : C.unsigned_int;
    field_count         : C.unsigned_int;
    server_status       : C.unsigned_int;
    server_language     : C.unsigned_int;
    warning_count       : C.unsigned_int;

    options             : MYSQL_OPTIONS;
    status              : mysql_status;

    free_me             : my_bool;
    reconnect           : my_bool;
    scramble            : ARRAY [0..SCRAMBLE_LENGTH + 1] OF CHAR;
    rpl_pivot           : my_bool;
    master,next_slave   : RefMysqlT;
    last_used_slave     : RefMysqlT;
    last_used_con       : RefMysqlT;
    stmts               : RefListT;
    methods             : RefMysqlMethodsT;
    thd                 : ADDRESS;
    unbuffered_fetch_owner : UNTRACED REF my_bool;
    info_buffer         : C.char_star;
  END;
  RefMysqlT = UNTRACED BRANDED REF MYSQL;
%}


/*
typedef struct st_mysql_res {
  my_ulonglong row_count;
  MYSQL_FIELD	*fields;
  MYSQL_DATA	*data;
  MYSQL_ROWS	*data_cursor;
  unsigned long *lengths;		// column lengths of current row 
  MYSQL		*handle;		// for unbuffered reads
  MEM_ROOT	field_alloc;
  unsigned int	field_count, current_field;
  MYSQL_ROW	row;			// If unbuffered read
  MYSQL_ROW	current_row;		// buffer to current row
  my_bool	eof;			// Used by mysql_fetch_row
  // mysql_stmt_close() had to cancel this result
  my_bool       unbuffered_fetch_cancelled;  
  const struct st_mysql_methods *methods;
} MYSQL_RES;

*/

%insert(m3rawintf) %{
  MYSQL_RES = RECORD
    row_count   : my_ulonglong;
    fields      : RefMysqlFieldT;
    data        : RefMysqlDataT;
    data_cursor : RefMysqlRowsT;
    lengths     : C.unsigned_long_star;
    handle      : RefMysqlT;
    field_alloc : MEM_ROOT;
    field_count : C.unsigned_int;
    current_field : C.unsigned_int;
    row         : MYSQL_ROW;
    current_row : MYSQL_ROW;
    eof         : my_bool;
    unbuffered_fetch_cancelled : my_bool;
    methods     : RefMysqlMethodsT;
  END;
  RefMysqlResT = UNTRACED BRANDED REF MYSQL_RES;
%}

/*
typedef struct st_mysql_manager
{
  NET net;
  char *host,*user,*passwd;
  unsigned int port;
  my_bool free_me;
  my_bool eof;
  int cmd_status;
  int last_errno;
  char* net_buf,*net_buf_pos,*net_data_end;
  int net_buf_size;
  char last_error[MAX_MYSQL_MANAGER_ERR];
} MYSQL_MANAGER;

*/

%insert(m3rawintf) %{

CONST
  MAX_MYSQL_MANAGER_ERR = 256;

TYPE

  MYSQL_MANAGER = RECORD
    net          : RefNetT;
    host,user,passwd : C.char_star;
    port         : C.unsigned_int;
    free_me      : my_bool;
    eof          : my_bool;
    md_status    : C.int;
    last_errno   : C.int;
    net_buf      : C.char_star;
    net_buf_pos  : C.char_star;
    net_data_end : C.char_star;
    net_buf_size : C.int;
    last_error   : ARRAY [0..MAX_MYSQL_MANAGER_ERR + 1] OF CHAR;
  END;
  RefMysqlManagerT = UNTRACED BRANDED REF MYSQL_MANAGER;
%}

/*
typedef struct st_mysql_parameters
{
  unsigned long *p_max_allowed_packet;
  unsigned long *p_net_buffer_length;
} MYSQL_PARAMETERS;
*/

%insert(m3rawintf) %{

TYPE

  MYSQL_PARAMETERS = RECORD
    max_allowed_packet : C.unsigned_long;
    net_buffer_length : C.unsigned_long;
  END;
  RefMysqlParametersT = UNTRACED BRANDED REF MYSQL_PARAMETERS;
%}

/*
typedef struct st_mysql_bind
{
  unsigned long	*length;          // output length pointer 
  my_bool       *is_null;	  // Pointer to null indicator 
  void		*buffer;	  // buffer to get/put data 
  // set this if you want to track data truncations happened during fetch 
  my_bool       *error;
  enum enum_field_types buffer_type;	// buffer type 
  // output buffer length, must be set when fetching str/binary 
  unsigned long buffer_length;
  unsigned char *row_ptr;         // for the current data position 
  unsigned long offset;           // offset position for char/binary fetch 
  unsigned long	length_value;     // Used if length is 0 
  unsigned int	param_number;	  // For null count and error messages 
  unsigned int  pack_length;	  // Internal length for packed data 
  my_bool       error_value;      // used if error is 0 
  my_bool       is_unsigned;      // set if integer type is unsigned 
  my_bool	long_data_used;	  // If used with mysql_send_long_data 
  my_bool	is_null_value;    // Used if is_null is 0 
  void (*store_param_func)(NET *net, struct st_mysql_bind *param);
  void (*fetch_result)(struct st_mysql_bind *, MYSQL_FIELD *,
                       unsigned char **row);
  void (*skip_result)(struct st_mysql_bind *, MYSQL_FIELD *,
		      unsigned char **row);
} MYSQL_BIND;
*/

%insert(m3rawintf) %{

TYPE

  MYSQL_BIND = RECORD
    length         : UNTRACED REF C.unsigned_long;
    is_null        : UNTRACED REF my_bool;
    buffer         : ADDRESS;
    error          : UNTRACED REF my_bool;
    buffer_type    : CARDINAL;
    buffer_length  : C.unsigned_long;
    row_ptr        : UNTRACED REF C.unsigned_char;
    offset         : C.unsigned_long;
    length_value   : C.unsigned_long;
    param_number   : C.unsigned_int;
    pack_length    : C.unsigned_int;
    error_value    : my_bool;
    is_unsigned    : my_bool;
    long_data_used : my_bool;
    is_null_value  : my_bool;
    store_param_func : PROCEDURE(net : RefNetT; param : RefMysqlBindT); 
    fetch_result     : PROCEDURE(param : RefMysqlBindT; param2 : RefMysqlFieldT; row : C.unsigned_char_star_star);
    skip_result      : PROCEDURE(param : RefMysqlBindT; param2 : RefMysqlFieldT; row : C.unsigned_char_star_star);
  END;
  RefMysqlBindT = UNTRACED BRANDED REF MYSQL_BIND;
%}

/* statement handler 
typedef struct st_mysql_stmt
{
  MEM_ROOT       mem_root;             // root allocations 
  LIST           list;                 // list to keep track of all stmts 
  MYSQL          *mysql;               // connection handle 
  MYSQL_BIND     *params;              // input parameters 
  MYSQL_BIND     *bind;                // output parameters 
  MYSQL_FIELD    *fields;              // result set metadata 
  MYSQL_DATA     result;               // cached result set 
  MYSQL_ROWS     *data_cursor;         // current row in cached result 
  // copy of mysql->affected_rows after statement execution 
  my_ulonglong   affected_rows;
  my_ulonglong   insert_id;            // copy of mysql->insert_id 
  //
  //  mysql_stmt_fetch() calls this function to fetch one row (it's different
  //  for buffered, unbuffered and cursor fetch).

  int            (*read_row_func)(struct st_mysql_stmt *stmt, 
                                  unsigned char **row);
  unsigned long	 stmt_id;	       // Id for prepared statement 
  unsigned long  flags;                // i.e. type of cursor to open 
  unsigned long  prefetch_rows;        // number of rows per one COM_FETCH 
  //
  //  Copied from mysql->server_status after execute/fetch to know
  //  server-side cursor status for this statement.

  unsigned int   server_status;
  unsigned int	 last_errno;	       // error code 
  unsigned int   param_count;          // input parameter count 
  unsigned int   field_count;          // number of columns in result set 
  enum enum_mysql_stmt_state state;    // statement state 
  char		 last_error[MYSQL_ERRMSG_SIZE]; // error message 
  char		 sqlstate[SQLSTATE_LENGTH+1];
  // Types of input parameters should be sent to server 
  my_bool        send_types_to_server;
  my_bool        bind_param_done;      // input buffers were supplied 
  unsigned char  bind_result_done;     // output buffers were supplied 
  // mysql_stmt_close() had to cancel this result 
  my_bool       unbuffered_fetch_cancelled;  
  //
  //  Is set to true if we need to calculate field->max_length for 
  //  metadata fields when doing mysql_stmt_store_result.

  my_bool       update_max_length;     
} MYSQL_STMT;
*/

%insert(m3rawintf) %{

TYPE

  MYSQL_STMT = RECORD
    mem_root      : MEM_ROOT;
    list          : LIST;
    mysql         : RefMysqlT;
    params        : RefMysqlBindT;
    bind          : RefMysqlBindT;
    fields        : RefMysqlFieldT;
    result        : MYSQL_DATA;
    data_cursor   : RefMysqlRowsT;
    affected_rows : my_ulonglong;
    insert_id     : my_ulonglong;
    read_row_func : PROCEDURE (stmt : MYSQL_STMT; row : C.unsigned_char_star_star) : C.int;
    stmt_id       : C.unsigned_long;
    flags         : C.unsigned_long;
    prefetch_rows : C.unsigned_long;
    server_status : C.unsigned_int;
    last_errno    : C.unsigned_int;
    param_count   : C.unsigned_int;
    field_count   : C.unsigned_int;
    state         : C.int;
    last_error    : C.char_star; (*ARRAY[0..MYSQL_ERRMSG_SIZE-1] OF C.char;*)
    sqlstate      : C.char_star; (* ARRAY [0..SQLSTATE_LENGTH] OF C.char; *)
    send_types_to_server : my_bool;
    bind_param_done      : my_bool;
    bind_result_done     : C.unsigned_char; 
    unbuffered_fetch_cancelled : my_bool;
    update_max_length    : my_bool;
  END;
  RefMysqlStmtT = UNTRACED BRANDED REF MYSQL_STMT;
%}

%insert(m3rawintf) %{

TYPE

  enum_stmt_attr_type = {
    STMT_ATTR_UPDATE_MAX_LENGTH,
    STMT_ATTR_CURSOR_TYPE,
    STMT_ATTR_PREFETCH_ROWS
  };
%}

/*
typedef struct st_mysql_methods
{
  my_bool (*read_query_result)(MYSQL *mysql);
  my_bool (*advanced_command)(MYSQL *mysql,
			      enum enum_server_command command,
			      const char *header,
			      unsigned long header_length,
			      const char *arg,
			      unsigned long arg_length,
			      my_bool skip_check);
  MYSQL_DATA *(*read_rows)(MYSQL *mysql,MYSQL_FIELD *mysql_fields,
			   unsigned int fields);
  MYSQL_RES * (*use_result)(MYSQL *mysql);
  void (*fetch_lengths)(unsigned long *to, 
			MYSQL_ROW column, unsigned int field_count);
  void (*flush_use_result)(MYSQL *mysql);
#if !defined(MYSQL_SERVER) || defined(EMBEDDED_LIBRARY)
  MYSQL_FIELD * (*list_fields)(MYSQL *mysql);
  my_bool (*read_prepare_result)(MYSQL *mysql, MYSQL_STMT *stmt);
  int (*stmt_execute)(MYSQL_STMT *stmt);
  int (*read_binary_rows)(MYSQL_STMT *stmt);
  int (*unbuffered_fetch)(MYSQL *mysql, char **row);
  void (*free_embedded_thd)(MYSQL *mysql);
  const char *(*read_statistics)(MYSQL *mysql);
  my_bool (*next_result)(MYSQL *mysql);
  int (*read_change_user_result)(MYSQL *mysql, char *buff, const char *passwd);
  int (*read_rows_from_cursor)(MYSQL_STMT *stmt);
#endif
} MYSQL_METHODS;
*/

/*
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
*/

%insert(m3rawintf) %{

TYPE

  MYSQL_METHODS = RECORD
    read_query_result : PROCEDURE(mysql : RefMysqlT) : my_bool;
    advanced_command  : PROCEDURE(mysql : RefMysqlT; 
                                  command : CARDINAL;
                                  header : C.char_star; header_length : C.unsigned_long;
                                  arg : C.char_star; arg_length : C.unsigned_long;
                                  skip_check : my_bool) : my_bool;
    read_rows : PROCEDURE(mysql : RefMysqlT; 
                          mysql_fields : RefMysqlFieldT; 
                          fields : C.unsigned_int) : RefMysqlDataT;
    use_result : PROCEDURE(mysql : RefMysqlT) : RefMysqlResT;

    fetch_lengths : PROCEDURE(to : REF C.unsigned_long;
                              column : MYSQL_ROW;
                              field_count : C.unsigned_int);
    flush_use_result : PROCEDURE(mysql : RefMysqlT);
(* fixme add embedded library fields *)
  END;
  RefMysqlMethodsT = UNTRACED BRANDED REF MYSQL_METHODS;
%}

%insert(m3wrapintf) %{

CONST

  CLIENT_NET_READ_TIMEOUT  = 365*24*3600;
  CLIENT_NET_WRITE_TIMEOUT = 365*24*3600;

  MAX_MYSQL_MANAGER_ERR = 256;
  MAX_MYSQL_MANAGER_MSG = 256;

  MANAGER_OK           = 200;
  MANAGER_INFO         = 250;
  MANAGER_ACCESS       = 401;
  MANAGER_CLIENT_ERR   = 450;
  MANAGER_INTERNAL_ERR  = 500;

  MYSQL_NO_DATA        = 100;
  MYSQL_DATA_TRUNCATED = 101;

(* Field Types *)
  MYSQL_TYPE_DECIMAL     = 0;
  MYSQL_TYPE_TINY        = 1;
  MYSQL_TYPE_SHORT       = 2;
  MYSQL_TYPE_LONG        = 3;
  MYSQL_TYPE_FLOAT       = 4;
  MYSQL_TYPE_DOUBLE      = 5;
  MYSQL_TYPE_NULL        = 6;
  MYSQL_TYPE_TIMESTAMP   = 7;
  MYSQL_TYPE_LONGLONG    = 8;
  MYSQL_TYPE_INT24       = 9;
  MYSQL_TYPE_DATE        = 10;
  MYSQL_TYPE_TIME        = 11;
  MYSQL_TYPE_DATETIME    = 12;
  MYSQL_TYPE_YEAR        = 13;
  MYSQL_TYPE_NEWDATE     = 14;
  MYSQL_TYPE_VARCHAR     = 15;
  MYSQL_TYPE_BIT         = 16;
  MYSQL_TYPE_NEWDECIMAL  = 246;
  MYSQL_TYPE_ENUM        = 247;
  MYSQL_TYPE_SET         = 248;
  MYSQL_TYPE_TINY_BLOB   = 249;
  MYSQL_TYPE_MEDIUM_BLOB = 250;
  MYSQL_TYPE_LONG_BLOB   = 251;
  MYSQL_TYPE_BLOB        = 252;
  MYSQL_TYPE_VAR_STRING  = 253;
  MYSQL_TYPE_STRING      = 254;
  MYSQL_TYPE_GEOMETRY    = 255;


TYPE
  InitCBT  = PROCEDURE(p1 : REF ADDRESS; p2 : TEXT; p3 : ADDRESS) : INTEGER;
  ReadCBT  = PROCEDURE(p1 : ADDRESS; p2 : TEXT; p3 : CARDINAL) : INTEGER;
  ErrorCBT = PROCEDURE(p1 : ADDRESS; p2 : TEXT; p3 : CARDINAL) : INTEGER;
  EndCBT   = PROCEDURE(p1 : ADDRESS);

  ExtendCBT = PROCEDURE(p1 : ADDRESS; p2 : TEXT; p3 : REF LONGINT) : TEXT;

%}