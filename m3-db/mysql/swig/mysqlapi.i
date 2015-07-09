/*  the MySQL api copied from mysql.h  */


/*
  Set up and bring down the server; to ensure that applications will
  work when linked against either the standard client library or the
  embedded server library, these functions should be called.
*/
int STDCALL mysql_server_init(int argc, char **argv, char **groups);
void STDCALL mysql_server_end(void);
/*
  mysql_server_init/end need to be called when using libmysqld or
  libmysqlclient (exactly, mysql_server_init() is called by mysql_init() so
  you don't need to call it explicitely; but you need to call
  mysql_server_end() to free memory). The names are a bit misleading
  (mysql_SERVER* to be used when using libmysqlCLIENT). So we add more general
  names which suit well whether you're using libmysqld or libmysqlclient. We
  intend to promote these aliases over the mysql_server* ones.
*/

MYSQL_PARAMETERS *STDCALL mysql_get_parameters(void);

/*
  Set up and bring down a thread; these function should be called
  for each thread in an application which opens at least one MySQL
  connection.  All uses of the connection(s) should be between these
  function calls.
*/
my_bool STDCALL mysql_thread_init(void);
void STDCALL mysql_thread_end(void);

/*
  Functions to get information from the MYSQL and MYSQL_RES structures
  Should definitely be used if one uses shared libraries.
*/

my_ulonglong STDCALL mysql_num_rows(MYSQL_RES *res);
unsigned int STDCALL mysql_num_fields(MYSQL_RES *res);
my_bool STDCALL mysql_eof(MYSQL_RES *res);
MYSQL_FIELD *STDCALL mysql_fetch_field_direct(MYSQL_RES *res,
					      unsigned int fieldnr);
MYSQL_FIELD * STDCALL mysql_fetch_fields(MYSQL_RES *res);
MYSQL_ROW_OFFSET STDCALL mysql_row_tell(MYSQL_RES *res);
MYSQL_FIELD_OFFSET STDCALL mysql_field_tell(MYSQL_RES *res);

unsigned int STDCALL mysql_field_count(MYSQL *mysql);
my_ulonglong STDCALL mysql_affected_rows(MYSQL *mysql);
my_ulonglong STDCALL mysql_insert_id(MYSQL *mysql);
unsigned int STDCALL mysql_errno(MYSQL *mysql);
const char * STDCALL mysql_error(MYSQL *mysql);
const char *STDCALL mysql_sqlstate(MYSQL *mysql);
unsigned int STDCALL mysql_warning_count(MYSQL *mysql);
const char * STDCALL mysql_info(MYSQL *mysql);
unsigned long STDCALL mysql_thread_id(MYSQL *mysql);
const char * STDCALL mysql_character_set_name(MYSQL *mysql);
int          STDCALL mysql_set_character_set(MYSQL *mysql, const char *csname);

MYSQL *		STDCALL mysql_init(MYSQL *mysql);
my_bool		STDCALL mysql_ssl_set(MYSQL *mysql, const char *key,
				      const char *cert, const char *ca,
				      const char *capath, const char *cipher);
my_bool		STDCALL mysql_change_user(MYSQL *mysql, const char *user, 
					  const char *passwd, const char *db);
MYSQL *		STDCALL mysql_real_connect(MYSQL *mysql, const char *host,
					   const char *user,
					   const char *passwd,
					   const char *db,
					   unsigned int port,
					   const char *unix_socket,
					   unsigned long clientflag);
int		STDCALL mysql_select_db(MYSQL *mysql, const char *db);
int		STDCALL mysql_query(MYSQL *mysql, const char *q);
int		STDCALL mysql_send_query(MYSQL *mysql, const char *q,
					 unsigned long length);
int		STDCALL mysql_real_query(MYSQL *mysql, const char *q,
					unsigned long length);
MYSQL_RES *     STDCALL mysql_store_result(MYSQL *mysql);
MYSQL_RES *     STDCALL mysql_use_result(MYSQL *mysql);

void        STDCALL mysql_get_character_set_info(MYSQL *mysql,
                           MY_CHARSET_INFO *charset);

/* local infile support */


void
mysql_set_local_infile_handler(MYSQL *mysql,
                               int (*local_infile_init)(void **, const char *,
                            void *),
                               int (*local_infile_read)(void *, char *,
							unsigned int),
                               void (*local_infile_end)(void *),
                               int (*local_infile_error)(void *, char*,
							 unsigned int),
                               void *userdata);

void
mysql_set_local_infile_default(MYSQL *mysql);


int		STDCALL mysql_shutdown(MYSQL *mysql,
                                       enum mysql_enum_shutdown_level
                                       shutdown_level);
int		STDCALL mysql_dump_debug_info(MYSQL *mysql);
int		STDCALL mysql_refresh(MYSQL *mysql,
				     unsigned int refresh_options);
int		STDCALL mysql_kill(MYSQL *mysql,unsigned long pid);
int		STDCALL mysql_set_server_option(MYSQL *mysql,
						enum enum_mysql_set_option
						option);
int		STDCALL mysql_ping(MYSQL *mysql);
const char *	STDCALL mysql_stat(MYSQL *mysql);
const char *	STDCALL mysql_get_server_info(MYSQL *mysql);
const char *	STDCALL mysql_get_client_info(void);
unsigned long	STDCALL mysql_get_client_version(void);
const char *	STDCALL mysql_get_host_info(MYSQL *mysql);
unsigned long	STDCALL mysql_get_server_version(MYSQL *mysql);
unsigned int	STDCALL mysql_get_proto_info(MYSQL *mysql);
MYSQL_RES *	STDCALL mysql_list_dbs(MYSQL *mysql,const char *wild);
MYSQL_RES *	STDCALL mysql_list_tables(MYSQL *mysql,const char *wild);
MYSQL_RES *	STDCALL mysql_list_processes(MYSQL *mysql);
int		STDCALL mysql_options(MYSQL *mysql,enum mysql_option option,
				      const char *arg);
void		STDCALL mysql_free_result(MYSQL_RES *result);
void		STDCALL mysql_data_seek(MYSQL_RES *result,
					my_ulonglong offset);
MYSQL_ROW_OFFSET STDCALL mysql_row_seek(MYSQL_RES *result,
						MYSQL_ROW_OFFSET offset);
MYSQL_FIELD_OFFSET STDCALL mysql_field_seek(MYSQL_RES *result,
					   MYSQL_FIELD_OFFSET offset);
MYSQL_ROW	STDCALL mysql_fetch_row(MYSQL_RES *result);
unsigned long * STDCALL mysql_fetch_lengths(MYSQL_RES *result);
MYSQL_FIELD *	STDCALL mysql_fetch_field(MYSQL_RES *result);
MYSQL_RES *     STDCALL mysql_list_fields(MYSQL *mysql, const char *table,
					  const char *wild);
unsigned long	STDCALL mysql_escape_string(char *to,const char *from,
					    unsigned long from_length);
unsigned long	STDCALL mysql_hex_string(char *to,const char *from,
                                         unsigned long from_length);
unsigned long STDCALL mysql_real_escape_string(MYSQL *mysql,
					       char *to,const char *from,
					       unsigned long length);
void		STDCALL mysql_debug(const char *debug);
 
void 		STDCALL myodbc_remove_escape(MYSQL *mysql,char *name);
unsigned int	STDCALL mysql_thread_safe(void);
my_bool		STDCALL mysql_embedded(void);
my_bool         STDCALL mysql_read_query_result(MYSQL *mysql);

MYSQL_STMT * STDCALL mysql_stmt_init(MYSQL *mysql);
int STDCALL mysql_stmt_prepare(MYSQL_STMT *stmt, const char *query,
                               unsigned long length);
int STDCALL mysql_stmt_execute(MYSQL_STMT *stmt);
int STDCALL mysql_stmt_fetch(MYSQL_STMT *stmt);
int STDCALL mysql_stmt_fetch_column(MYSQL_STMT *stmt, MYSQL_BIND *bind, 
                                    unsigned int column,
                                    unsigned long offset);
int STDCALL mysql_stmt_store_result(MYSQL_STMT *stmt);
unsigned long STDCALL mysql_stmt_param_count(MYSQL_STMT * stmt);
my_bool STDCALL mysql_stmt_attr_set(MYSQL_STMT *stmt,
                                    enum enum_stmt_attr_type attr_type,
                                    const void *attr);
my_bool STDCALL mysql_stmt_attr_get(MYSQL_STMT *stmt,
                                    enum enum_stmt_attr_type attr_type,
                                    void *attr);
my_bool STDCALL mysql_stmt_bind_param(MYSQL_STMT * stmt, MYSQL_BIND * bnd);
my_bool STDCALL mysql_stmt_bind_result(MYSQL_STMT * stmt, MYSQL_BIND * bnd);
my_bool STDCALL mysql_stmt_close(MYSQL_STMT * stmt);
my_bool STDCALL mysql_stmt_reset(MYSQL_STMT * stmt);
my_bool STDCALL mysql_stmt_free_result(MYSQL_STMT *stmt);
my_bool STDCALL mysql_stmt_send_long_data(MYSQL_STMT *stmt, 
                                          unsigned int param_number,
                                          const char *data, 
                                          unsigned long length);
MYSQL_RES *STDCALL mysql_stmt_result_metadata(MYSQL_STMT *stmt);
MYSQL_RES *STDCALL mysql_stmt_param_metadata(MYSQL_STMT *stmt);
unsigned int STDCALL mysql_stmt_errno(MYSQL_STMT * stmt);
const char *STDCALL mysql_stmt_error(MYSQL_STMT * stmt);
const char *STDCALL mysql_stmt_sqlstate(MYSQL_STMT * stmt);
MYSQL_ROW_OFFSET STDCALL mysql_stmt_row_seek(MYSQL_STMT *stmt, 
                                             MYSQL_ROW_OFFSET offset);
MYSQL_ROW_OFFSET STDCALL mysql_stmt_row_tell(MYSQL_STMT *stmt);
void STDCALL mysql_stmt_data_seek(MYSQL_STMT *stmt, my_ulonglong offset);
my_ulonglong STDCALL mysql_stmt_num_rows(MYSQL_STMT *stmt);
my_ulonglong STDCALL mysql_stmt_affected_rows(MYSQL_STMT *stmt);
my_ulonglong STDCALL mysql_stmt_insert_id(MYSQL_STMT *stmt);
unsigned int STDCALL mysql_stmt_field_count(MYSQL_STMT *stmt);

my_bool STDCALL mysql_commit(MYSQL * mysql);
my_bool STDCALL mysql_rollback(MYSQL * mysql);
my_bool STDCALL mysql_autocommit(MYSQL * mysql, my_bool auto_mode);
my_bool STDCALL mysql_more_results(MYSQL *mysql);
int STDCALL mysql_next_result(MYSQL *mysql);
void STDCALL mysql_close(MYSQL *sock);

