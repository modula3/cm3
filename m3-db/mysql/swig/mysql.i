%module MySQL

/*
  This is a swig module to produces a set of Modula3 modules
  to access a MySQL database.
  Peter McKinna - Jan 2010
*/

%pragma(modula3) library="m3mysql";
%pragma(modula3) unsafe="true";

#define STDCALL
#define result res

%insert(m3makefile) %{
% compiled / works with with CM3 ver 5.8
import_lib("mysqlclient","/usr/lib")
include_dir("class")
%}

%rename("Init") mysql_init;
%rename("StoreResult") mysql_store_result;
%rename("UseResult") mysql_use_result;
%rename("NumRows") mysql_num_rows;
%rename("AffectedRows") mysql_affected_rows;
%rename("InsertId") mysql_insert_id;
%rename("ThreadId") mysql_thread_id;
%rename("NumFields") mysql_num_fields;
%rename("FieldCount") mysql_field_count;
%rename("Errno") mysql_errno;
%rename("WarningCount") mysql_warning_count;
%rename("Error") mysql_error;
%rename("Sqlstate") mysql_sqlstate;
%rename("Info") mysql_info;
%rename("CharacterSetName") mysql_character_set_name;
%rename("SetCharacterSet") mysql_set_character_set;
%rename("SelectDb") mysql_select_db;
%rename("Query") mysql_query;
%rename("SendQuery") mysql_send_query;
%rename("RealQuery") mysql_real_query;
%rename("Eof") mysql_eof;
%rename("SslSet") mysql_ssl_set;
%rename("ChangeUser") mysql_change_user;
%rename("MasterQuery") mysql_master_query;
%rename("MasterSendQuery") mysql_master_send_query;
%rename("SlaveQuery") mysql_slave_query;
%rename("SlaveSendQuery") mysql_slave_send_query;
%rename("ThreadInit") mysql_thread_init;
%rename("ThreadEnd") mysql_thread_end;
%rename("RealConnect") mysql_real_connect;
%rename("FieldTell") mysql_field_tell;
%rename("RowTell") mysql_row_tell;
%rename("EnableRplParse") mysql_enable_rpl_parse;
%rename("DisableRplParse") mysql_disable_rpl_parse;
%rename("RplParseEnabled") mysql_rpl_parse_enabled;
%rename("EnableReadsFromMaster") mysql_enable_reads_from_master;
%rename("DisableReadsFromMaster") mysql_disable_reads_from_master;
%rename("RplQueryType") mysql_rpl_query_type;
%rename("ReadsFromMasterEnabled") mysql_reads_from_master_enabled;
%rename("RplProbe") mysql_rpl_probe;
%rename("SetMaster") mysql_set_master;
%rename("AddSlave") mysql_add_slave;
%rename("Shutdown") mysql_shutdown;
%rename("DumpDebugInfo") mysql_dump_debug_info;
%rename("Refresh") mysql_refresh;
%rename("Kill") mysql_kill;
%rename("SetServerOption") mysql_set_server_option;
%rename("Ping") mysql_ping;
%rename("Stat") mysql_stat;
%rename("GetServerInfo") mysql_get_server_info;
%rename("GetClientInfo") mysql_get_client_info;
%rename("GetClientVersion") mysql_get_client_version;
%rename("GetHostInfo") mysql_get_host_info;
%rename("GetServerVersion") mysql_get_server_version;
%rename("GetProtoInfo") mysql_get_proto_info;
%rename("ListDbs") mysql_list_dbs;
%rename("ListTables") mysql_list_tables;
%rename("ListProcesses") mysql_list_processes;
%rename("Options") mysql_options;
%rename("FreeResult") mysql_free_result;
%rename("DataSeek") mysql_data_seek;
%rename("RowSeek") mysql_row_seek;
%rename("FieldSeek") mysql_field_seek;
%rename("FetchLengths") mysql_fetch_lengths;
%rename("ListFields") mysql_list_fields;
%rename("EscapeString") mysql_escape_string;
%rename("HexString") mysql_hex_string;
%rename("RealEscapeString") mysql_real_escape_string;
%rename("RemoveEscape") myodbc_remove_escape;
%rename("Debug") mysql_debug;
%rename("ThreadSafe") mysql_thread_safe;
%rename("Embedded") mysql_embedded;
%rename("ReadQueryResult") mysql_read_query_result;
%rename("FetchFieldDirect") mysql_fetch_field_direct;
%rename("FetchField") mysql_fetch_field;
%rename("FetchFields") mysql_fetch_fields;
%rename("FetchRow") mysql_fetch_row;
%rename("ServerInit") mysql_server_init;
%rename("ServerEnd") mysql_server_end;
%rename("Commit") mysql_commit;
%rename("Rollback") mysql_rollback;
%rename("Autocommit") mysql_autocommit;
%rename("MoreResults") mysql_more_results;
%rename("NextResult") mysql_next_result;
%rename("Close") mysql_close;
%rename("ManagerInit") mysql_manager_init;
%rename("ManagerConnect") mysql_manager_connect;
%rename("ManagerClose") mysql_manager_close;
%rename("ManagerCommand") mysql_manager_command;
%rename("ManagerFetchLine") mysql_manager_fetch_line;
%rename("GetParameters") mysql_get_parameters;
%rename("StmtInit") mysql_stmt_init;
%rename("StmtPrepare") mysql_stmt_prepare;
%rename("StmtExecute") mysql_stmt_execute;
%rename("StmtFetch") mysql_stmt_fetch;
%rename("StmtFetchColumn") mysql_stmt_fetch_column;
%rename("StmtStoreResult") mysql_stmt_store_result;
%rename("StmtParamCount") mysql_stmt_param_count;
%rename("StmtAttrSet") mysql_stmt_attr_set;
%rename("StmtAttrGet") mysql_stmt_attr_get;
%rename("StmtBindParam") mysql_stmt_bind_param;
%rename("StmtBindResult") mysql_stmt_bind_result;
%rename("StmtClose") mysql_stmt_close;
%rename("StmtReset") mysql_stmt_reset;
%rename("StmtFreeResult") mysql_stmt_free_result;
%rename("StmtSendLongData") mysql_stmt_send_long_data;
%rename("StmtResultMetadata") mysql_stmt_result_metadata;
%rename("StmtParamMetadata") mysql_stmt_param_metadata;
%rename("StmtErrno") mysql_stmt_errno;
%rename("StmtError") mysql_stmt_error;
%rename("StmtSqlstate") mysql_stmt_sqlstate;
%rename("StmtRowSeek") mysql_stmt_row_seek;
%rename("StmtRowTell") mysql_stmt_row_tell;
%rename("StmtDataSeek") mysql_stmt_data_seek;
%rename("StmtNumRows") mysql_stmt_num_rows;
%rename("StmtAffectedRows") mysql_stmt_affected_rows;
%rename("StmtInsertId") mysql_stmt_insert_id;
%rename("StmtFieldCount") mysql_stmt_field_count;
%rename("GetCharSetInfo") mysql_get_character_set_info;
%rename("OdbcEscapeString") mysql_odbc_escape_string;
%rename("SetLocalInfileHandler") mysql_set_local_infile_handler;
%rename("SetLocalInfileDefault") mysql_set_local_infile_default;


%include "mysqltypes.i"
%include "mysqlmaps.i"
%include "mysqlapi.i"
