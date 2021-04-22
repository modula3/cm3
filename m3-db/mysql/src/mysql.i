%module MySQL

/*
  This is a swig module to produces a set of Modula3 modules
  to access a MySQL database.
  Revised Version
  Peter McKinna - 2021
*/

%pragma(modula3) library="m3mysql";
%pragma(modula3) unsafe="true";

%include "mysqlmaps.i"

#define STDCALL
#define result mysql_res

//removes the Ctypes import from the safe interface
%typemap("m3wrapretvar:import")  char * ""

//rename to something more M3 like
%rename("%(camelcase)s",sourcefmt="%(regex:/mysql_(.*)/\\1/)s",%$isfunction) ""; 
//some renames need extra support
%rename("Option") mysql_option;
%rename("Status") mysql_status;
%rename("ProtocolType") mysql_protocol_type;
%rename("StmtState") enum_mysql_stmt_state;
%rename("StmtAttrType") enum_stmt_attr_type;
%rename("FieldTypes") enum_field_types;
%rename("SQLMode") mysql_ssl_mode;

//ignore the struct definitions except for field
//%ignore st_mysql_field;
%ignore st_mysql_rows;
%ignore st_mysql_data;
%ignore st_mysql_options;
%ignore st_mysql_options_ci;
%ignore character_set;
%ignore st_mysql;
%ignore st_mysql_res;
%ignore st_mysql_stmt;


//ignore these for mariadb, if there is a demand
//for a binding we can try to implement it.
%ignore mysql_load_plugin;
%ignore mysql_load_plugin_v;
%ignore mysql_client_find_plugin;
%ignore mysql_client_register_plugin;
%ignore my_set_error;
%ignore mysql_optionsv;
%ignore mysql_get_optionv;
%ignore st_ma_const_string;
%ignore st_mysql_client_plugin;
%ignore st_ma_used_mem;
%ignore st_ma_mem_root;
%ignore st_mysql_time;
%ignore MYSQL_PARAMETERS;
%ignore st_mysql_client_plugin;

%ignore st_mariadb_api;
%ignore st_mariadb_methods;
%ignore mariadb_get_infov;
%ignore mariadb_field_attr;
%ignore mariadb_get_info;
%ignore mariadb_connection;
%ignore mariadb_get_charset_by_name;
%ignore mariadb_get_charset_by_nr;
%ignore mariadb_reconnect;
%ignore mariadb_cancel;
%ignore mariadb_deinitialize_ssl;
%ignore mariadb_convert_string;

//only inserted if swigged with -generatem3make
%insert(m3makefile) %{
% compiled / works with with CM3 ver 5.8
import_lib("mysqlclient","/usr/lib")
%}

%{
#include <mariadb/mysql.h>
%}

%include <mariadb/mysql.h>
