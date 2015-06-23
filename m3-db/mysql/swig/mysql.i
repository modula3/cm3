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

//want effect of these 2 combined
//%rename("%(strip:[mysql])s") "";
//%rename("%(camelcase)s") "";
//which this does
%rename("%(camelcase)s",sourcefmt="%(regex:/mysql_(.*)/\\1/)s",%$isfunction) ""; 
%rename("RemoveEscape") myodbc_remove_escape;
%rename("MySQLOption") mysql_option;
%rename("MySQLStatus") mysql_status;
%rename("MySQLProtocolType") mysql_protocol_type;
%rename("MySQLStmtState") enum_mysql_stmt_state;
%rename("MySQLStmtAttrType") enum_stmt_attr_type;
%rename("MySQLFieldTypes") enum_field_types;

%ignore mysql_escape_string;

%include "mysqltypes.i"
%include "mysqlmaps.i"
%include "mysqlapi.i"

