# include	"sql1.h"
# include	"sqlext1.h"



enum {
	en_AllocEnv 		= SQL_API_SQLALLOCENV,
	en_AllocConnect		= SQL_API_SQLALLOCCONNECT,
	en_Connect		= SQL_API_SQLCONNECT,
	en_DriverConnect	= SQL_API_SQLDRIVERCONNECT,
	en_BrowseConnect	= SQL_API_SQLBROWSECONNECT,

	en_DataSources		= SQL_API_SQLDATASOURCES,
	en_Drivers		= SQL_API_SQLDRIVERS,
	en_GetInfo		= SQL_API_SQLGETINFO,
	en_GetFunctions		= SQL_API_SQLGETFUNCTIONS,
	en_GetTypeInfo		= SQL_API_SQLGETTYPEINFO,

	en_SetConnectOption	= SQL_API_SQLSETCONNECTOPTION,
	en_GetConnectOption	= SQL_API_SQLGETCONNECTOPTION,
	en_SetStmtOption	= SQL_API_SQLSETSTMTOPTION,
	en_GetStmtOption	= SQL_API_SQLGETSTMTOPTION,

	en_AllocStmt		= SQL_API_SQLALLOCSTMT,
	en_Prepare		= SQL_API_SQLPREPARE,
	en_BindParameter	= SQL_API_SQLBINDPARAMETER,
	en_ParamOptions		= SQL_API_SQLPARAMOPTIONS,
	en_GetCursorName	= SQL_API_SQLGETCURSORNAME,
	en_SetCursorName	= SQL_API_SQLSETCURSORNAME,
	en_SetScrollOptions	= SQL_API_SQLSETSCROLLOPTIONS,
	en_SetParam		= SQL_API_SQLSETPARAM,

	en_Execute		= SQL_API_SQLEXECUTE,
	en_ExecDirect		= SQL_API_SQLEXECDIRECT,
	en_NativeSql		= SQL_API_SQLNATIVESQL,
	en_DescribeParam	= SQL_API_SQLDESCRIBEPARAM,
	en_NumParams		= SQL_API_SQLNUMPARAMS,
	en_ParamData		= SQL_API_SQLPARAMDATA,
	en_PutData		= SQL_API_SQLPUTDATA,

	en_RowCount		= SQL_API_SQLROWCOUNT,
	en_NumResultCols	= SQL_API_SQLNUMRESULTCOLS,
	en_DescribeCol		= SQL_API_SQLDESCRIBECOL,
	en_ColAttributes	= SQL_API_SQLCOLATTRIBUTES,
	en_BindCol		= SQL_API_SQLBINDCOL,
	en_Fetch		= SQL_API_SQLFETCH,
	en_ExtendedFetch	= SQL_API_SQLEXTENDEDFETCH,
	en_GetData		= SQL_API_SQLGETDATA,
	en_SetPos		= SQL_API_SQLSETPOS,
	en_MoreResults		= SQL_API_SQLMORERESULTS,
	en_Error		= SQL_API_SQLERROR,

	en_ColumnPrivileges	= SQL_API_SQLCOLUMNPRIVILEGES,
	en_Columns		= SQL_API_SQLCOLUMNS,
	en_ForeignKeys		= SQL_API_SQLFOREIGNKEYS,
	en_PrimaryKeys		= SQL_API_SQLPRIMARYKEYS,
	en_ProcedureColumns	= SQL_API_SQLPROCEDURECOLUMNS,
	en_Procedures		= SQL_API_SQLPROCEDURES,
	en_SpecialColumns	= SQL_API_SQLSPECIALCOLUMNS,
	en_Statistics		= SQL_API_SQLSTATISTICS,
	en_TablePrivileges	= SQL_API_SQLTABLEPRIVILEGES,
	en_Tables		= SQL_API_SQLTABLES,

	en_FreeStmt		= SQL_API_SQLFREESTMT,
	en_Cancel		= SQL_API_SQLCANCEL,
	en_Transact		= SQL_API_SQLTRANSACT,

	en_Disconnect		= SQL_API_SQLDISCONNECT,
	en_FreeConnect		= SQL_API_SQLFREECONNECT,
	en_FreeEnv		= SQL_API_SQLFREEENV,

	en_NullProc		= SYSERR
};
