
(* SQLext defines the Microsoft SQL Extensions.  It is based on
   the version of Microsoft's SQLext.h dated  4/10/95.  *)

UNSAFE INTERFACE SQLext;

FROM SQLtypes IMPORT SQLRETURN, SQLHWND, SQLHENV, SQLHDBC, 
   SQLHSTMT, SQLUSMALLINT, SQLUSMALLINT_star, SQLSMALLINT,
   SQLSMALLINT_star, SQLPOINTER, SQLINTEGER, SQLINTEGER_star, SQLUINTEGER,
   SQLUINTEGER_star, SQLCHAR_star;

CONST 
  (* SQLBindParameter extensions *)
  SQL_DEFAULT_PARAM            = -5;
  SQL_IGNORE                   = -6;
  SQL_LEN_DATA_AT_EXEC_OFFSET  = -100;

PROCEDURE SQL_LEN_DATA_AT_EXEC (length: INTEGER): INTEGER;
(* == RETURN (-length+SQL_LEN_DATA_AT_EXEC_OFFSET) *)

CONST
  (* Defines for SQLSetPos *)
  SQL_ENTIRE_ROWSET            = 0;

  (* Operations in SQLSetPos *)
  SQL_POSITION                 = 0;    (* 1.0 FALSE *)
  SQL_REFRESH                  = 1;    (* 1.0 TRUE *)
  SQL_UPDATE                   = 2;
  SQL_DELETE                   = 3;
  SQL_ADD                      = 4;

  (* Lock options in SQLSetPos *)
  SQL_LOCK_NO_CHANGE           = 0;    (* 1.0 FALSE *)
  SQL_LOCK_EXCLUSIVE           = 1;    (* 1.0 TRUE *)
  SQL_LOCK_UNLOCK              = 2;

  (* Macros for SQLSetPos *)

PROCEDURE SQL_POSITION_TO (hstmt: SQLHSTMT;  irow: SQLUSMALLINT): SQLRETURN;
(* == RETURN SQLSetPos (hstmt, irow, SQL_POSITION, SQL_LOCK_NO_CHANGE) *)

PROCEDURE SQL_LOCK_RECORD (hstmt: SQLHSTMT;  irow, fLock: SQLUSMALLINT): SQLRETURN;
(* == RETURN SQLSetPos (hstmt, irow, SQL_POSITION, fLock) *)

PROCEDURE SQL_REFRESH_RECORD (hstmt: SQLHSTMT;  irow, fLock: SQLUSMALLINT): SQLRETURN;
(* == RETURN SQLSetPos (hstmt, irow, SQL_REFRESH, fLock)  *)

PROCEDURE SQL_UPDATE_RECORD (hstmt: SQLHSTMT;  irow: SQLUSMALLINT): SQLRETURN;
(* == RETURN SQLSetPos (hstmt, irow, SQL_UPDATE, SQL_LOCK_NO_CHANGE)  *)

PROCEDURE SQL_DELETE_RECORD (hstmt: SQLHSTMT;  irow: SQLUSMALLINT): SQLRETURN;
(* == RETURN SQLSetPos (hstmt, irow, SQL_DELETE, SQL_LOCK_NO_CHANGE)  *)

PROCEDURE SQL_ADD_RECORD (hstmt: SQLHSTMT;  irow: SQLUSMALLINT): SQLRETURN;
(* == RETURN SQLSetPos (hstmt, irow, SQL_ADD, SQL_LOCK_NO_CHANGE)  *)

(* Level 1 Prototypes *)

CONST
  (* Options for SQLDriverConnect *)
  SQL_DRIVER_NOPROMPT             = 0;
  SQL_DRIVER_COMPLETE             = 1;
  SQL_DRIVER_PROMPT               = 2;
  SQL_DRIVER_COMPLETE_REQUIRED    = 3;

<*EXTERNAL SQLDriverConnect *>
PROCEDURE SQLDriverConnect (
            hdbc:              SQLHDBC;
            hwnd:              SQLHWND;
            szConnStrIn:       SQLCHAR_star;
            cbConnStrIn:       SQLSMALLINT;
            szConnStrOut:      SQLCHAR_star;
            cbConnStrOutMax:   SQLSMALLINT;
            pcbConnStrOut:     SQLSMALLINT_star;
            fDroverCompletion: SQLUSMALLINT): SQLRETURN;

(* Level 2 Functions *)

CONST
  (* SQLExtendedFetch "fFetchType" values *)
  SQL_FETCH_NEXT                   = 1;
  SQL_FETCH_FIRST                  = 2;
  SQL_FETCH_LAST                   = 3;
  SQL_FETCH_PRIOR                  = 4;
  SQL_FETCH_ABSOLUTE               = 5;
  SQL_FETCH_RELATIVE               = 6;
  SQL_FETCH_BOOKMARK               = 8;

  (* SQLExtendedFetch "rgfRowStatus" element values *)
  SQL_ROW_SUCCESS                  = 0;
  SQL_ROW_DELETED                  = 1;
  SQL_ROW_UPDATED                  = 2;
  SQL_ROW_NOROW                    = 3;
  SQL_ROW_ADDED                    = 4;
  SQL_ROW_ERROR                    = 5;

  (* Defines for SQLForeignKeys (returned in result set) *)
  SQL_CASCADE                      = 0;
  SQL_RESTRICT                     = 1;
  SQL_SET_NULL                     = 2;
  SQL_NO_ACTION                    = 3;
  SQL_SET_DEFAULT                  = 4;

  (* Defines for SQLBindParameter and SQLProcedureColumns (returned in the result set) *)
  SQL_PARAM_TYPE_UNKNOWN           = 0;
  SQL_PARAM_INPUT                  = 1;
  SQL_PARAM_INPUT_OUTPUT           = 2;
  SQL_RESULT_COL                   = 3;
  SQL_PARAM_OUTPUT                 = 4;
  SQL_RETURN_VALUE                 = 5;

  (* Defines for SQLProcedures (returned in the result set) *)
  SQL_PT_UNKNOWN                   = 0;
  SQL_PT_PROCEDURE                 = 1;
  SQL_PT_FUNCTION                  = 2;

  (* Defines used by Driver Manager when mapping SQLSetParam to SQLBindParameter *)
  SQL_PARAM_TYPE_DEFAULT           = SQL_PARAM_INPUT_OUTPUT;
  SQL_SETPARAM_VALUE_MAX           = -1;

(* Level 2 Prototypes *)

<*EXTERNAL SQLBrowseConnect *>
PROCEDURE SQLBrowseConnect (
            hdbc:            SQLHDBC;
            szConnStrIn:     SQLCHAR_star;
            cbConnStrIn:     SQLSMALLINT;
            szConnStrOut:    SQLCHAR_star;
            cbConnStrOutMax: SQLSMALLINT;
            pcbConnStrOut:   SQLSMALLINT_star): SQLRETURN;

<*EXTERNAL SQLColumnPrivileges *>
PROCEDURE SQLColumnPrivileges (
            hstmt:         SQLHSTMT;
            szCatalogName: SQLCHAR_star;
            cbCatalogName: SQLSMALLINT;
            szSchemaName:  SQLCHAR_star;
            cbSchemaName:  SQLSMALLINT;
            szTableName:   SQLCHAR_star;
            cbTableName:   SQLSMALLINT;
            szColumnName:  SQLCHAR_star;
            cbColumnName:  SQLSMALLINT): SQLRETURN;

<*EXTERNAL SQLDescribeParam *>
PROCEDURE SQLDescribeParam (
            hstmt:       SQLHSTMT;
            ipar:        SQLUSMALLINT;
            pfSqlType:   SQLSMALLINT_star;
            pcbParamDef: SQLUINTEGER_star;
            pibScale:    SQLSMALLINT_star;
            pfNullable:  SQLSMALLINT_star): SQLRETURN;

<*EXTERNAL SQLExtendedFetch *>
PROCEDURE SQLExtendedFetch (
            hstmt:        SQLHSTMT;
            fFetchType:   SQLUSMALLINT;
            irow:         SQLINTEGER;
            pcrow:        SQLUINTEGER_star;
            rgfRowStatus: SQLUSMALLINT_star): SQLRETURN;

<*EXTERNAL SQLForeignKeys *>
PROCEDURE SQLForeignKeys (
            hstmt:           SQLHSTMT;
            szPkCatalogName: SQLCHAR_star;
            cbPkCatalogName: SQLSMALLINT;
            szPkSchemaName:  SQLCHAR_star;
            cbPkSchemaName:  SQLSMALLINT;
            szPkTableName:   SQLCHAR_star;
            cbPkTableName:   SQLSMALLINT;
            szFkCatalogName: SQLCHAR_star;
            cbFkCatalogName: SQLSMALLINT;
            szFkSchemaName:  SQLCHAR_star;
            cbFkSchemaName:  SQLSMALLINT;
            szFkTableName:   SQLCHAR_star;
            cbFkTableName:   SQLSMALLINT): SQLRETURN;

<*EXTERNAL SQLMoreResults *>
PROCEDURE SQLMoreResults (hstmt: SQLHSTMT): SQLRETURN;

<*EXTERNAL SQLNativeSql *>
PROCEDURE SQLNativeSql (
            hdbc:        SQLHDBC;
            szSqlStrIn:  SQLCHAR_star;
            cbSqlStrIn:  SQLINTEGER;
            szSqlStr:    SQLCHAR_star;
            cbSqlStrMax: SQLINTEGER;
            pcbSqlStr:   SQLINTEGER_star): SQLRETURN;

<*EXTERNAL SQLNumParams *>
PROCEDURE SQLNumParams (
            hstmt: SQLHSTMT;
            pcpar: SQLSMALLINT_star): SQLRETURN;

<*EXTERNAL SQLParamOptions *>
PROCEDURE SQLParamOptions (
            hstmt: SQLHSTMT;
            crow:  SQLUINTEGER;
            pirow: SQLUINTEGER_star): SQLRETURN;

<*EXTERNAL SQLPrimaryKeys *>
PROCEDURE SQLPrimaryKeys (
            hstmt:         SQLHSTMT;
            szCatalogName: SQLCHAR_star;
            cbCatalogName: SQLSMALLINT;
            szSchemaName:  SQLCHAR_star;
            cbSchemaName:  SQLSMALLINT;
            szTableName:   SQLCHAR_star;
            cbTableName:   SQLSMALLINT): SQLRETURN;

<*EXTERNAL SQLProcedureColumns *>
PROCEDURE SQLProcedureColumns (
            hstmt:         SQLHSTMT;
            szCatalogName: SQLCHAR_star;
            cbCatalogName: SQLSMALLINT;
            szSchemaName:  SQLCHAR_star;
            cbSchemaName:  SQLSMALLINT;
            szProcName:    SQLCHAR_star;
            cbProcName:    SQLSMALLINT;
            szColumnName:  SQLCHAR_star;
            cbColumnName:  SQLSMALLINT): SQLRETURN;

<*EXTERNAL SQLProcedures *>
PROCEDURE SQLProcedures (
            hstmt:         SQLHSTMT;
            szCatalogName: SQLCHAR_star;
            cbCatalogName: SQLSMALLINT;
            szSchemaName:  SQLCHAR_star;
            cbSchemaName:  SQLSMALLINT;
            szProcName:    SQLCHAR_star;
            cbProcName:    SQLSMALLINT): SQLRETURN;

<*EXTERNAL SQLSetPos *>
PROCEDURE SQLSetPos (
            hstmt:   SQLHSTMT;
            irow:    SQLUSMALLINT;
            fOption: SQLUSMALLINT;
            fLock:   SQLUSMALLINT): SQLRETURN;

<*EXTERNAL SQLTablePrivileges *>
PROCEDURE SQLTablePrivileges (
            hstmt:         SQLHSTMT;
            szCatalogName: SQLCHAR_star;
            cbCatalogName: SQLSMALLINT;
            szSchemaName:  SQLCHAR_star;
            cbSchemaName:  SQLSMALLINT;
            szTableName:   SQLCHAR_star;
            cbTableName:   SQLSMALLINT): SQLRETURN;

(* SDK 2.0 Additions *)

<*EXTERNAL SQLDrivers *>
PROCEDURE SQLDrivers (
            henv:               SQLHENV;
            fDirection:         SQLUSMALLINT;
            szDriverDesc:       SQLCHAR_star;
            cbDriverDescMax:    SQLSMALLINT;
            pcbDriverDesc:      SQLSMALLINT_star;
            szDriverAttributes: SQLCHAR_star;
            cbDrvrAttrMax:      SQLSMALLINT;
            pcbDrvrAttr:        SQLSMALLINT_star): SQLRETURN;

<*EXTERNAL SQLBindParameter *>
PROCEDURE SQLBindParameter (
            hstmt:      SQLHSTMT;
            ipar:       SQLUSMALLINT;
            fParamType: SQLSMALLINT;
            fCType:     SQLSMALLINT;
            fSqlType:   SQLSMALLINT;
            cbColDef:   SQLUINTEGER;
            ibScale:    SQLSMALLINT;
            rgbValue:   SQLPOINTER;
            cbValueMax: SQLINTEGER;
            pcbValue:   SQLINTEGER_star): SQLRETURN;

END SQLext.

