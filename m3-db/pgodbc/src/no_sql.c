#include <stdio.h>
#include <string.h>
#include "confg.h"
#include "structure.h"
#include "error.h"
#include "sql1.h"
#include "sqlext1.h"
#include "addenv.h"

#include <assert.h>


/*------------------------------------------ unimplemented SQL calls ----*/

void Unimplemented () 
{
  fprintf (stderr, "SQL procedure not implemented\n");
  assert (0);
}

/********** SQLAllocEnv *******************/
RETCODE SQL_API SQLAllocEnv(HENV FAR *phenv)
  {
    Unimplemented ();
  }

/********** SQLFreeEnv *******************/
RETCODE SQL_API SQLFreeEnv(HENV henv)
  {
    Unimplemented ();
  }

/********** SQLAllocConnect *******************************/
RETCODE SQL_API SQLAllocConnect(HENV henv, HDBC FAR *phdbc)
  {
    Unimplemented ();
  }

/*****************SQLFreeConnect************************/
RETCODE SQL_API	SQLFreeConnect( HDBC	hdbc )
{
    Unimplemented ();
}

/********** SQLConnect *******************************/
RETCODE SQL_API SQLConnect (
			HDBC            hdbc,
			UCHAR FAR*      szDSN,
			SWORD           cbDSN,
			UCHAR FAR*      szUID,
			SWORD           cbUID,
			UCHAR FAR*      szAuthStr,
			SWORD           cbAuthStr)
{
    Unimplemented ();
}


/*********************************************************/
RETCODE SQL_API	SQLDisconnect ( HDBC hdbc )
{
    Unimplemented ();
}


/*******************   SQLAllocStmt      *********************/
RETCODE SQL_API SQLAllocStmt (HDBC hdbc, HSTMT FAR* phstmt )
{
    Unimplemented ();
}

/********************* SQLFreeStmt **************************/
RETCODE SQL_API SQLFreeStmt (
			HSTMT   hstmt,
			UWORD   fOption )
{
    Unimplemented ();
}


/******************** SQLSetCursorName **************************/
RETCODE SQL_API SQLSetCursorName (
			HSTMT           hstmt,
			UCHAR FAR*      szCursor,
			SWORD           cbCursor )
{
    Unimplemented ();
}

/********************** SQLPrepare *********************/
RETCODE SQL_API SQLPrepare(
			   HSTMT           hstmt,
			   UCHAR FAR*      szSqlStr,
			   SDWORD          cbSqlStr )
{
    Unimplemented ();
}

/************************* SQLFetch *************************/
RETCODE SQL_API SQLFetch ( HSTMT hstmt )
{
    Unimplemented ();
}
/***************************SQLDescribeCol************************/

RETCODE SQL_API SQLDescribeCol( 
			HSTMT           hstmt,
			UWORD           icol,
			UCHAR  FAR*     szColName,
			SWORD           cbColNameMax,
			SWORD  FAR*     pcbColName,
			SWORD  FAR*     pfSqlType,
			UDWORD FAR*     pcbColDef,
			SWORD  FAR*     pibScale,
			SWORD  FAR*     pfNullable )
{
    Unimplemented ();
} 
	

/************************* SQLExecute *************************/
RETCODE SQL_API SQLExecute ( HSTMT hstmt )
{
    Unimplemented ();
}

/********************** SQLRowCount *******************/
RETCODE SQL_API SQLRowCount( 
			HSTMT           hstmt,
			SDWORD FAR*     pcrow ) 
{
    Unimplemented ();
}

/***************************SQLNumResultsCols **************/
RETCODE SQL_API SQLNumResultCols(
			HSTMT           hstmt,
			SWORD FAR*      pccol )
{
    Unimplemented ();
}


/********************** SQLExecDirect *********************/
RETCODE SQL_API SQLExecDirect(
			   HSTMT           hstmt,
			   UCHAR FAR*      szSqlStr,
			   SDWORD          cbSqlStr )
{
    Unimplemented ();
}
  
RETCODE SQL_API	SQLDrivers(HENV henv,UWORD x,UCHAR FAR* y,SWORD z,
			   SWORD  FAR* w, UCHAR  FAR* v, 
			   SWORD a, SWORD FAR* b) 
{ 
  Unimplemented();
}

RETCODE SQL_API SQLSetConnectOption (HDBC a, UWORD b, UDWORD c)
{
  Unimplemented();
}

RETCODE SQL_API	SQLDataSources( HENV a,UWORD b,UCHAR  FAR* c,
				SWORD d,SWORD  FAR* e,
				UCHAR  FAR* f,SWORD g, SWORD  FAR* h)
{
  Unimplemented();
}

RETCODE SQL_API	SQLGetCursorName(HSTMT a,UCHAR FAR* b,SWORD c,SWORD FAR* d)
{
  Unimplemented();
}
RETCODE SQL_API	SQLSetPos ( HSTMT a, UWORD b,UWORD c,UWORD d)
{
  Unimplemented();
}
RETCODE SQL_API	SQLError (HENV a,HDBC b,HSTMT c,UCHAR FAR* d,SDWORD FAR* e,UCHAR FAR* f,
			SWORD g,SWORD FAR* h)
{
  Unimplemented();
}

RETCODE	SQL_API	SQLTransact(HENV a,HDBC b,UWORD c)
{
  Unimplemented();
}

RETCODE SQL_API	SQLBindCol ( HSTMT a,UWORD b,SWORD c,PTR d,SDWORD e,SDWORD FAR* g)
{
  Unimplemented();
}

