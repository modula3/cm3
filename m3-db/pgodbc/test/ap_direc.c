#include <stdio.h>
#include <string.h>
#include "confg.h"
#include "error.h"
#include "sql1.h"
#include "sqlext1.h"
#include "libpq-fe.h"
#include "sql.c"

/************************** main for test *****************/
 main(UCHAR *server, UCHAR *uid, UCHAR *pwd)
  {
   
   RETCODE rc;
   HENV  henv;
   HDBC  hdbc;

   HSTMT hstmt;

   UCHAR FAR * hst; int len;
   UCHAR FAR * cursor;

   hst="select city, prcp from weather where date > '01-21-1996'";
   len=strlen(hst);
   cursor="DECLARE portal CURSOR FOR ";
   server ="TestDB"; /* szDSN */
   
   SQLAllocEnv(&henv);
   
   SQLAllocConnect(henv, &hdbc);
   
   SQLConnect(hdbc, server, SQL_NTS, uid, SQL_NTS, pwd, SQL_NTS);
   
   SQLAllocStmt(hdbc, &hstmt);
   
   SQLSetCursorName(hstmt,cursor , strlen(cursor));
    
   SQLExecDirect(hstmt, hst,len);
     
   SQLFreeStmt(hstmt, SQL_DROP); 
   
   SQLDisconnect(hdbc);
     
   SQLFreeConnect(hdbc);

   SQLFreeEnv(henv);
   
  return (0);
     
 }
