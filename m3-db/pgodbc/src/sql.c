#include <stdio.h>
#include <string.h>
#include "confg.h"
#include "structure.h"
#include "error.h"
#include "sql1.h"
#include "sqlext1.h"
#include "addenv.h"
#include "libpq-fe.h"

#include <assert.h>


/********** SQLAllocEnv *******************/
RETCODE SQL_API SQLAllocEnv(HENV FAR *phenv)
  {
   GENV FAR *genv;
   genv = (GENV*)MEM_ALLOC(sizeof(GENV));

   if (genv == NULL)
	{
	 *phenv = SQL_NULL_HENV;
	 return SQL_ERROR;
	}
   genv->type=SQL_HANDLE_ENV;  
   genv->hdbc=SQL_NULL_HDBC;
   genv->herr=SQL_NULL_HERR;

   *phenv = (HENV)genv;
   return SQL_SUCCESS;
  }

/********** SQLFreeEnv *******************/
RETCODE SQL_API SQLFreeEnv(HENV henv)
  {
   GENV FAR *genv = (GENV*)henv;
   RETCODE retcode = SQL_SUCCESS;

   if (henv == SQL_NULL_HANDLE)
       {
	retcode=SQL_INVALID_HANDLE;
       }
   if(genv->hdbc != SQL_NULL_HDBC)
      {
       retcode=SQL_ERROR;
      }
   MEM_FREE(henv);
   
   return retcode;
  }

/********** SQLAllocConnect *******************************/
RETCODE SQL_API SQLAllocConnect(HENV henv, HDBC FAR *phdbc)
  {
   GENV FAR *genv = (GENV FAR*)henv;
   ODBC  FAR*   pdbc;
   RETCODE      retcode = SQL_SUCCESS;

   if (henv == SQL_NULL_HENV )
      {
	retcode=SQL_INVALID_HANDLE;
      }
   if( phdbc == NULL )
      {
	retcode=SQL_ERROR;
      }
   pdbc = (ODBC FAR*)MEM_ALLOC (sizeof(ODBC));
   if (pdbc == NULL )
      {
       *phdbc = SQL_NULL_HDBC;
       retcode=SQL_ERROR;
      }
  
  pdbc->next = genv->hdbc;
  genv->hdbc = pdbc;
  pdbc->genv = henv;            
  pdbc->henv = SQL_NULL_HENV;   
  pdbc->hstmt= SQL_NULL_HSTMT;
  pdbc->herr = SQL_NULL_HERR;
  pdbc->dhdbc= SQL_NULL_HDBC;   
  pdbc->state= en_dbc_allocated;
  pdbc->trace = 0;
  pdbc->tstm  = NULL;
  pdbc->tfile = NULL;

  /* set connect options to default values */
  pdbc->access_mode     = SQL_MODE_DEFAULT;
  pdbc->autocommit      = SQL_AUTOCOMMIT_DEFAULT;
  pdbc->current_qualifier = NULL;
  pdbc->login_timeout   = 0UL;
  pdbc->odbc_cursors    = SQL_CUR_DEFAULT;
  pdbc->packet_size     = 0UL;
  pdbc->quiet_mode      = (UDWORD)NULL;
  pdbc->txn_isolation   = SQL_TXN_READ_UNCOMMITTED;
  pdbc->cb_commit       = (SWORD)SQL_CB_DELETE;
  pdbc->cb_rollback     = (SWORD)SQL_CB_DELETE;

  *phdbc = (HDBC)pdbc;

  return retcode;
}

/*****************SQLFreeConnect************************/
RETCODE SQL_API	SQLFreeConnect( HDBC	hdbc )
{
        RETCODE retcode;

	GENV   FAR*	genv;
	ODBC  FAR*	pdbc = (ODBC FAR*)hdbc;
	ODBC  FAR*	npdbc;

	if( hdbc == SQL_NULL_HDBC )
	{
		retcode=SQL_INVALID_HANDLE;
	}


	genv = (GENV FAR*)pdbc->genv;

	for( npdbc  = (ODBC FAR*)genv->hdbc;
	     npdbc != NULL;
	     npdbc  = npdbc->next )
	{
		if( pdbc == npdbc )
		{
			genv->hdbc = pdbc->next;
			break;
		}

		if( pdbc == npdbc->next )
		{
			npdbc->next = pdbc->next;
			break;
		}
	}

	if( pdbc->tfile )
	{
		MEM_FREE( pdbc->tfile );
	}

	MEM_FREE ( pdbc );

	return retcode;
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
 ODBC  FAR*      pdbc    = (ODBC FAR*)hdbc;

 RETCODE  retcode;
 

 char *host, *port, *options, *tty, *dbname;
       
 host = "washington.positron.qc.ca"; port = NULL; options = NULL; tty = NULL;
 dbname = szDSN;

 
  if( szDSN == NULL || cbDSN == 0 )
    {
	retcode=SQL_ERROR;
    }
  if( hdbc == SQL_NULL_HDBC )
       {
	retcode = SQL_INVALID_HANDLE;
       }
  else 
   {
     pdbc->consuiv = PQsetdb(host, port, options, tty, dbname);
     if(PQstatus(pdbc->consuiv) == CONNECTION_OK)
      { 
      retcode = CONNECTION_OK;
      pdbc->dhdbc=hdbc;
      } 
    else 
     {
     retcode = CONNECTION_BAD;
     }
  }
   return retcode;
}

/************ Exit from a database connection ***************/
void DbaseDisconnect(PGconn* dbconn)
  {
   PQfinish(dbconn);
   exit(1);
  }


/*********************************************************/
RETCODE SQL_API	SQLDisconnect ( HDBC hdbc )
{
   ODBC   FAR*	pdbc 	= (ODBC*)hdbc;

   RETCODE retcode; 
   
   retcode=SQL_SUCCESS;

   if( hdbc == SQL_NULL_HDBC ) 
      {
	retcode=SQL_INVALID_HANDLE;
      }

   if(pdbc != NULL)
    {
     MEM_FREE(pdbc);       
     retcode = SQL_SUCCESS;
    } 
         
  return retcode;
}


/*******************   SQLAllocStmt      *********************/
RETCODE SQL_API SQLAllocStmt (HDBC hdbc, HSTMT FAR* phstmt )
{
     
     ODBC FAR*  pdbc    = (ODBC FAR*)hdbc;
     STMT FAR*  pstmt   = NULL;
     RETCODE    retcode = SQL_SUCCESS;

     if( hdbc == SQL_NULL_HDBC )
	retcode=SQL_INVALID_HANDLE;

     if( phstmt == NULL )
       {
	 retcode=SQL_ERROR;
       }
     pstmt = (STMT FAR*)MEM_ALLOC(sizeof(STMT));
     if( pstmt == NULL )
	{
		*phstmt = SQL_NULL_HSTMT;
		retcode=SQL_ERROR;
	}
     if( retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO )
	{
		*phstmt = SQL_NULL_HSTMT;
		MEM_FREE ( pstmt );
		return retcode;
	}

     pstmt->comsuiv = pdbc->consuiv;
     pstmt->next = pdbc->hstmt;
     pdbc->hstmt = pstmt;
     *phstmt = (HSTMT)pstmt;     
    

  return retcode;
}

/********************* SQLFreeStmt **************************/
RETCODE SQL_API SQLFreeStmt (
			HSTMT   hstmt,
			UWORD   fOption )
{
	STMT FAR*       pstmt = (STMT FAR*)hstmt;
	
	ODBC  FAR*      pdbc;

	RETCODE         retcode=SQL_SUCCESS;


	if( hstmt == SQL_NULL_HSTMT || pstmt->hdbc == SQL_NULL_HDBC )
		retcode=SQL_INVALID_HANDLE;

     	pdbc = (ODBC FAR*)(pstmt->hdbc);
      

       if( pstmt != NULL )
	{
		pstmt->comsuiv=SQL_NULL_HSTMT;
		retcode=SQL_SUCCESS;
	}
        MEM_FREE(pstmt);

	return retcode;
}


/******************** SQLSetCursorName **************************/
RETCODE SQL_API SQLSetCursorName (
			HSTMT           hstmt,
			UCHAR FAR*      szCursor,
			SWORD           cbCursor )
{

   STMT FAR*    pstmt   = (STMT* )hstmt;

   RETCODE      retcode = SQL_SUCCESS;

   if( hstmt == SQL_NULL_HSTMT || pstmt->hdbc == SQL_NULL_HDBC )
     {
       retcode=SQL_INVALID_HANDLE;
     }

   if( szCursor == NULL )
     {
       retcode=SQL_ERROR;
     }

    if( cbCursor < 0 && cbCursor != SQL_NTS )
     {
	 retcode=SQL_ERROR;
     }
   
  if(szCursor != NULL)
    { 
     pstmt->container_cursor=szCursor;
    
     pstmt->porte=Mot2(pstmt->container_cursor);
     pstmt->cursor_state = 1;
     retcode=SQL_SUCCESS;
    }
     return retcode;

}

/********************** SQLPrepare *********************/
RETCODE SQL_API SQLPrepare(
			   HSTMT           hstmt,
			   UCHAR FAR*      szSqlStr,
			   SDWORD          cbSqlStr )
{
   UCHAR *portal; 
   

   PGresult* res;   

   SDWORD FAR* pcol;     

   RETCODE retcode = SQL_SUCCESS;

   STMT FAR * pstmt   = (STMT *)hstmt;
   
   pstmt->query=NULL;
   
   if(szSqlStr != NULL)
    {
     portal=malloc(strlen(pstmt->container_cursor)+strlen(szSqlStr));
     strcpy(portal,pstmt->container_cursor);
     strcat(portal, " ");
     strcat(portal, szSqlStr);
     pstmt->query=portal;
     pstmt->prep_state=1;
    }
   else
    {
     fprintf(stderr,"Cursor Name note defined or set\n");
    }

   if(hstmt == SQL_NULL_HSTMT || pstmt->hdbc == SQL_NULL_HDBC)
      retcode=SQL_INVALID_HANDLE;
   if(szSqlStr == NULL )
     {
	retcode=SQL_ERROR;
     }
   if( cbSqlStr < 0  )
     {
	retcode=SQL_ERROR;
     }
  
     pstmt->res=PQexec(pstmt->comsuiv, "BEGIN");
     if(PQresultStatus(pstmt->res) != PGRES_COMMAND_OK)
     {
       retcode = PGRES_BAD_RESPONSE;
       PQclear(pstmt->res);
       DbaseDisconnect(pstmt->comsuiv); 
     }
        
     PQclear(pstmt->res);
   
     pstmt->res=PQexec(pstmt->comsuiv, pstmt->query);
     if(PQresultStatus(pstmt->res) != PGRES_COMMAND_OK)
       {
	retcode = PGRES_BAD_RESPONSE;
	PQclear(pstmt->res);
	DbaseDisconnect(pstmt->comsuiv);
       }
   

 return retcode;
}

/************************* SQLFetch *************************/
RETCODE SQL_API SQLFetch ( HSTMT hstmt )
{
 
   UCHAR *Fetchall, *Fetchfin;

   SDWORD FAR* rows;

   RETCODE retcode = SQL_SUCCESS;
 
   STMT FAR*    pstmt   = (STMT FAR*)hstmt;       

   Fetchall="FETCH ALL in ";

   if( hstmt == SQL_NULL_HSTMT || pstmt->hdbc == SQL_NULL_HDBC )
     {
	retcode=SQL_INVALID_HANDLE;
     }
   
   if(pstmt->prep_state==1)
     {
      Fetchfin=malloc(strlen(Fetchall)+strlen(pstmt->porte)+1);
      strcpy(Fetchfin, Fetchall);
      strcat(Fetchfin, " ");
      strcat(Fetchfin, pstmt->porte);
     }
   else
     {
      fprintf(stderr, "Database communication problem\n");
     }
     
    pstmt->res=PQexec(pstmt->comsuiv,Fetchfin);
    if(PQresultStatus(pstmt->res) != PGRES_TUPLES_OK)
      {
	    retcode = PGRES_BAD_RESPONSE;
	    PQclear(pstmt->res);
	    DbaseDisconnect(pstmt->comsuiv); 
      }    
      
      /** compute results rows **/
      SQLRowCount(hstmt, rows);
   
   return retcode;
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
   STMT FAR* pstmt = (STMT FAR*)hstmt;	
   RETCODE retcode;
   SWORD FAR*  i;
   SWORD tempColMax;

   char strFormat[6], buffer[3];

   SQLNumResultCols(hstmt, i); 
   cbColNameMax=0;
    
     if( hstmt == SQL_NULL_HSTMT || pstmt->hdbc == SQL_NULL_HDBC )
      {
	 retcode=SQL_INVALID_HANDLE;
      }
     for(icol=0; icol <pstmt->pcol; icol++)
       {
        szColName=PQfname(pstmt->res, icol);
        tempColMax=strlen(szColName);
        if(cbColNameMax < tempColMax)
           {
           cbColNameMax=tempColMax;
           }
        strcpy(strFormat, "%-");
        sprintf(buffer, "%d", cbColNameMax+3);
        strcat(strFormat, buffer);
        strcat(strFormat, "s");
	printf(strFormat, szColName);
	/* set other output if needed */
       }
       retcode = SQL_SUCCESS;      
                
   return retcode;
} 
	

/************************* SQLExecute *************************/
RETCODE SQL_API SQLExecute ( HSTMT hstmt )
{
   
   int i, j;

   SWORD FAR* pccol;

   UCHAR *Pclose, *Dclose;

   RETCODE retcode = SQL_SUCCESS;

   STMT FAR*    pstmt   = (STMT FAR*)hstmt;

   Pclose="CLOSE ";

   Dclose=malloc(strlen(Pclose)+strlen(pstmt->porte)+1);
   strcpy(Dclose, Pclose);
   strcat(Dclose, " ");
   strcat(Dclose, pstmt->porte);       

   if( hstmt == SQL_NULL_HSTMT || pstmt->hdbc == SQL_NULL_HDBC )
     {
	retcode=SQL_INVALID_HANDLE;
     }

   /* pstmt->pcol = PQnfields(pstmt->res); */
   
   /** compute results colums **/
   SQLNumResultCols(hstmt, pccol); 

   for(i=0; i< pstmt->pcol; i++)
    {
     printf("%-15s", PQfname(pstmt->res, i));
    }
     printf("\n\n");
       
    for(i=0; i < pstmt->out_rows; i++)
      {
       for(j=0; j< pstmt->pcol; j++)
	 {
	  printf("%-15s", PQgetvalue(pstmt->res, i, j));
	 }
       printf("\n");
      }
   PQclear(pstmt->res); 
   pstmt->res=PQexec(pstmt->comsuiv, Dclose);
   PQclear(pstmt->res);
   
   pstmt->res=PQexec(pstmt->comsuiv,"END");
   PQclear(pstmt->res);
   PQfinish(pstmt->comsuiv);   
   return retcode;
}

/********************** SQLRowCount *******************/
RETCODE SQL_API SQLRowCount( 
			HSTMT           hstmt,
			SDWORD FAR*     pcrow ) 
{
     STMT FAR*  pstmt   = (STMT FAR*)hstmt;
     
	
     RETCODE retcode;

     if( hstmt == SQL_NULL_HSTMT || pstmt->hdbc == SQL_NULL_HDBC )
      {
	retcode=SQL_INVALID_HANDLE;
      }

    if(pstmt->res != NULL)
      {
       pstmt->out_rows = PQntuples(pstmt->res);
       retcode = SQL_SUCCESS;
       pcrow = (SDWORD *)pstmt->out_rows;      
      }          

 return retcode;
}

/***************************SQLNumResultsCols **************/
RETCODE SQL_API SQLNumResultCols(
			HSTMT           hstmt,
			SWORD FAR*      pccol )
{

    STMT FAR*   pstmt   = (STMT FAR*)hstmt;
	
    RETCODE retcode;

    if( hstmt == SQL_NULL_HSTMT || pstmt->hdbc == SQL_NULL_HDBC )
      {
	retcode=SQL_INVALID_HANDLE;
      }

    if(pstmt->res != NULL)
      {
       pstmt->pcol=PQnfields(pstmt->res);
       retcode = SQL_SUCCESS;
       pccol = (SWORD *)pstmt->pcol; 
      }          

  return retcode;
}


/********************** SQLExecDirect *********************/
RETCODE SQL_API SQLExecDirect(
			   HSTMT           hstmt,
			   UCHAR FAR*      szSqlStr,
			   SDWORD          cbSqlStr )
{
   int nFields, nRows, i, j;

   SWORD FAR* pccol;

   SDWORD FAR* rows;

   RETCODE retcode;

   UCHAR *Fetchall, *Fetchfin, *Dclose, *Pclose;

   STMT FAR * pstmt   = (STMT *)hstmt;   

   UCHAR *portal;
   
   Fetchall="FETCH ALL in ";

   retcode = SQL_SUCCESS;

   portal=malloc(strlen(pstmt->container_cursor)+strlen(szSqlStr)+1);
   strcpy(portal, pstmt->container_cursor);
   strcat(portal, " ");
   strcat(portal, szSqlStr);
   pstmt->query=portal;

   Pclose="CLOSE ";

   Dclose=malloc(strlen(Pclose)+strlen(pstmt->porte)+1);
   strcpy(Dclose, Pclose);
   strcat(Dclose, pstmt->porte);       


   if(hstmt == SQL_NULL_HSTMT || pstmt->hdbc == SQL_NULL_HDBC)
      retcode=SQL_INVALID_HANDLE;

   if(szSqlStr == NULL )
     {
	retcode=SQL_ERROR;
     }

   if( cbSqlStr < 0  )
     {
	retcode=SQL_ERROR;
     } 
     pstmt->res=PQexec(pstmt->comsuiv, "BEGIN");
     if(PQresultStatus(pstmt->res) != PGRES_COMMAND_OK)
     {
	retcode = PGRES_BAD_RESPONSE;
	PQclear(pstmt->res);
	DbaseDisconnect(pstmt->comsuiv); 
     }
     PQclear(pstmt->res);
	
     pstmt->res=PQexec(pstmt->comsuiv, portal);
     if(PQresultStatus(pstmt->res) != PGRES_COMMAND_OK)
       {
	 retcode = PGRES_BAD_RESPONSE;
	 PQclear(pstmt->res);
	 DbaseDisconnect(pstmt->comsuiv); 
       }
     PQclear(pstmt->res);
      
     Fetchfin=malloc(strlen(Fetchall)+strlen(pstmt->porte)+1);
     strcpy(Fetchfin, Fetchall);
     strcat(Fetchfin, pstmt->porte);
     
     pstmt->res=PQexec(pstmt->comsuiv, Fetchfin);
     if(PQresultStatus(pstmt->res) != PGRES_TUPLES_OK)
      {
	fprintf(stderr, "FETCH problem - tuples\n"); 
	PQclear(pstmt->res);
	DbaseDisconnect(pstmt->comsuiv); 
      }
      
      /** compute results colums **/
      SQLNumResultCols(hstmt, pccol); 
      nFields = pstmt->pcol;
      
      /** compute results rows **/
      SQLRowCount(hstmt, rows);
      nRows=pstmt->out_rows;

      for(i=0; i< nFields; i++)
	{
	  printf("%-15s",PQfname(pstmt->res,i));
	}
      printf("\n\n");
   
      for(i=0; i< nRows; i++)
	{
	 for(j=0; j < nFields; j++) 
	   {
	    printf("%-15s",PQgetvalue(pstmt->res,i,j));
	   }
	 printf("\n");
	 }

      PQclear(pstmt->res);
      
      pstmt->res=PQexec(pstmt->comsuiv, Dclose);
      PQclear(pstmt->res);
       
      pstmt->res=PQexec(pstmt->comsuiv, "END");
      PQclear(pstmt->res);
      DbaseDisconnect(pstmt->comsuiv);
      PQclear(pstmt->res);
      
 return retcode;
}

/*------------------------------------------ unimplemented SQL calls ----*/

void Unimplemented () 
{
  fprintf (stderr, "SQL procedure not implemented\n");
  assert (0);
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

