# include       "sql1.h"
# include       "sqlext1.h"
# include       "error.h"


typedef struct ENV{
		int     type;
		HENV    henv;
		HDBC    hdbc;
		HERR    *herr;
		int     state;
		}GENV;

typedef struct STM{
		int     type;
		struct STM* next;

		HERR    herr;
		HDBC    hdbc;
		HSTMT   dhstmt;

		int     state;
		int     cursor_state;
		int     prep_state;
		int     asyn_on;
		int     need_on;
		struct pg_conn* comsuiv;
		struct pg_result* res;
		UCHAR FAR * container_cursor;
		UCHAR FAR * porte;
		UCHAR FAR * query;
		int out_rows;
		int pcol;
               
		}STMT;


typedef struct handle{
	int     type;           /* must be 1st field */
	struct handle FAR*      
		next;

	HENV    genv;           /* back point to global env object */

	HDBC    dhdbc;          /* driver's private dbc */
	HENV    henv;           /* back point to instant env object */
	HSTMT   hstmt;          /* list of statement object handle(s) */
	HERR    herr;

	int     state;

	/* options */
	UDWORD  access_mode;
	UDWORD  autocommit;

	UDWORD  login_timeout;
	UDWORD  odbc_cursors;
	UDWORD  packet_size;
	UDWORD  quiet_mode;
	UDWORD  txn_isolation;
	SWORD   cb_commit;
	SWORD   cb_rollback;

	char FAR*       
		current_qualifier;

	int     trace;  /* trace flag */
	char FAR*       
		tfile;
	void FAR*
		tstm;   /* trace stream */
	struct pg_conn* consuiv;
        char *Database;
}ODBC;

enum {en_dbc_allocated, end_dbc_need_data, end_dbc_connected, en_dbc_hstmt,
      en_dbc_transaction};




/********************** functions to retrieve portal *************/
/**** in a declaration example of the user during the set of the */
/**** cursor by SQLSetCursorName: e.g. "DECLARE portal CURSOR... */
/**** ... string: "portal" 2nd word in the declaration cursor by */
/* user must be known by SQLFetch...                             */

int Comp_Let_Mot2(char *phrase)
  {
   int nbmot, nblet_mot;
   char *space=" ";
   
   nblet_mot=0;
   nbmot=0;

   while(*phrase)
     {
     if(*phrase == *space)
       {
       nbmot=nbmot+1;
       }
     if((nbmot==2) && (*phrase !=*space))
       {
       nblet_mot=nblet_mot+1;
       }
     phrase++;
     }
  return nblet_mot;
  }

  char *Mot2(char *str1)
  {
   int cmot;
   int nb, i;
   char *d_mot;
   char *space=" ";
   nb=Comp_Let_Mot2(str1);
   d_mot=malloc(nb+1);
   i=0;
   cmot=0;
   while(*str1)
    {
	if(*str1 == *space)
	 {
	  cmot=cmot+1;
	 }
	if(cmot==1)
		{
		 d_mot[i]=*str1;
		i++;
		}
	str1++;
    }
    d_mot[i]='\0';
   return d_mot;
  }



