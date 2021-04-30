
//No Need to import MySQLRaw into safe interface
//%typemap("m3wrapintype:import")  MYSQL_RES *res %{MySQLRaw%}

%typemap("m3wrapargraw")  char   %{ORD($1_name)%}
//the enum one
%typemap("m3wrapintype")  enum enum_stmt_attr_type %{INTEGER%}

//change Cardinal32.T to CARDINAL - fix the modula3.swg
//could get range error in the conversion on 32 bit machines
%typemap(m3wraptype) unsigned long, const unsigned long &  %{INTEGER%}


// ----------------------------- size_t ------------------------------------

%typemap(m3rawintype)     size_t* %{INTEGER%}
%typemap(m3wrapintype)    size_t* %{INTEGER%}


// ----------------------------- void * ------------------------------------

%typemap(m3rawtype)  void  *        %{ADDRESS%}
%typemap(m3wraptype) void  *        %{ADDRESS%}


// ----------------------------- const void * ------------------------------

%typemap("m3wrapintype")  const void * %{ADDRESS%}
%typemap("m3wrapintype")        void * %{ADDRESS%}
%typemap("m3wrapinmode")  const void * %{%}
%typemap("m3wrapinmode")        void * %{%}

//the string typemaps

// ----------------------------- const char * ------------------------------

%typemap(m3wrapinmode)  const char *   %{%}
%typemap(m3rawintype)   const char *   %{C.const_char_star%}
%typemap(m3wrapintype)  const char *   %{TEXT%}
%typemap(m3wrapargvar)  const char *   %{$1: C.const_char_star;%}
%typemap(m3wrapinconv)  const char *   %{$1 := NewString($1_name);%}
%typemap(m3wrapargraw)  const char *   %{$1%}
%typemap(m3wrapfreearg) const char *   %{FreeString($1_name,$1);%}


// ----------------------------- char * ------------------------------------

%typemap(m3wrapinmode)  char *         %{%}
%typemap(m3rawintype)   char *         %{C.char_star%}
%typemap(m3wrapintype)  char *         %{TEXT%}
%typemap(m3wrapargvar)  char *         %{$1: C.char_star;%}
%typemap(m3wrapinconv)  char *         %{$1 := NewString($1_name);%}
%typemap(m3wrapargraw)  char *         %{$1%}
%typemap(m3wrapfreearg) char *         %{FreeString($1_name,$1);%}


// ----------------------------- char ** -----------------------------------

%typemap(m3rawinmode)   char **        %{READONLY%}
%typemap(m3wrapinmode)  char **        %{READONLY%}
%typemap(m3rawintype)   char **        %{(*ARRAY OF*) C.char_star%}
%typemap(m3wrapintype)  char **        %{ARRAY OF TEXT%}
%typemap(m3wrapargvar)  char **        %{$1: C.char_star;%}
%typemap(m3wrapinconv)  char **        %{$1 := NewString($1_name[0]);%}
%typemap(m3wrapargraw)  char **        %{$1%}
%typemap(m3wrapfreearg) char **        %{FreeString($1_name[0],$1);%}


// ----------------------------- unsigned char ** --------------------------

%apply char ** {unsigned char **}


// ----------------------------- int ---------------------------------------

%typemap("m3rawintype")     int  %{C.int%}
%typemap("m3wrapinmode")    int  %{%}
%typemap("m3wrapintype")    int  %{Int32%}
%typemap("m3wraprettype")   int  %{Int32%}

//If we need exceptions for selected functions which return int on error
//we could expand this list. But for the most part we rely on the user
//to deal with the error since the definition could change and some
//functions return int which is not an error.

//%typemap("m3wrapretcheck")  int mysql_select_db %{IF result # 0 THEN RAISE ReturnE(result); END;%};
//%typemap("m3wrapretcheck:throws")  int mysql_select_db %{ReturnE%}


// ----------------------------- unsigned int -------------------------------
//these should be Word types

%typemap("m3rawintype")     unsigned int  %{C.unsigned_int%}
%typemap("m3wrapinmode")    unsigned int  %{%}
%typemap("m3wrapintype")    unsigned int  %{Int32%}
%typemap("m3wraprettype")   unsigned int  %{Int32%}


// ----------------------------- int * -------------------------------------

%typemap(m3wrapinmode)  int *         %{VAR%}
%typemap(m3rawintype)   int *         %{C.int%}
%typemap(m3wrapintype)  int *         %{Int32%}


// ----------------------------- unsigned long ------------------------------
//these should be Word types
/*
%typemap("m3rawintype")     unsigned long  %{C.unsigned_long%}
%typemap("m3wrapinmode")    unsigned long  %{%}
%typemap("m3wrapintype")    unsigned long  %{LONGINT%}
%typemap("m3wraprettype")   unsigned long  %{LONGINT%}
*/

// ----------------------------- unsigned long * ---------------------------

%typemap("m3rawrettype")   unsigned long *  %{C.unsigned_long_star%}
%typemap("m3wraprettype")  unsigned long *  %{RefLengthsT%}
%typemap("m3wrapretvar")   unsigned long *  %{ret : C.unsigned_long_star;
result : RefLengthsT;%};
%typemap("m3wrapretraw")   unsigned long *  %{ret%};
%typemap("m3wrapretcheck") unsigned long *  %{result := LOOPHOLE(ret,RefLengthsT);%};
%typemap("m3wrapretconv")  unsigned long *  %{result%};


// ----------------------------- unsigned long long ------------------------

%typemap("m3rawrettype")   unsigned long long  %{C.unsigned_long_long%}
%typemap("m3wraprettype")  unsigned long long  %{LONGINT%}
%typemap("m3wrapintype")   unsigned long long  %{LONGINT%}
%typemap("m3wrapretvar")   unsigned long long  %{ret : C.unsigned_long_long;
result : LONGINT;%};
%typemap("m3wrapretraw")   unsigned long long  %{ret%};
%typemap("m3wrapretcheck") unsigned long long  %{result := LOOPHOLE(ret,LONGINT);%};
%typemap("m3wrapretconv")  unsigned long long  %{result%};


// ----------------------------- my_ulonglong ------------------------------

%apply my_ulonglong {unsigned long long}


// ----------------------------- my_bool -----------------------------------

%typemap("m3rawintype")    my_bool     %{my_bool%}
%typemap("m3rawrettype")   my_bool     %{my_bool%}
%typemap("m3wraprettype")  my_bool     %{BOOLEAN%}
%typemap("m3wrapretvar")   my_bool     %{ret : MySQLRaw.my_bool;
result : BOOLEAN;%};
%typemap("m3wrapretraw")   my_bool     %{ret%};
%typemap("m3wrapretcheck") my_bool     %{result := VAL(ret,BOOLEAN);%};
%typemap("m3wrapretconv")  my_bool     %{result%};
%typemap("m3wrapintype")   my_bool     %{BOOLEAN%}
%typemap("m3wrapargraw")   my_bool     %{$1%}
%typemap("m3wrapargvar")   my_bool     %{$1 := LOOPHOLE($1_name,MySQLRaw.my_bool);%}


// ----------------------------- MYSQL_FIELD_OFFSET --------------------------

%typemap("m3rawrettype")  MYSQL_FIELD_OFFSET %{C.unsigned_int%}
%typemap("m3rawintype")   MYSQL_FIELD_OFFSET %{C.unsigned_int%}
%typemap("m3wraprettype") MYSQL_FIELD_OFFSET %{CARDINAL%}
%typemap("m3wrapintype")  MYSQL_FIELD_OFFSET %{CARDINAL%}


// ----------------------------- MYSQL_ROW -----------------------------------
//MYSQL_ROW typemaps need to return an array of texts

%typemap("m3rawrettype")  MYSQL_ROW    %{C.char_star_star%}
%typemap("m3wraprettype") MYSQL_ROW    %{REF ARRAY OF TEXT%}
%typemap("m3wrapretconv") MYSQL_ROW    %{row%};
%typemap("m3wrapretvar")  MYSQL_ROW    %{ret : C.char_star_star;
result : RefRow;
numFields : INTEGER;
row : REF ARRAY OF TEXT;%}
%typemap("m3wrapretraw")   MYSQL_ROW   %{ret%};
%typemap("m3wrapretcheck") MYSQL_ROW   %{IF ret = NIL THEN
  row := NIL;
ELSE
  result := LOOPHOLE(ret,RefRow);
  numFields := NumFields(mysql_res);
  row := NEW(REF ARRAY OF TEXT,numFields);
  FOR i := 0 TO numFields -1 DO
    (* DB NULL results in NIL M3 text *)
    IF result[i] # NIL THEN row[i] := M3toC.CopyStoT(result[i]) END;
  END;
END;%};


// ----------------------------- MYSQL_ROW * ---------------------------------

%typemap("m3rawrettype")  MYSQL_ROW *  %{C.char_star_star%}
%typemap("m3rawintype")   MYSQL_ROW *  %{C.char_star_star%}
%typemap("m3rawinmode")   MYSQL_ROW *  %{%}
%typemap("m3wraprettype") MYSQL_ROW *  %{REF ARRAY OF TEXT%}
%typemap("m3wrapretraw")  MYSQL_ROW *  %{ret%};
%typemap("m3wrapinmode")  MYSQL_ROW *  %{VAR%}
%typemap("m3wrapintype")  MYSQL_ROW *  %{REF ARRAY OF TEXT%}
%typemap("m3wrapargraw")  MYSQL_ROW *  %{$1%}
%typemap("m3wrapargvar")  MYSQL_ROW *  %{numFields : INTEGER;
res : RefRow;
$1 := LOOPHOLE($1_name,C.char_star_star);%}
%typemap("m3wrapoutcheck") MYSQL_ROW *  %{
IF $1 = NIL THEN
  ret := NIL;
ELSE
  res := LOOPHOLE($1,RefRow);
  numFields := NumFields(mysql_res);
  ret := NEW(REF ARRAY OF TEXT,numFields);
  FOR i := 0 TO numFields -1 DO
    (* DB NULL results in NIL M3 text *)
    IF res[i] # NIL THEN ret[i] := M3toC.CopyStoT(res[i]) END;
  END;
END;
%}


// ----------------------------- MYSQL * -------------------------------------

%typemap("m3rawintype")    MYSQL *       %{RefMysqlT%}
%typemap("m3rawinmode")    MYSQL *       %{%}
%typemap("m3wrapintype")   MYSQL *       %{T%}
%typemap("m3wrapinmode")   MYSQL *       %{%}
%typemap("m3rawrettype")   MYSQL *       %{RefMysqlT%}
%typemap("m3wraprettype")  MYSQL *       %{T%}
//MySQLRaw.RefMysqlT was ADDRESS which worked as well
%typemap("m3wrapargraw")   MYSQL *       %{rawT%}
%typemap("m3wrapargvar")   MYSQL *       %{rawT: MySQLRaw.RefMysqlT := LOOPHOLE($1_name,MySQLRaw.RefMysqlT);%}
%typemap("m3wrapretvar")   MYSQL *       %{ret : MySQLRaw.RefMysqlT;
result : T;%};
%typemap("m3wrapretraw")   MYSQL *       %{ret%};
%typemap("m3wrapretconv")  MYSQL *       %{result%};
%typemap("m3wrapretcheck") MYSQL *       %{result := LOOPHOLE(ret,T);%}

//This would raise an exception but lets make the user handle it 
/*
%typemap("m3wrapretcheck") MYSQL *       %{result := LOOPHOLE(ret,T);
IF result = NIL THEN
  RAISE ConnE;
END;%};
//This adds the RAISES clause
%typemap("m3wrapretcheck:throws")  MYSQL * %{ConnE%}
*/

// ----------------------------- MYSQL ** ------------------------------------

%typemap("m3rawintype")    MYSQL **  %{RefMysqlT%}
%typemap("m3rawinmode")    MYSQL **  %{VAR%}
%typemap("m3wrapintype")   MYSQL **  %{T%}
%typemap("m3wrapinmode")   MYSQL **  %{VAR%}

%typemap("m3rawrettype")   MYSQL **  %{RefMysqlT%}
%typemap("m3wraprettype")  MYSQL **  %{T%}
%typemap("m3wrapargraw")   MYSQL **  %{raw1T%}
%typemap("m3wrapargvar")   MYSQL **  %{raw1T: MySQLRaw.RefMysqlT := LOOPHOLE($1_name,MySQLRaw.RefMysqlT);%}
%typemap("m3wrapretvar")   MYSQL **  %{ret1 : MySQLRaw.RefMysqlT;
result : T;%};
%typemap("m3wrapretraw")   MYSQL **  %{ret1%};
%typemap("m3wrapretconv")  MYSQL **  %{result1%};
%typemap("m3wrapretcheck") MYSQL **  %{result := LOOPHOLE(ret,T);%}

//See above re exception policy
/*
%typemap("m3wrapretcheck") MYSQL **  %{result := LOOPHOLE(ret1,T);
IF result = NIL THEN
  RAISE ConnE;
END;%};
//This adds the RAISES clause
%typemap("m3wrapretcheck:throws")  MYSQL ** %{ConnE%}
*/

// ----------------------------- MYSQL_RES * ---------------------------------

%typemap("m3rawintype")     MYSQL_RES *  %{RefMysqlResultT%}
%typemap("m3rawinmode")     MYSQL_RES *  %{%}
%typemap("m3rawrettype")    MYSQL_RES *  %{RefMysqlResultT%}
%typemap("m3wraprettype")   MYSQL_RES *  %{ResultT%}
%typemap("m3wrapretvar")    MYSQL_RES *  %{ret : MySQLRaw.RefMysqlResultT;
result : ResultT;%};
%typemap("m3wrapretraw")    MYSQL_RES *  %{ret%};
%typemap("m3wrapretconv")   MYSQL_RES *  %{result%};
%typemap("m3wrapintype")    MYSQL_RES *  %{ResultT%}
%typemap("m3wrapinmode")    MYSQL_RES *  %{%}
%typemap("m3wrapargraw")    MYSQL_RES *  %{$1%}
%typemap("m3wrapargvar")    MYSQL_RES *  %{$1: MySQLRaw.RefMysqlResultT := LOOPHOLE($1_name,MySQLRaw.RefMysqlResultT);%}
%typemap("m3wrapretcheck")  MYSQL_RES *  %{result := LOOPHOLE(ret,ResultT);%};

//dont raise an exception since functions like store_result return
//useful results even if nil and the others are like list_dbs which are
//rarely used. 
//%typemap("m3wrapretcheck")  MYSQL_RES *  %{result := LOOPHOLE(ret,ResultT);
//IF result = NIL THEN RAISE ResultE; END;%};
//adds the RAISES clause
//%typemap("m3wrapretcheck:throws")  MYSQL_RES * %{ResultE%}


// ----------------------------- MYSQL_RES ** --------------------------------

%typemap("m3rawintype")     MYSQL_RES **  %{RefMysqlResultT%}
%typemap("m3rawinmode")     MYSQL_RES **  %{VAR%}
%typemap("m3wrapinmode")    MYSQL_RES **  %{VAR%}
%typemap("m3wrapretvar")    MYSQL_RES **  %{ret : MySQLRaw.RefMysqlResultT;
result : ResultT;%};
%typemap("m3wrapretraw")    MYSQL_RES **  %{ret%};
%typemap("m3wrapretconv")   MYSQL_RES **  %{result%};
%typemap("m3wrapintype")    MYSQL_RES **  %{ResultT%}
%typemap("m3wrapinmode")    MYSQL_RES **  %{%}
%typemap("m3wrapargraw")    MYSQL_RES **  %{$1%}
%typemap("m3wrapargvar")    MYSQL_RES **  %{$1: MySQLRaw.RefMysqlResultT := LOOPHOLE($1_name,MySQLRaw.RefMysqlResultT);%}


// ----------------------------- MYSQL_ROW_OFFSET ----------------------------

%typemap("m3rawrettype")    MYSQL_ROW_OFFSET   %{RowOffsetT%}
%typemap("m3wraprettype")   MYSQL_ROW_OFFSET   %{RowOffsetT%}
%typemap("m3rawintype")     MYSQL_ROW_OFFSET   %{RowOffsetT%}
//cope with anonymous input arg name
%typemap("m3wrapinname")    MYSQL_ROW_OFFSET   %{$1_in%}
%typemap("m3wrapintype")    MYSQL_ROW_OFFSET   %{RowOffsetT%}%typemap("m3wrapargvar")    MYSQL_ROW_OFFSET   %{$1: MySQLRaw.RowOffsetT := LOOPHOLE($1_name_in,MySQLRaw.RowOffsetT);%}
%typemap("m3wrapargraw")    MYSQL_ROW_OFFSET   %{$1%}
%typemap("m3wrapretvar")    MYSQL_ROW_OFFSET   %{ret : MySQLRaw.RowOffsetT;
result : RowOffsetT;%};
%typemap("m3wrapretraw")    MYSQL_ROW_OFFSET   %{ret%};
%typemap("m3wrapretconv")   MYSQL_ROW_OFFSET   %{result%};
%typemap("m3wrapretcheck")  MYSQL_ROW_OFFSET   %{result := LOOPHOLE(ret,RowOffsetT);%};

%apply MYSQL_ROW_OFFSET {MYSQL_ROWS *}

//dont ignore fields we need them - they are fully wrapped
//%ignore st_mysql_field;
//%ignore st_mysql_field::type;
//%ignore st_mysql_field::extension;

//For the struct constructor (New_MYSQL_FIELD etc) which we dont export
%typemap("m3rawintype")   struct st_mysql_field  * %{C.int%}
%typemap("m3rawrettype")  struct st_mysql_field  * %{C.int_star%}
%typemap("m3wraprettype") struct st_mysql_field  * %{C.int_star%}
%typemap("m3wrapinmode")  struct st_mysql_field  * %{VALUE%}
%typemap("m3wrapintype")  struct st_mysql_field  * %{C.int%}


// ----------------------------- MYSQL_FIELD -------------------------------

%typemap("m3rawrettype")   MYSQL_FIELD *  %{RefMysqlFieldT%}
%typemap("m3wraprettype")  MYSQL_FIELD *  %{FieldT%}
%typemap("m3wrapretvar")   MYSQL_FIELD *  %{ret : MySQLRaw.RefMysqlFieldT;
result : FieldT;%}
%typemap("m3wrapretraw")   MYSQL_FIELD *  %{ret%}
%typemap("m3wrapretconv")  MYSQL_FIELD *  %{result%}
%typemap("m3wrapretcheck") MYSQL_FIELD *  %{result := NewField(ret^);%}

//mysql_fetch_fields returns an array
%typemap("m3wraprettype")  MYSQL_FIELD * mysql_fetch_fields %{RefFieldArray%}
%typemap("m3wrapretvar")   MYSQL_FIELD * mysql_fetch_fields %{ret : MySQLRaw.RefMysqlFieldT;
result : RefFieldArray;
len : CARDINAL;%}
%typemap("m3wrapretcheck") MYSQL_FIELD * mysql_fetch_fields %{len := MySQLRaw.NumFields(arg1);
result := GetFieldList(ret,len);%}


// ----------------------------- MYSQL_MANAGER -------------------------------

%typemap("m3rawintype")    MYSQL_MANAGER *     %{RefMysqlManagerT%}
%typemap("m3rawinmode")    MYSQL_MANAGER *     %{%}
%typemap("m3wrapintype")   MYSQL_MANAGER *con  %{ManagerT%}
%typemap("m3wrapinmode")   MYSQL_MANAGER *     %{%}
%typemap("m3rawrettype")   MYSQL_MANAGER *     %{RefMysqlManagerT%}
%typemap("m3wraprettype")  MYSQL_MANAGER *     %{ManagerT%}
%typemap("m3wrapargraw")   MYSQL_MANAGER *con  %{$1%}
%typemap("m3wrapargvar")   MYSQL_MANAGER *con  %{$1: MySQLRaw.RefMysqlManagerT := LOOPHOLE($1_name,MySQLRaw.RefMysqlManagerT);%}
%typemap("m3wrapretvar")   MYSQL_MANAGER *     %{ret : MySQLRaw.RefMysqlManagerT;
result : ManagerT;
%};
%typemap("m3wrapretraw")   MYSQL_MANAGER *     %{ret%};
%typemap("m3wrapretconv")  MYSQL_MANAGER *     %{result%};
%typemap("m3wrapretcheck") MYSQL_MANAGER *     %{result := LOOPHOLE(ret,ManagerT);
IF result = NIL THEN
  RAISE ConnE;
END;%};
//add the raises clause
%typemap("m3wrapretcheck:throws")  MYSQL_MANAGER * %{ConnE%}


// ----------------------------- MYSQL_PARAMETERS ----------------------------

%typemap("m3rawrettype")   MYSQL_PARAMETERS *  %{RefMysqlParametersT%}
%typemap("m3wraprettype")  MYSQL_PARAMETERS *  %{ParametersT%}
%typemap("m3wrapretvar")   MYSQL_PARAMETERS *  %{ret : MySQLRaw.RefMysqlParametersT;
result : ParametersT;%};
%typemap("m3wrapretraw")   MYSQL_PARAMETERS *  %{ret%};
%typemap("m3wrapretconv")  MYSQL_PARAMETERS *  %{result%};
%typemap("m3wrapretcheck") MYSQL_PARAMETERS *  %{result := LOOPHOLE(ret,ParametersT);%};


// ----------------------------- MYSQL_STMT ----------------------------------

%typemap("m3rawintype")     MYSQL_STMT *  %{RefMysqlStmtT%}
%typemap("m3rawinmode")     MYSQL_STMT *  %{%}
%typemap("m3wrapinmode")    MYSQL_STMT *  %{%}
%typemap("m3wrapintype")    MYSQL_STMT *  %{StmtT%}
%typemap("m3rawrettype")    MYSQL_STMT *  %{RefMysqlStmtT%}
%typemap("m3wraprettype")   MYSQL_STMT *  %{StmtT%}
%typemap("m3wrapargraw")    MYSQL_STMT *  %{$1%}
%typemap("m3wrapargvar")    MYSQL_STMT *  %{$1: MySQLRaw.RefMysqlStmtT := LOOPHOLE($1_name,MySQLRaw.RefMysqlStmtT);%}

%typemap("m3wrapretvar")    MYSQL_STMT *  %{ret : MySQLRaw.RefMysqlStmtT;
result : StmtT;%};
%typemap("m3wrapretraw")    MYSQL_STMT *  %{ret%};
%typemap("m3wrapretconv")   MYSQL_STMT *  %{result%};
//never actioned
//%typemap("m3wrapretcheck")  MYSQL_STMT *  %{result := LOOPHOLE(ret,StmtT);
//IF result = NIL THEN RAISE ResultE; END;%};
//add the RAISES clause
//%typemap("m3wrapretcheck:throws")  MYSQL_STMT * %{ResultE%}


// ----------------------------- MYSQL_BIND ----------------------------------
//Not used anymore
%typemap("m3rawintype")     MYSQL_BIND *  %{RefMysqlBindT%}
%typemap("m3rawinmode")     MYSQL_BIND *  %{%}
%typemap("m3wrapinmode")    MYSQL_BIND *  %{%}
%typemap("m3wrapintype")    MYSQL_BIND *  %{BindT%}
%typemap("m3wrapargraw")    MYSQL_BIND *  %{$1%}
%typemap("m3wrapargvar")    MYSQL_BIND *  %{$1: MySQLRaw.RefMysqlBindT := LOOPHOLE($1_name,MySQLRaw.RefMysqlBindT);%}


// ----------------------------- MY_CHARSET_INFO * ----------------------------

%typemap("m3rawintype")     MY_CHARSET_INFO *  %{RefMysqlCharsT%}
%typemap("m3wrapintype")    MY_CHARSET_INFO *  %{CharsT%}
%typemap("m3rawinmode")     MY_CHARSET_INFO *  %{%}
%typemap("m3wrapargraw")    MY_CHARSET_INFO *  %{$1%}
%typemap("m3wrapargvar")    MY_CHARSET_INFO *  %{$1: MySQLRaw.RefMysqlCharsT := LOOPHOLE($1_name,MySQLRaw.RefMysqlCharsT);%}


// ----------------------------- local_infile --------------------------------

%typemap(m3wrapinmode)  int (*) (void **, const char *, void *) %{%}
%typemap(m3rawinmode)   int (*) (void **, const char *, void *) %{%}
%typemap(m3wrapinmode)  int (*) (void *, char *, unsigned int) %{%}
%typemap(m3rawinmode)   int (*) (void *, char *, unsigned int) %{%}
%typemap(m3wrapinmode)  void (*) (void *) %{%}
%typemap(m3rawinmode)   void (*) (void *) %{%}

%typemap(m3rawintype)   int (*local_infile_init) (void **, const char *, void *) %{InitRawCBT%}
%typemap(m3wrapintype)  int (*local_infile_init) (void **, const char *, void *) %{InitCBT%}
%typemap(m3wrapargraw)  int (*local_infile_init) (void **, const char *, void *) %{<*NOWARN*>P0%}

//if uncomment must rename the RETURN func below in the nested proc
//%typemap(m3wrapinname)  int (*) (void **, const char *, void *) %{local_callback_0%}
%typemap(m3wrapargvar)  int (*local_infile_init) (void **, const char *, void *) %{
PROCEDURE P0(p1 : REF C.void_star; p2 : C.char_star; p3 : C.void_star) : C.int =
VAR
  r1 : REF ADDRESS := p1;
  r2 := M3toC.CopyStoT(p2);
  r3 := p3;
BEGIN
  RETURN local_infile_init(r1,r2,r3);
END P0;%}

%typemap(m3rawintype)   int (*local_infile_read) (void *, char *, unsigned int) %{ReadRawCBT%}
%typemap(m3wrapintype)  int (*local_infile_read) (void *, char *, unsigned int) %{ReadCBT%}
%typemap(m3wrapargraw)  int (*local_infile_read) (void *, char *, unsigned int) %{<*NOWARN*>P1%}
%typemap(m3wrapargvar)  int (*local_infile_read) (void *, char *, unsigned int) %{
PROCEDURE P1(p1 : C.void_star; p2 : C.char_star; p3 : C.unsigned_int) : C.int =
VAR
  r1 : ADDRESS := p1;
  r2 := M3toC.CopyStoT(p2);
  r3 := p3;
BEGIN
  RETURN local_infile_read(r1,r2,r3);
END P1;%}

%typemap(m3rawintype)   int (*local_infile_error) (void *, char *, unsigned int) %{ErrorRawCBT%}
%typemap(m3wrapintype)  int (*local_infile_error) (void *, char *, unsigned int) %{ErrorCBT%}
%typemap(m3wrapargraw)  int (*local_infile_error) (void *, char *, unsigned int) %{<*NOWARN*>P2%}
%typemap(m3wrapargvar)  int (*local_infile_error) (void *, char *, unsigned int) %{
PROCEDURE P2(p1 : C.void_star; p2 : C.char_star; p3 : C.unsigned_int) : C.int =
VAR
  r1 : ADDRESS := p1;
  r2 := M3toC.CopyStoT(p2);
  r3 := p3;
BEGIN
  RETURN local_infile_error(r1,r2,r3);
END P2;
VAR
%}

%typemap(m3rawintype)    void (*) (void *) %{EndRawCBT%}
%typemap(m3wrapintype)   void (*) (void *) %{EndCBT%}


%typemap("m3wrapintype")  enum mysql_enum_shutdown_level %{INTEGER%}
%typemap("m3wrapintype")  enum enum_mysql_set_option %{INTEGER%}
%typemap("m3wrapintype")  enum mysql_option %{INTEGER%}
%typemap("m3wrapintype")  enum enum_session_state_type %{INTEGER%}
%typemap("m3wraptype")    enum enum_field_types %{INTEGER%}

//code to insert into the raw interface

%insert(m3rawintf) %{
TYPE
  my_ulonglong = C.unsigned_long_long;
  my_bool = C.char;

  OPAQUE = RECORD END;

  RefMysqlT = REF OPAQUE;
  RefMysqlStmtT = REF OPAQUE;
  RefMysqlResultT = REF OPAQUE;
  RefMysqlBindT = REF OPAQUE;
  RefMysqlCharsT = REF OPAQUE;
  RowOffsetT = REF OPAQUE;
  RefMysqlParametersT = REF OPAQUE;
  RefMysqlFieldT = UNTRACED REF MYSQL_FIELD;

  InitRawCBT = PROCEDURE(p1 : REF C.void_star; p2 : C.char_star; p3 : C.void_star) : C.int;
  ReadRawCBT = PROCEDURE(p1 : C.void_star; p2 : C.char_star; p3 : C.unsigned_int) : C.int;
  ErrorRawCBT = PROCEDURE(p1 : C.void_star; p2 : C.char_star; p3 : C.unsigned_int) : C.int;
  EndRawCBT = PROCEDURE(p1 : C.void_star);
  ExtendRawCBT = PROCEDURE(p1 : C.void_star; p2 : C.char_star; p3 : REF C.unsigned_long) : C.char_star;
%}

//code to insert into the safe interface

%insert(m3wrapintf) %{
TYPE
  T <: ADDRESS;
  ResultT <: ADDRESS;
  StmtT <: ADDRESS;
  RowOffsetT <: ADDRESS;
  ManagerT <: ADDRESS;
  ParametersT <: ADDRESS;
  BindT <: ADDRESS;
  CharsT <: ADDRESS;
  FieldT = REF MYSQL_FIELD;
  RefFieldArray = REF ARRAY OF FieldT;

TYPE
  Int32 = [-16_7FFFFFFF-1..16_7FFFFFFF];

CONST
  MAX_COLUMNS = 1000; (* Arbitrary limit to how many cols returned in a query *)

TYPE
  RefLengthsT =  UNTRACED REF ARRAY [0..MAX_COLUMNS] OF INTEGER;

TYPE
  InitCBT  = PROCEDURE(p1 : REF ADDRESS; p2 : TEXT; p3 : ADDRESS) : INTEGER;
  ReadCBT  = PROCEDURE(p1 : ADDRESS; p2 : TEXT; p3 : CARDINAL) : INTEGER;
  ErrorCBT = PROCEDURE(p1 : ADDRESS; p2 : TEXT; p3 : CARDINAL) : INTEGER;
  EndCBT   = PROCEDURE(p1 : ADDRESS);

CONST (* Enum FieldTypes *)
  MYSQL_TYPE_DECIMAL = 0;
  MYSQL_TYPE_TINY = 1;
  MYSQL_TYPE_SHORT = 2;
  MYSQL_TYPE_LONG = 3;
  MYSQL_TYPE_FLOAT = 4;
  MYSQL_TYPE_DOUBLE = 5;
  MYSQL_TYPE_NULL = 6;
  MYSQL_TYPE_TIMESTAMP = 7;
  MYSQL_TYPE_LONGLONG = 8;
  MYSQL_TYPE_INT24 = 9;
  MYSQL_TYPE_DATE = 10;
  MYSQL_TYPE_TIME = 11;
  MYSQL_TYPE_DATETIME = 12;
  MYSQL_TYPE_YEAR = 13;
  MYSQL_TYPE_NEWDATE = 14;
  MYSQL_TYPE_VARCHAR = 15;
  MYSQL_TYPE_BIT = 16;
  MYSQL_TYPE_TIMESTAMP2 = 17;
  MYSQL_TYPE_DATETIME2 = 18;
  MYSQL_TYPE_TIME2 = 19;
  MYSQL_TYPE_JSON = 245;
  MYSQL_TYPE_NEWDECIMAL = 246;
  MYSQL_TYPE_ENUM = 247;
  MYSQL_TYPE_SET = 248;
  MYSQL_TYPE_TINY_BLOB = 249;
  MYSQL_TYPE_MEDIUM_BLOB = 250;
  MYSQL_TYPE_LONG_BLOB = 251;
  MYSQL_TYPE_BLOB = 252;
  MYSQL_TYPE_VAR_STRING = 253;
  MYSQL_TYPE_STRING = 254;
  MYSQL_TYPE_GEOMETRY = 255;
  MAX_NO_FIELD_TYPES = 256;

TYPE (* Enum FieldTypes *)
  FieldTypes = [0..256];
%}

%insert(m3wrapimpl) %{
TYPE
  RefRow = UNTRACED REF ARRAY [0..MAX_COLUMNS] OF C.char_star;

REVEAL
  T = UNTRACED BRANDED REF MySQLRaw.OPAQUE;
  ResultT = UNTRACED BRANDED REF MySQLRaw.OPAQUE;
  StmtT = UNTRACED BRANDED REF MySQLRaw.OPAQUE;
  RowOffsetT = UNTRACED BRANDED REF MySQLRaw.OPAQUE;
  ManagerT = UNTRACED BRANDED REF MySQLRaw.OPAQUE;
  ParametersT = UNTRACED BRANDED REF MySQLRaw.OPAQUE;
  BindT = UNTRACED BRANDED REF MySQLRaw.OPAQUE;
  CharsT = UNTRACED BRANDED REF MySQLRaw.OPAQUE;

VAR Null := LOOPHOLE(0,ADDRESS);

PROCEDURE NewString(t : TEXT) : C.char_star =
  VAR res : C.char_star;
  BEGIN
    IF t = NIL THEN res := Null; ELSE res := M3toC.CopyTtoS(t); END;
    RETURN res;
  END NewString;

PROCEDURE FreeString(t : TEXT; c : C.char_star) =
  BEGIN
    IF t # NIL THEN M3toC.FreeCopiedS(c); END;
  END FreeString;

PROCEDURE ToText (s: C.char_star): TEXT =
  BEGIN
    IF s # NIL THEN RETURN M3toC.CopyStoT(s); ELSE RETURN NIL; END;
  END ToText;

PROCEDURE NewField (READONLY fieldRef: MySQLRaw.MYSQL_FIELD): FieldT =
  VAR ret := NEW(FieldT);
  BEGIN
    ret.name := ToText(fieldRef.name);
    ret.org_name := ToText(fieldRef.org_name);
    ret.table := ToText(fieldRef.table);
    ret.org_table := ToText(fieldRef.org_table);
    ret.db := ToText(fieldRef.db);
    ret.catalog := ToText(fieldRef.catalog);
    ret.def := ToText(fieldRef.def);
    ret.length := fieldRef.length;
    ret.max_length := fieldRef.max_length;
    ret.name_length := fieldRef.name_length;
    ret.org_name_length := fieldRef.org_name_length;
    ret.table_length := fieldRef.table_length;
    ret.org_table_length := fieldRef.org_table_length;
    ret.db_length := fieldRef.db_length;
    ret.catalog_length := fieldRef.catalog_length;
    ret.def_length := fieldRef.def_length;
    ret.flags := fieldRef.flags;
    ret.decimals := fieldRef.decimals;
    ret.charsetnr := fieldRef.charsetnr;
    ret.type := fieldRef.type;
    RETURN ret;
  END NewField;

PROCEDURE GetFieldList (fieldRef: MySQLRaw.RefMysqlFieldT; numFields: CARDINAL):
  RefFieldArray =
  VAR
    ret : RefFieldArray;
  BEGIN
    ret := NEW(RefFieldArray, numFields);
    FOR j := 0 TO numFields - 1 DO
      ret[j] := NewField(fieldRef^);
      INC(fieldRef, BYTESIZE(MySQLRaw.MYSQL_FIELD));
    END;
    RETURN ret;
  END GetFieldList;
%}

