// depends on mysqltypes.i

//the string typemaps
//the const char * typemaps
%typemap(m3wrapinmode)  const char *   %{%} //was VALUE
%typemap(m3rawintype)   const char *   %{C.const_char_star%}
%typemap(m3wrapintype)  const char *   %{TEXT%}
%typemap(m3wrapargvar)  const char *   %{$1: C.const_char_star;%}
%typemap(m3wrapinconv)  const char *   %{$1 := NewString($1_name);%}
%typemap(m3wrapargraw)  const char *   %{$1%}
%typemap(m3wrapfreearg) const char *   %{FreeString($1_name,$1);%}

//the char * typemaps
%typemap(m3wrapinmode)  char *         %{%} //was VALUE
%typemap(m3rawintype)   char *         %{C.char_star%}
%typemap(m3wrapintype)  char *         %{TEXT%}
%typemap(m3wrapargvar)  char *         %{$1: C.char_star;%}
%typemap(m3wrapinconv)  char *         %{$1 := NewString($1_name);%}
%typemap(m3wrapargraw)  char *         %{$1%}
%typemap(m3wrapfreearg) char *         %{FreeString($1_name,$1);%}

//the char** typemaps
%typemap(m3rawinmode)   char **        %{READONLY%}
%typemap(m3wrapinmode)  char **        %{READONLY%}
%typemap(m3rawintype)   char **        %{(*ARRAY OF*) C.char_star%}
%typemap(m3wrapintype)  char **        %{ARRAY OF TEXT%}
%typemap(m3wrapargvar)  char **        %{$1: C.char_star;%}
%typemap(m3wrapinconv)  char **        %{$1 := NewString($1_name[0]);%}
%typemap(m3wrapargraw)  char **        %{$1%}
%typemap(m3wrapfreearg) char **        %{FreeString($1_name[0],$1);%}

//the void * typemaps
%typemap("m3wrapintype")  const void * %{REFANY%}
%typemap("m3wrapintype")        void * %{REFANY%}

%typemap("m3wrapargraw")  const void * %{$1%}
%typemap("m3wrapargraw")        void * %{$1%}
%typemap("m3wrapargvar")  const void * %{$1: ADDRESS := LOOPHOLE($1_name,ADDRESS);%}
%typemap("m3wrapargvar")        void * %{$1: ADDRESS := LOOPHOLE($1_name,ADDRESS);%}

//my_ulonglong typemaps
%typemap("m3rawrettype")  my_ulonglong %{C.unsigned_long_long%}
%typemap("m3wraprettype") my_ulonglong %{LONGINT%}

%typemap("m3rawintype")   my_ulonglong %{C.unsigned_long_long%}
%typemap("m3wrapintype")  my_ulonglong %{LONGINT%}

//unsigned long typemaps
%typemap("m3rawrettype")   unsigned long *  %{C.unsigned_long_star%}
%typemap("m3wraprettype")  unsigned long *  %{RefLengthsT%}

%typemap("m3wrapretvar")   unsigned long *  %{ret : C.unsigned_long_star;
result : RefLengthsT;%};

%typemap("m3wrapretraw")   unsigned long *  %{ret%};
%typemap("m3wrapretcheck") unsigned long *  %{result := LOOPHOLE(ret,RefLengthsT);%};
%typemap("m3wrapretconv")  unsigned long *  %{result%};

//my_bool typemaps
%typemap("m3rawrettype")   my_bool     %{my_bool%}
%typemap("m3wraprettype")  my_bool     %{BOOLEAN%}

%typemap("m3wrapretvar")   my_bool     %{ret : MySQLRaw.my_bool;
result : BOOLEAN;%};

%typemap("m3wrapretraw")   my_bool     %{ret%};
%typemap("m3wrapretcheck") my_bool     %{result := VAL(ret,BOOLEAN);%};
%typemap("m3wrapretconv")  my_bool     %{result%};

%typemap("m3rawintype")    my_bool     %{my_bool%}
%typemap("m3wrapintype")   my_bool     %{BOOLEAN%}
%typemap("m3wrapargraw")   my_bool     %{$1%}
%typemap("m3wrapargvar")   my_bool     %{$1 := LOOPHOLE($1_name,MySQLRaw.my_bool);%}

//exceptions for int returns
%typemap("m3wrapretcheck")         int  %{IF result # 0 THEN RAISE ReturnE(result); END;%};
%typemap("m3wrapretcheck:throws")  int %{ReturnE%}

/* trying to remove the int return type from the safe interface and only
have the exception proving difficult
%typemap("m3wraprettype")  int     %{%}
//%typemap("m3wrapretraw")  int     %{%}

//%typemap("m3wrapretvar")   int     %{result : INTEGER;%}
//%typemap("m3wrapretconv")  int     %{%};
*/

%insert(m3wrapintf) %{
TYPE
  T <: ADDRESS; (* connection handle *)

  ResultT <: ADDRESS; (* result handle *)
  StmtT <: ADDRESS; (* statement handle *)
  RowOffsetT <: ADDRESS;
  FieldT <: ADDRESS;
  ParametersT <: ADDRESS;
  BindT <: ADDRESS;
  CharsT <: ADDRESS;
  
CONST
  MAX_COLUMNS = 1000; (* Arbitrary limit to how many cols returned in a query *)

EXCEPTION ConnE;
EXCEPTION ResultE;
EXCEPTION ReturnE(INTEGER);

TYPE
  RefLengthsT =  UNTRACED REF ARRAY [0..MAX_COLUMNS] OF INTEGER;
%}


/* This usage of zeroptr as null does not work
VAR
  zeroValue := 0;
  zeroPtr   := LOOPHOLE (ADR (zeroValue), C.char_star);
*/

%insert(m3wrapimpl) %{
TYPE
  RefRow = UNTRACED REF ARRAY [0..MAX_COLUMNS] OF C.char_star;

REVEAL
  T = UNTRACED BRANDED REF MySQLRaw.OPAQUE;
  ResultT = UNTRACED BRANDED REF MySQLRaw.MYSQL_RES_REC;
  StmtT = UNTRACED BRANDED REF MySQLRaw.OPAQUE;
  RowOffsetT = UNTRACED BRANDED REF MySQLRaw.OPAQUE;
  FieldT = UNTRACED BRANDED REF MySQLRaw.MYSQL_FIELD_REC;
  ParametersT = UNTRACED BRANDED REF MySQLRaw.OPAQUE;
  BindT = UNTRACED BRANDED REF MySQLRaw.OPAQUE;
  CharsT = UNTRACED BRANDED REF MySQLRaw.OPAQUE;

VAR Null := LOOPHOLE(0,ADDRESS);


PROCEDURE NewString(t : TEXT) : C.char_star =
VAR res : C.char_star;
BEGIN
  IF t = NIL THEN res := Null; ELSE res := M3toC.SharedTtoS(t); END;
  RETURN res;
END NewString;

PROCEDURE FreeString(t : TEXT; c : C.char_star) =
BEGIN
  IF t # NIL THEN M3toC.FreeSharedS(t,c); END;
END FreeString;
%}

//enums
%typemap("m3wrapintype")  enum mysql_enum_shutdown_level %{INTEGER%}
%typemap("m3wrapintype")  enum enum_mysql_set_option %{INTEGER%}
%typemap("m3wrapintype")  enum mysql_option %{INTEGER%}
%typemap("m3wrapintype")  enum enum_stmt_attr_type %{INTEGER%}

//MYSQL_FIELD_OFFSET  typemaps
%typemap("m3rawrettype")  MYSQL_FIELD_OFFSET %{C.unsigned_int%}
%typemap("m3wraprettype") MYSQL_FIELD_OFFSET %{CARDINAL%}
%typemap("m3rawintype")   MYSQL_FIELD_OFFSET %{C.unsigned_int%}
%typemap("m3wrapintype")  MYSQL_FIELD_OFFSET %{CARDINAL%}

//MYSQL_ROW typemaps need to return an array of texts
%typemap("m3rawrettype")  MYSQL_ROW    %{C.char_star_star%}
%typemap("m3wraprettype") MYSQL_ROW    %{REF ARRAY OF TEXT%}
%typemap("m3wrapretvar")  MYSQL_ROW    %{ret : C.char_star_star;
result : RefRow;
numFields : INTEGER;
row : REF ARRAY OF TEXT;%}

%typemap("m3wrapretraw")   MYSQL_ROW   %{ret%};
%typemap("m3wrapretcheck") MYSQL_ROW   %{IF ret = NIL THEN
  row := NIL;
ELSE
  result := LOOPHOLE(ret,RefRow);
  numFields := NumFields(res);
  row := NEW(REF ARRAY OF TEXT,numFields);
  FOR i := 0 TO numFields -1 DO
  (* DB NULL results in NIL M3 text *)
    IF result[i] # NIL THEN row[i] := M3toC.CopyStoT(result[i]) END;
  END;
END;%};

%typemap("m3wrapretconv")  MYSQL_ROW   %{row%};

//MYSQL typemaps
%typemap("m3rawintype")    MYSQL *       %{MYSQL%}
%typemap("m3rawinmode")    MYSQL *       %{%}

%typemap("m3wrapintype")   MYSQL *       %{T%}
%typemap("m3wrapinmode")   MYSQL *       %{%}

%typemap("m3rawrettype")   MYSQL *       %{MYSQL%}
%typemap("m3wraprettype")  MYSQL *       %{T%}

%typemap("m3wrapargraw")   MYSQL *       %{$1%}
%typemap("m3wrapargvar")   MYSQL *       %{$1: MySQLRaw.MYSQL := LOOPHOLE($1_name,MySQLRaw.MYSQL);%}

%typemap("m3wrapretvar")   MYSQL *       %{ret : MySQLRaw.MYSQL;
result : T;%};

%typemap("m3wrapretraw")   MYSQL *       %{ret%};
%typemap("m3wrapretconv")  MYSQL *       %{result%};
%typemap("m3wrapretcheck") MYSQL *       %{result := LOOPHOLE(ret,T);
IF result = NIL THEN 
  RAISE ConnE; 
END;%};
%typemap("m3wrapretcheck:throws")  MYSQL * %{ConnE%}



//MYSQL_RES typemaps
%typemap("m3rawintype")     MYSQL_RES *  %{MYSQL_RES%}
%typemap("m3rawinmode")     MYSQL_RES *  %{%}

%typemap("m3rawrettype")    MYSQL_RES *  %{MYSQL_RES%}
%typemap("m3wraprettype")   MYSQL_RES *  %{ResultT%}

%typemap("m3wrapretvar")    MYSQL_RES *  %{ret : MySQLRaw.MYSQL_RES;
result : ResultT;%};
%typemap("m3wrapretraw")    MYSQL_RES *  %{ret%};
%typemap("m3wrapretconv")   MYSQL_RES *  %{result%};
%typemap("m3wrapretcheck")  MYSQL_RES *  %{result := LOOPHOLE(ret,ResultT);
IF result = NIL THEN RAISE ResultE; END;%};

%typemap("m3wrapintype")    MYSQL_RES *  %{ResultT%}
%typemap("m3wrapinmode")    MYSQL_RES *  %{%}

%typemap("m3wrapargraw")    MYSQL_RES *  %{$1%}
%typemap("m3wrapargvar")    MYSQL_RES *  %{$1: MySQLRaw.MYSQL_RES := LOOPHOLE($1_name,MySQLRaw.MYSQL_RES);%}
%typemap("m3wrapretcheck:throws")  MYSQL_RES * %{ResultE%}

//NO Need to import MySQLRaw into safe interface
//%typemap("m3wrapintype:import")  MYSQL_RES *res %{MySQLRaw%}


//MYSQL_ROW_OFFSET typemaps
%typemap("m3rawrettype")    MYSQL_ROW_OFFSET   %{MYSQL_ROW_OFFSET%}
%typemap("m3wraprettype")   MYSQL_ROW_OFFSET   %{RowOffsetT%}
%typemap("m3rawintype")     MYSQL_ROW_OFFSET   %{MYSQL_ROW_OFFSET%}
%typemap("m3wrapintype")    MYSQL_ROW_OFFSET   %{RowOffsetT%}%typemap("m3wrapargvar")    MYSQL_ROW_OFFSET   %{$1: MySQLRaw.MYSQL_ROW_OFFSET := LOOPHOLE($1_name,MySQLRaw.MYSQL_ROW_OFFSET);%}
%typemap("m3wrapargraw")    MYSQL_ROW_OFFSET   %{$1%}

%typemap("m3wrapretvar")    MYSQL_ROW_OFFSET   %{ret : MySQLRaw.MYSQL_ROW_OFFSET;
result : RowOffsetT;%};
%typemap("m3wrapretraw")    MYSQL_ROW_OFFSET   %{ret%};
%typemap("m3wrapretconv")   MYSQL_ROW_OFFSET   %{result%};
%typemap("m3wrapretcheck")  MYSQL_ROW_OFFSET   %{result := LOOPHOLE(ret,RowOffsetT);%};


//MYSQL_FIELD typemaps
%typemap("m3rawrettype")   MYSQL_FIELD *  %{MYSQL_FIELD%}
%typemap("m3wraprettype")  MYSQL_FIELD *  %{FieldT%}

%typemap("m3wrapretvar")   MYSQL_FIELD *  %{ret : MySQLRaw.MYSQL_FIELD;
result : FieldT;%};
%typemap("m3wrapretraw")   MYSQL_FIELD *  %{ret%};
%typemap("m3wrapretconv")  MYSQL_FIELD *  %{result%};
%typemap("m3wrapretcheck") MYSQL_FIELD *  %{result := LOOPHOLE(ret,FieldT);%};

 
//MYSQL_PARAMETERS typemaps
%typemap("m3rawrettype")   MYSQL_PARAMETERS *  %{MYSQL_PARAMETERS%}
%typemap("m3wraprettype")  MYSQL_PARAMETERS *  %{ParametersT%}

%typemap("m3wrapretvar")   MYSQL_PARAMETERS *  %{ret : MySQLRaw.MYSQL_PARAMETERS;
result : ParametersT;%};
%typemap("m3wrapretraw")   MYSQL_PARAMETERS *  %{ret%};
%typemap("m3wrapretconv")  MYSQL_PARAMETERS *  %{result%};
%typemap("m3wrapretcheck") MYSQL_PARAMETERS *  %{result := LOOPHOLE(ret,ParametersT);%};


//MYSQL_STMT typemaps
%typemap("m3rawintype")     MYSQL_STMT *  %{MYSQL_STMT%}
%typemap("m3rawinmode")     MYSQL_STMT *  %{%}
%typemap("m3wrapinmode")    MYSQL_STMT *  %{%}
%typemap("m3wrapintype")    MYSQL_STMT *  %{StmtT%}

%typemap("m3rawrettype")    MYSQL_STMT *  %{MYSQL_STMT%}
%typemap("m3wraprettype")   MYSQL_STMT *  %{StmtT%}

%typemap("m3wrapargraw")    MYSQL_STMT *  %{$1%}
%typemap("m3wrapargvar")    MYSQL_STMT *  %{$1: MySQLRaw.MYSQL_STMT := LOOPHOLE($1_name,MySQLRaw.MYSQL_STMT);%}

%typemap("m3wrapretvar")    MYSQL_STMT *  %{ret : MySQLRaw.MYSQL_STMT;
result : StmtT;%};
%typemap("m3wrapretraw")    MYSQL_STMT *  %{ret%};
%typemap("m3wrapretconv")   MYSQL_STMT *  %{result%};
%typemap("m3wrapretcheck")  MYSQL_STMT *  %{result := LOOPHOLE(ret,StmtT);
IF result = NIL THEN RAISE ResultE; END;%};

//This adds the RAISES clause 
%typemap("m3wrapretcheck:throws")  MYSQL_STMT * %{ResultE%}

//MYSQL_BIND typemaps
%typemap("m3rawintype")     MYSQL_BIND *  %{MYSQL_BIND%}
%typemap("m3rawinmode")     MYSQL_BIND *  %{%}
%typemap("m3wrapinmode")    MYSQL_BIND *  %{%}
%typemap("m3wrapintype")    MYSQL_BIND *  %{BindT%}

%typemap("m3wrapargraw")    MYSQL_BIND *  %{$1%}
%typemap("m3wrapargvar")    MYSQL_BIND *  %{$1: MySQLRaw.MYSQL_BIND := LOOPHOLE($1_name,MySQLRaw.MYSQL_BIND);%}


//MYSQL_CHARSET_INFO typemaps
%typemap("m3rawintype")     MY_CHARSET_INFO *  %{MYSQL_CHARSET_INFO%}
%typemap("m3wrapintype")    MY_CHARSET_INFO *  %{CharsT%}

%typemap("m3rawinmode")     MY_CHARSET_INFO *  %{%}

%typemap("m3wrapargraw")    MY_CHARSET_INFO *  %{$1%}
%typemap("m3wrapargvar")    MY_CHARSET_INFO *  %{$1: MySQLRaw.MYSQL_CHARSET_INFO := LOOPHOLE($1_name,MySQLRaw.MYSQL_CHARSET_INFO);%}


//local infile typemaps
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
END P0;
%}

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
END P1;
%}

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


//specialize these char * for the callback
%typemap(m3wrapinmode)  char * (*extend_buffer) (void *, char *, unsigned long *) %{%}
%typemap(m3rawinmode)   char * (*extend_buffer) (void *, char *, unsigned long *) %{%}
%typemap(m3rawintype)   char * (*extend_buffer) (void *, char *, unsigned long *) %{ExtendRawCBT%}
%typemap(m3wrapintype)  char * (*extend_buffer) (void *, char *, unsigned long *) %{ExtendCBT%}
%typemap(m3wrapargraw)  char * (*extend_buffer) (void *, char *, unsigned long *) %{<*NOWARN*>P0%}

%typemap(m3wrapargvar)  char * (*extend_buffer) (void *, char *, unsigned long *) %{
PROCEDURE P0(p1 : C.void_star; p2 : C.char_star; p3 : REF C.unsigned_long) : C.char_star =
VAR 
  r1 : ADDRESS := p1;
  r2 := M3toC.CopyStoT(p2);
  r3 := LOOPHOLE(p3, REF LONGINT);
BEGIN
  RETURN M3toC.CopyTtoS(extend_buffer(r1,r2,r3));
END P0;
%}

