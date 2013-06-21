//common tmaps for C types

%typemap("m3rawrettype")    int &      %{UNTRACED REF INTEGER%}
%typemap("m3rawrettype")    double &   %{UNTRACED REF LONGREAL%}


//typemaps for int returns
%typemap("m3rawrettype")    int &      %{UNTRACED REF C.int%}
%typemap("m3wraprettype")   int &      %{UNTRACED REF INTEGER%}
%typemap("m3wrapouttype")   int &      %{UNTRACED REF INTEGER%}

%typemap("m3wrapretvar")    int &      %{ret : UNTRACED REF C.int;
result := NEW(UNTRACED REF INTEGER);%};
%typemap("m3wrapretraw")    int &      %{ret%}
%typemap("m3wrapretconv")   int &      %{result%}
%typemap("m3wrapretcheck")  int &      %{result^ := ret^; %}


//int * input parms
//dont think these are correct although they compile need to test
//more thoroghly the ones below from the opengl are better even though the
//rawintype map is overridden somewhere so it gets lost and the compile fails
%typemap("m3wrapinmode")    int *     %{VAR%}
%typemap("m3wrapintype")    int *     %{INTEGER%}
%typemap("m3wrapargvar")    int *     %{$1tmp: C.int;%}
%typemap("m3wrapinconv")    int *     %{$1tmp := $1_name;%}
%typemap("m3wrapoutconv")   int *     %{$1tmp%}
%typemap("m3wrapargraw")    int *     %{$1tmp%}


//int * from opengl
/*
%typemap("m3rawinmode")   int *  %{%}
%typemap("m3wrapinmode")  int *  %{%}
%typemap("m3rawintype")   int *  %{UNTRACED REF ARRAY OF C.int%}
%typemap("m3rawrettype")  int *  %{UNTRACED REF ARRAY OF C.int%}
%typemap("m3wrapintype")  int *  %{UNTRACED REF ARRAY OF INTEGER%}
%typemap("m3wraprettype") int *  %{UNTRACED REF ARRAY OF INTEGER%}
%typemap("m3wrapargvar")  int *  %{$1tmp := LOOPHOLE($1_name,UNTRACED REF ARRAY OF C.int);%}
%typemap("m3wrapargraw")  int *  %{$1tmp%}

%typemap("m3wrapretvar")  int *   %{ret : UNTRACED REF ARRAY OF C.int;
result : UNTRACED REF ARRAY OF INTEGER;%}
%typemap("m3wrapretraw")  int *   %{ret%}
%typemap("m3wrapretconv") int *   %{result%}
%typemap("m3wrapretcheck") int *  %{result := LOOPHOLE(ret,UNTRACED REF ARRAY OF INTEGER);%};
*/
//end maps from opengl

//double * input parms
%typemap("m3wrapinmode")    double *  %{VAR%}
%typemap("m3wrapintype")    double *  %{LONGREAL%}
%typemap("m3wrapargvar")    double *  %{$1tmp: C.double;%}
%typemap("m3wrapinconv")    double *  %{$1tmp := $1_name;%}
%typemap("m3wrapoutconv")   double *  %{$1tmp%}
%typemap("m3wrapargraw")    double *  %{$1tmp%}

//typemaps for double returns
%typemap("m3rawrettype")    double &  %{UNTRACED REF C.double%}
%typemap("m3wraprettype")   double &  %{UNTRACED REF LONGREAL%}
%typemap("m3wrapouttype")   double &  %{UNTRACED REF LONGREAL%}

%typemap("m3wrapretvar")    double &  %{ret : UNTRACED REF C.double;
result := NEW(UNTRACED REF LONGREAL);%};
%typemap("m3wrapretraw")    double &  %{ret%}
%typemap("m3wrapretconv")   double &  %{result%}
%typemap("m3wrapretcheck")  double &  %{result^ := ret^; %}


//the char * typemaps
%typemap("m3rawinmode")     char *   %{%}
%typemap("m3wrapinmode")    char *   %{%}
%typemap("m3rawintype")     char *   %{C.char_star%}
%typemap("m3wrapintype")    char *   %{TEXT%}
%typemap("m3wrapargvar")    char *   %{$1tmp: C.char_star;%}
%typemap("m3wrapinconv")    char *   %{$1tmp := M3toC.CopyTtoS($1_name);%}
//%typemap("m3wrapinconv")    char *   %{$1tmp := M3toC.SharedTtoS($1_name);%}
%typemap("m3wrapargraw")    char *   %{$1tmp%}
/*the wrapfreearg had this code
but it was causing double delete crashes
M3toC.FreeSharedS($1_name,$1tmp);
M3toC.FreeCopiedS($1tmp);
*/
%typemap("m3wrapfreearg")   char *   %{
%}

%apply char * {const char *}; //other applies??
//this causes errors in qpixmap where const unsigned char is overloaded
//perhaps the new cmaps module will work better
//%apply char * {const unsigned char *};

//move uchar * to common
//this needs a define to unsigned char
%typemap("m3rawrettype")   uchar *        %{UNTRACED REF C.unsigned_char%}
%typemap("m3wraprettype")  uchar *        %{UNTRACED REF CHAR%}

%typemap("m3rawintype")    uchar *        %{UNTRACED REF CHAR%}
%typemap("m3wrapintype")   uchar *        %{UNTRACED REF CHAR%}

%typemap("m3wrapretvar")   uchar *        %{ret:ADDRESS; result : UNTRACED REF CHAR;%}
%typemap("m3wrapretraw")   uchar *        %{ret%};
%typemap("m3wrapretconv")  uchar *        %{result%}
%typemap("m3wrapouttype")  uchar *        %{UNTRACED REF CHAR%}
%typemap("m3wrapretcheck") uchar *        %{result := LOOPHOLE(result,UNTRACED REF CHAR);%}

%apply uchar * {const uchar *}


//char ** mainly for arrgv
/* fixme see qapplication
%typemap("m3rawinmode")   char **   %{READONLY%}
%typemap("m3wrapinmode")  char **   %{READONLY%}
%typemap("m3rawintype")   char **   %{(*ARRAY OF*) C.char_star%}
%typemap("m3wrapintype")  char **   %{ARRAY OF TEXT%}

%typemap("m3wrapargvar")  char **   %{$1: C.char_star;%}
//%typemap("m3wrapinconv")  char **   %{$1 := M3toC.SharedTtoS($1_name[0]);%}
%typemap("m3wrapinconv")  char **   %{$1 := M3toC.CopyTtoS($1_name[0]);%}
%typemap("m3wrapargraw")  char **   %{$1%}
//%typemap("m3wrapfreearg") char **   %{M3toC.FreeSharedS($1_name[0],$1);%}
%typemap("m3wrapfreearg") char **   %{M3toC.FreeCopiedS($1);%}
*/

//char * returns
%typemap("m3wrapretvar")    char *  %{result : C.char_star;%};
%typemap("m3wrapretcheck")  char *  %{%}
//swig is now importing into interface which is kind of bug this stops this import
%typemap("m3wrapretvar:import")  char * ""
%typemap("m3wrapretvar:import")  const char * ""

//some basic string and bytearray types
%typemap("m3rawintype")   QChar           %{ADDRESS%}
%typemap("m3rawintype")   qlonglong       %{LONGINT%}
%typemap("m3rawintype")   qulonglong      %{LONGINT%}
%typemap("m3rawintype")   ulong           %{LONGINT%}
%typemap("m3rawintype")   uint            %{CARDINAL%}
%typemap("m3rawintype")   ushort          %{INTEGER%}
%typemap("m3rawintype")   uchar           %{CHAR%}

%typemap("m3wrapintype")   bool   *       %{BOOLEAN%}
%typemap("m3wrapinmode")   bool   *       %{VAR%}
%typemap("m3rawinmode")    bool   *       %{VAR%}



//refs seems to work except for wrapped return type is integer instead of untraced ref integer
//also this is a copy ref see the qapplication for tmaps that pass reff
//back to caller without copy
%typemap("m3rawrettype")    int &  %{UNTRACED REF C.int%}
%typemap("m3wraprettype")   int &  %{UNTRACED REF INTEGER%}
%typemap("m3wrapouttype")   int &  %{UNTRACED REF INTEGER%}
%typemap("m3wrapretvar")    int &  %{ret : UNTRACED REF C.int;
result := NEW(UNTRACED REF INTEGER);%};
%typemap("m3wrapretraw")    int &  %{ret%}
%typemap("m3wrapretconv")   int &  %{result%}
%typemap("m3wrapretcheck")  int &  %{result^ := ret^; %}

//ref needs a pointer since am
//getting segv on argc in wrapped cxx
%typemap("ctype") int & %{$1_basetype *%}

%apply int & {int *}; //??other applies const etc??
