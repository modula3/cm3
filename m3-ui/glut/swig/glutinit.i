
/*
  typemaps for an int *arg char **arg function where we have to convert the
  parm before the raw call needed 5 typemap settings for int and 8 for **argv
*/

//the int * typemaps

%typemap(m3wrapinmode)  int *pargc %{VALUE%}
%typemap(m3wrapintype)  int *pargc %{CARDINAL%}

%typemap(m3wrapargvar)  int *pargc %{$1: C.int;%}
%typemap(m3wrapinconv)  int *pargc %{$1 := $1_name;%}
%typemap(m3wrapargraw)  int *pargc %{$1%}


//the char** typemaps

%typemap(m3rawinmode)   char **argv   %{READONLY%}
%typemap(m3wrapinmode)  char **argv   %{READONLY%}
%typemap(m3rawintype)   char **argv   %{(*ARRAY OF*) C.char_star%}
%typemap(m3wrapintype)  char **argv   %{ARRAY OF TEXT%}

%typemap(m3wrapargvar)  char **argv   %{$1: C.char_star;%}
%typemap(m3wrapinconv)  char **argv   %{$1 := M3toC.SharedTtoS($1_name[0]);%}
%typemap(m3wrapargraw)  char **argv   %{$1%}
%typemap(m3wrapfreearg) char **argv   %{M3toC.FreeSharedS($1_name[0],$1);%}


//the api
FGAPI void    FGAPIENTRY glutInit( int* pargc, char** argv );

