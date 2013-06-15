
/*
  typemaps for an int *arg char **arg function where we have to convert the
  parm before the raw call needed 5 typemap settings for int and 8 for **argv
*/

//the int * typemaps

%typemap("ctype")           int * pargc    %{int *%}
%typemap("m3wrapintype")    int * pargc    %{CARDINAL%}
%typemap("m3rawintype")     int * pargc    %{C.int%}
%typemap("m3wrapargraw")    int * pargc    %{m3argc%}
%typemap("m3wrapinmode")    int * pargc    %{%}
%typemap("m3rawinmode")     int * pargc    %{VAR%}
%typemap("m3wrapinconv")    int * pargc    %{m3argc := ORD($1_name);%}

//char ** for argv
%typemap("m3rawinmode")   char ** argv   %{%}
%typemap("m3wrapinmode")  char ** argv   %{READONLY%}
%typemap("m3rawintype")   char ** argv   %{ADDRESS%}
%typemap("m3wrapintype")  char ** argv   %{ARRAY OF TEXT%}
%typemap("m3wrapargraw")  char ** argv   %{ADR(m3argv[0])%}
%typemap("m3wrapargvar")  char ** argv   %{%}


%typemap("m3wrapinconv")  char ** argv
%{m3argv := NEW(UNTRACED REF ARRAY OF C.char_star, m3argc + 1);
FOR i := 0 TO m3argc  - 1 DO
m3argv[i] := M3toC.CopyTtoS($1_name[i]);
END;
m3argv[m3argc] := NIL;%}



//the api
FGAPI void    FGAPIENTRY glutInit( int* pargc, char** argv );

