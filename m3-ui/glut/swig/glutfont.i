
//the glut font typemaps

%typemap(m3rawintype) void * %{REF INTEGER%}
%typemap(m3wrapintype) void * %{REFANY%}

//the string typemaps
%typemap(m3rawinmode)   const unsigned char *string   %{READONLY%}
%typemap(m3wrapinmode)   const unsigned char *string    %{READONLY%}
%typemap(m3rawintype)   const unsigned char *string   %{C.char_star%}
%typemap(m3wrapintype)   const unsigned char *string   %{TEXT%}

%typemap(m3wrapargvar)  const unsigned char *string %{$1: C.char_star;%}
%typemap(m3wrapinconv) const unsigned char *string  %{$1 := M3toC.SharedTtoS($1_name);%}
%typemap(m3wrapargraw)  const unsigned char *string %{$1%}
%typemap(m3wrapfreearg)  const unsigned char *string %{M3toC.FreeSharedS($1_name,$1);%}

/*
 * Font stuff, see freeglut_font.c
 */
FGAPI void    FGAPIENTRY glutBitmapCharacter( void* font, int character );
FGAPI int     FGAPIENTRY glutBitmapWidth( void* font, int character );
FGAPI void    FGAPIENTRY glutStrokeCharacter( void* font, int character );
FGAPI int     FGAPIENTRY glutStrokeWidth( void* font, int character );
FGAPI int     FGAPIENTRY glutBitmapLength( void* font, const unsigned char* string );
FGAPI int     FGAPIENTRY glutStrokeLength( void* font, const unsigned char* string );
