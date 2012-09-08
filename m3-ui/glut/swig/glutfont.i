
//the glut font typemaps

//the string typemaps
%typemap(m3rawinmode)   const unsigned char *string   %{%}
%typemap(m3wrapinmode)  const unsigned char *string   %{READONLY%}
%typemap(m3rawintype)   const unsigned char *string   %{C.char_star%}
%typemap(m3wrapintype)  const unsigned char *string   %{TEXT%}

%typemap(m3wrapargvar)  const unsigned char *string   %{$1: C.char_star;%}
%typemap(m3wrapinconv)  const unsigned char *string   %{$1 := M3toC.SharedTtoS($1_name);%}
%typemap(m3wrapargraw)  const unsigned char *string   %{$1%}
%typemap(m3wrapfreearg) const unsigned char *string   %{M3toC.FreeSharedS($1_name,$1);%}

%typemap(m3wrapintype)  void *  %{Fonts%}
%typemap(m3rawintype)   void *  %{ADDRESS%}
%typemap(m3wrapinmode)  void *  %{VALUE%}

%typemap(m3wrapargraw)  void *  %{$1%}
%typemap(m3wrapargvar)  void *  %{$1 : ADDRESS := mf[$1_name];%}

/*
 * Font stuff, see freeglut_font.c
 */


%insert(m3wrapintf) %{
TYPE
  Fonts = {StrokeRoman,
           StrokeMonoRoman,
           Bitmap9By15,
           Bitmap8By13,
           BitmapTimesRoman10,
           BitmapTimesRoman24,
           BitmapHelvetica10,
           BitmapHelvetica12,
           BitmapHelvetica18};

  FontList = ARRAY Fonts OF ADDRESS;
%}

%insert(m3wrapimpl) %{
VAR
  mf := FontList{ADR(GLUTRaw.glutStrokeRoman),
                 ADR(GLUTRaw.glutStrokeMonoRoman),
                 ADR(GLUTRaw.glutBitmap9By15),
                 ADR(GLUTRaw.glutBitmap8By13),
                 ADR(GLUTRaw.glutBitmapTimesRoman10),
                 ADR(GLUTRaw.glutBitmapTimesRoman24),
                 ADR(GLUTRaw.glutBitmapHelvetica10),
                 ADR(GLUTRaw.glutBitmapHelvetica12),
                 ADR(GLUTRaw.glutBitmapHelvetica18)};
%}

FGAPI void    FGAPIENTRY glutBitmapCharacter( void* font, int character );
FGAPI int     FGAPIENTRY glutBitmapWidth( void* font, int character );
FGAPI void    FGAPIENTRY glutStrokeCharacter( void* font, int character );
FGAPI int     FGAPIENTRY glutStrokeWidth( void* font, int character );
FGAPI int     FGAPIENTRY glutBitmapLength( void* font, const unsigned char* string );
FGAPI int     FGAPIENTRY glutStrokeLength( void* font, const unsigned char* string );

FGAPI int     FGAPIENTRY glutBitmapHeight( void* font );
FGAPI GLfloat FGAPIENTRY glutStrokeHeight( void* font );


FGAPI void    FGAPIENTRY glutBitmapString( void* font, const unsigned char *string );
FGAPI void    FGAPIENTRY glutStrokeString( void* font, const unsigned char *string );

/*
The fonts themselvs are really static variables. We declare them as abitrary chars
we only really need the address of them.
*/

//cancels rename for these vars
%rename("%(lowercamelcase)s") "";

extern const char glutStrokeRoman;
extern const char glutStrokeMonoRoman;
extern const char glutBitmap9By15;
extern const char glutBitmap8By13;
extern const char glutBitmapTimesRoman10;
extern const char glutBitmapTimesRoman24;
extern const char glutBitmapHelvetica10;
extern const char glutBitmapHelvetica12;
extern const char glutBitmapHelvetica18;
