%module GLUT

/*
   usage
   swig -I/usr/include -modula3 glut.i
   then copy modula3 files into src dir
*/

%pragma(modula3) library="GLUT";
%pragma(modula3) unsafe="true";

%{
#include <GL/freeglut_std.h>
#include <GL/freeglut_ext.h>
%}

#define FGAPI
#define FGAPIENTRY

//remove dependancy on GL and typedef the GLenum to its base GL type
typedef unsigned int GLenum;

%insert(m3makefile) %{
import ("opengl")
import_lib("glut","/usr/lib")
%}

%insert(m3rawintf) %{
<*EXTERNAL *> VAR glutStrokeRoman : C.char;
<*EXTERNAL *> VAR glutStrokeMonoRoman : C.char;
<*EXTERNAL *> VAR glutBitmap9By15 : C.char;
<*EXTERNAL *> VAR glutBitmap8By13 : C.char;
<*EXTERNAL *> VAR glutBitmapTimesRoman10 : C.char;
<*EXTERNAL *> VAR glutBitmapTimesRoman24 : C.char;
<*EXTERNAL *> VAR glutBitmapHelvetica10 : C.char;
<*EXTERNAL *> VAR glutBitmapHelvetica12 : C.char;
<*EXTERNAL *> VAR glutBitmapHelvetica18 : C.char;
%}

%insert(m3wrapintf) %{
%}

%insert(m3wrapimpl) %{
VAR
  m3argc : C.int;
  m3argv : UNTRACED REF ARRAY OF C.char_star; %}


//strip the glut prefix from all names
%rename("%(strip:[glut])s") "";
//have to keep constants with prefix else it removes it for procs
//%rename("%(strip:[GLUT_])s") "";

//for ext these joystick funcs are deprecated
%ignore glutJoystickGetNumAxes;
%ignore glutJoystickGetNumButtons;
%ignore glutJoystickNotWorking;
%ignore glutJoystickGetDeadBand;
%ignore glutJoystickSetDeadBand;
%ignore glutJoystickGetSaturation;
%ignore glutJoystickSetSaturation;
%ignore glutJoystickSetMinRange;
%ignore glutJoystickSetMaxRange;
%ignore glutJoystickSetCenter;
%ignore glutJoystickGetMinRange;
%ignore glutJoystickGetMaxRange;
%ignore glutJoystickGetCenter;
//var args
%ignore glutInitErrorFunc;
%ignore glutInitWarningFunc;
//no need for this
%ignore GLUT_HAS_MULTI;

//map the GLint,GLfloat,GLdouble types to M3 types

%typemap("m3rawintype")   GLint %{INTEGER%}
%typemap("m3wrapintype")  GLint %{INTEGER%}

%typemap("m3rawintype")   GLdouble %{LONGREAL%}
%typemap("m3wrapintype")  GLdouble %{LONGREAL%}

%typemap("m3rawintype")   GLfloat %{REAL%}
%typemap("m3wrapintype")  GLfloat %{REAL%}
%typemap("m3wraprettype") GLfloat %{REAL%}

//the GetColor raw return type
%typemap("m3rawrettype")  GLfloat %{REAL%}

/*
  typemaps for an int *arg char **arg function where we have to convert the
  parm before the raw call needed 5 typemap settings for int and 8 for **argv
  could clean this up and not need global vars
*/

//the int * typemaps for argc

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
 
/*
 * Global callback functions, see freeglut_callbacks.c
 */

%insert(m3wrapintf) %{
(*
 * Callback functions, see freeglut_callbacks.c
*)
TYPE
  BYTE = [0..255];
  CallBack0T = PROCEDURE();
  CallBack1T = PROCEDURE(p1 : INTEGER);
  CallBack2T = PROCEDURE(p1,p2 : INTEGER);
  CallBack3T = PROCEDURE(p1,p2,p3 : INTEGER);
  CallBack4T = PROCEDURE(p1,p2,p3,p4 : INTEGER);
  CallBack5T = PROCEDURE(p1,p2,p3,p4,p5 : INTEGER);
  CallBack6T = PROCEDURE(p1 : BYTE; p2,p3 : INTEGER);
%}

%insert(m3rawintf) %{
TYPE
  void = BYTE;
  BYTE = [0..255];
  CallBack0T = PROCEDURE();
  CallBack1T = PROCEDURE(p1 : INTEGER);
  CallBack2T = PROCEDURE(p1,p2 : INTEGER);
  CallBack3T = PROCEDURE(p1,p2,p3 : INTEGER);
  CallBack4T = PROCEDURE(p1,p2,p3,p4 : INTEGER);
  CallBack5T = PROCEDURE(p1,p2,p3,p4,p5 : INTEGER);
  CallBack6T = PROCEDURE(p1 : BYTE; p2,p3 : INTEGER);
%}

//deprecated we dont import raw into safe interface
//%typemap("m3wrapintype:import")  void (*) (void) %{GLUTRaw%}

//these typemaps to remove the VAR for the callback so can pass procedure directly
%typemap(m3wrapinmode)  void (*) (void)  %{%}
%typemap(m3rawinmode)  void (*)  (void) %{%}
%typemap(m3wrapinmode)  void (*) (int) %{%}
%typemap(m3rawinmode)  void (*)  (int) %{%}
%typemap(m3wrapinmode)  void (*) (int, int) %{%}
%typemap(m3rawinmode)  void (*)  (int, int) %{%}
%typemap(m3wrapinmode)  void (*) (int, int, int) %{%}
%typemap(m3rawinmode)  void (*)  (int, int, int) %{%}
%typemap(m3wrapinmode)  void (*) (int, int, int, int) %{%}
%typemap(m3rawinmode)  void (*)  (int, int, int, int) %{%}
%typemap(m3wrapinmode)  void (*) (int, int, int, int, int) %{%}
%typemap(m3rawinmode)  void (*)  (int, int, int, int, int) %{%}
%typemap(m3wrapinmode)  void (*) (unsigned char, int, int) %{%}
%typemap(m3rawinmode)  void (*)  (unsigned char, int, int) %{%}
%typemap(m3wrapinmode)  void (*) (unsigned int, int, int, int) %{%}
%typemap(m3rawinmode)  void (*)  (unsigned int, int, int, int) %{%}


%typemap(m3rawintype)   void (*) (void) %{CallBack0T%}
%typemap(m3wrapintype)  void (*) (void) %{CallBack0T%}

%typemap(m3rawintype)   void (*) (int) %{CallBack1T%}
%typemap(m3wrapintype)  void (*) (int) %{CallBack1T%}

%typemap(m3rawintype)   void (*) (int, int) %{CallBack2T%}
%typemap(m3wrapintype)  void (*) (int, int) %{CallBack2T%}

%typemap(m3rawintype)   void (*) (int, int, int) %{CallBack3T%}
%typemap(m3wrapintype)  void (*) (int, int, int) %{CallBack3T%}

%typemap(m3rawintype)   void (*) (int, int, int, int) %{CallBack4T%}
%typemap(m3wrapintype)  void (*) (int, int, int, int) %{CallBack4T%}

%typemap(m3rawintype)   void (*) (int, int, int, int, int) %{CallBack5T%}
%typemap(m3wrapintype)  void (*) (int, int, int, int, int) %{CallBack5T%}

%typemap(m3rawintype)   void (*) (unsigned char, int, int) %{CallBack6T%}
%typemap(m3wrapintype)  void (*) (unsigned char, int, int) %{CallBack6T%}

%typemap(m3rawintype)   void (*) (unsigned int, int, int, int) %{CallBack4T%}
%typemap(m3wrapintype)  void (*) (unsigned int, int, int, int) %{CallBack4T%}

/*
 * Font stuff, see freeglut_font.c
 *
*/
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

%typemap(m3wrapintype)  void * font %{Fonts%}
%typemap(m3rawintype)   void * font %{ADDRESS%}
%typemap(m3wrapinmode)  void * font  %{VALUE%}

%typemap(m3wrapargraw)  void * font %{$1%}
%typemap(m3wrapargvar)  void * font  %{$1 : ADDRESS := mf[$1_name];%}

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

  FontList = ARRAY Fonts OF ADDRESS; %}

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
                 ADR(GLUTRaw.glutBitmapHelvetica18)};%}

%typemap(m3rawintype)     GLdouble offset[3] %{ARRAY [0..2] OF LONGREAL%}
%typemap(m3wrapintype)    GLdouble offset[3] %{ARRAY [0..2] OF LONGREAL%}

%typemap("m3rawrettype")  GLUTproc %{REF CallBack0T%}
%typemap("m3wraprettype") GLUTproc %{REF CallBack0T%}

//For the WindowData functions
%typemap("m3rawinmode")   void *  data %{%}
%typemap("m3wrapinmode")  void *  data %{%}
%typemap("m3rawrettype")  void *  %{ADDRESS%}
%typemap("m3wraprettype") void *  %{ADDRESS%}
%typemap("m3rawintype")   void * data %{ADDRESS%}
%typemap("m3wrapintype")  void * data %{ADDRESS%}

//for getmodevalues
%typemap("m3rawrettype")  int * %{REF C.int%}
%typemap("m3wraprettype")  int * %{REF INTEGER%}

%typemap("m3wrapretvar")  int *   %{ret : REF C.int; result : REF INTEGER;%}
%typemap("m3wrapretraw")  int *   %{ret%}
%typemap("m3wrapretconv") int *   %{result%}
%typemap("m3wrapretcheck") int *  %{result := LOOPHOLE(ret,REF INTEGER);%};

//int * overrides as var

%typemap("m3rawinmode")     myint *     %{VAR%}
%typemap("m3wrapinmode")    myint *     %{VAR%}
%typemap("m3rawintype")     myint *     %{C.int%}
%typemap("m3wrapintype")    myint *     %{INTEGER%}
%typemap("m3wrapargvar")    myint *     %{$1tmp: C.int;%}
%typemap("m3wrapinconv")    myint *     %{$1tmp := $1_name;%}
%typemap("m3wrapoutconv")   myint *     %{$1tmp%}
%typemap("m3wrapargraw")    myint *     %{$1tmp%}

//for getmodevalues
%apply myint * {int *size};

/*
The fonts themselvs are really static variables. We declare them as abitrary chars
we only really need the address of them.
*/

/*
//cancels rename for these vars
//%rename("%(lowercamelcase)s") "";

extern const char glutStrokeRoman;
extern const char glutStrokeMonoRoman;
extern const char glutBitmap9By15;
extern const char glutBitmap8By13;
extern const char glutBitmapTimesRoman10;
extern const char glutBitmapTimesRoman24;
extern const char glutBitmapHelvetica10;
extern const char glutBitmapHelvetica12;
extern const char glutBitmapHelvetica18;
*/
%ignore glutStrokeRoman;
%ignore glutStrokeMonoRoman;
%ignore glutBitmap9By15;
%ignore glutBitmap8By13;
%ignore glutBitmapTimesRoman10;
%ignore glutBitmapTimesRoman24;
%ignore glutBitmapHelvetica10;
%ignore glutBitmapHelvetica12;
%ignore glutBitmapHelvetica18;

%include <GL/freeglut_std.h>
%include <GL/freeglut_ext.h>
