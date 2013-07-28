%module QtApplication

%include "m3qt.i"
%include "common.i"

//not done yet
//%import "QtCoreApplication.i"
//%import "QtWidget.i" //probs with this import
%import "QtNamespace.i"

%{
#include <QtGui/qapplication.h>
#include <QtGui/qfont.h>
#include <QtGui/qfontmetrics.h>
#include <QtGui/qpalette.h>
#include <QtGui/qicon.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QApplication;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
FROM QtString IMPORT QString;

VAR
  m3argc : C.int;
  m3argv : UNTRACED REF ARRAY OF C.char_star;
%}

//%rename("exec3") exec;

%ignore QApplication::allWidgets;
%ignore QApplication::topLevelWidgets;
%ignore QApplication::desktop;
%ignore QApplication::clipboard;

%ignore commitData;
%ignore saveState;
%ignore QApplication::keyboardInputLocale;
%ignore notify;

%rename(AppLayoutDirection) QApplication::layoutDirection;


/* original
%typemap("ctype")           int &     %{int *%}
%typemap("m3wrapintype")    int &     %{INTEGER%}
%typemap("m3rawintype")     int &     %{C.int%}
%typemap("m3wrapargraw")    int &     %{m3argc%}
%typemap("m3wrapinmode")    int &     %{%}
%typemap("m3rawinmode")     int &     %{VAR%}
%typemap("m3wrapinconv")    int &     %{m3argc := ORD($1_name);%}
//%typemap("m3wrapargraw")    int &     %{$1tmp%}
//%typemap("m3wrapargvar")    int &     %{m3argc : C.int;%}
//%typemap("m3wrapargvar")    int &     %{m3argc : C.int := ORD($1_name);%}

//char ** mainly for arrgv
%typemap("m3rawinmode")   char **    %{%}
%typemap("m3wrapinmode")  char **    %{READONLY%}
%typemap("m3rawintype")   char **    %{ADDRESS%}
%typemap("m3wrapintype")  char **    %{ARRAY OF TEXT%}
%typemap("m3wrapargvar")  char **    %{%}
//%typemap("m3rawintype")   char **   %{(*ARRAY OF*) C.char_star%}
//%typemap("m3wrapargvar")  char **   %{$1: C.char_star;%}



%typemap("m3wrapinconv")  char **   %{
$1 := NEW(UNTRACED REF ARRAY OF C.char_star, m3argc + 1);
FOR i := 0 TO m3argc  - 1 DO
$1[i] := M3toC.CopyTtoS($1_name[i]);
END;
$1[m3argc] := NIL;%}

%typemap("m3wrapargraw")  char **   %{ADR($1[0])%}
*/


%typemap("ctype")           int & argc    %{int *%}
%typemap("m3wrapintype")    int & argc    %{INTEGER%}
%typemap("m3rawintype")     int & argc    %{C.int%}
%typemap("m3wrapargraw")    int & argc    %{m3argc%}
%typemap("m3wrapinmode")    int & argc    %{%}
%typemap("m3rawinmode")     int & argc    %{VAR%}
%typemap("m3wrapinconv")    int & argc    %{m3argc := ORD($1_name);%}

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


//override the const for methods putting in readonly
%typemap("m3rawinmode")    const SWIGTYPE * ""
%typemap("m3wrapinmode")   const SWIGTYPE * ""


//rename an enum no lets rename the functions instead
//%rename("ColorSpecT") QApplication::ColorSpec;

//rename the function so not clash with enums
%rename("AppType") type();
%rename("GetColorSpec") colorSpec();

//Local Enums
EnumMaps(QApplication, Type, ErrMode)
EnumMaps(QApplication, ColorSpec, ErrMode)

DoType(QSize,QtSize)
DoType(QPoint,QtPoint)
DoType(QStyle,QtStyle)
DoType(QCursor,QtCursor)
DoType(QIcon,QtIcon)
DoType(QPalette,QtPalette)
DoType(QFont,QtFont)
DoType(QFontMetrics,QtFontMetrics)
DoType(QInputContext,QtInputContext)
DoType(QWidget,QtWidget)


%include <QtGui/qapplication.h>
