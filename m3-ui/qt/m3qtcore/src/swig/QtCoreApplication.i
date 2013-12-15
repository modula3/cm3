%module QtCoreApplication

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtCore/qcoreapplication.h>
#include <QtCore/qstringlist.h>
%}

%insert(m3rawintf) %{

TYPE
  EventFilterT = PROCEDURE(message : ADDRESS; result : UNTRACED REF LONGINT);

%}

%insert(m3wrapintf) %{
FROM QtObject IMPORT QObject;

TYPE
  T = QCoreApplication;

  EventFilterT = PROCEDURE(message : ADDRESS; result : UNTRACED REF LONGINT);
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtString IMPORT QString;
FROM QtByteArray IMPORT QByteArray;
FROM QtObject IMPORT QObject;

VAR
  m3argc : C.int;
  m3argv : UNTRACED REF ARRAY OF C.char_star;
%}

//rename the result parm in filterEvent it shadows a local
#define result outResult

//No translator yet
%ignore installTranslator;
%ignore removeTranslator;
//dont need a post routine
%ignore qAddPostRoutine;
%ignore qRemovePostRoutine;

//these 2 cause link errors for some reason
%ignore qAppName;
%ignore qt_sendSpontaneousEvent;

//fixme maybe needs rename on the cleanup
%ignore setEventFilter;

//this is a strange one needs special typemap - fixme sometime
%ignore instance;

//Local Enums
EnumMaps(QCoreApplication, Encoding, ErrMode)

//remote flags
EnumFlags(QEventLoop::ProcessEventsFlags, ProcessEventsFlags)
EnumFlagsImport(QEventLoop::ProcessEventsFlags, ProcessEventsFlags, QtEventLoop)

//args and argv 
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


%typemap("m3rawintype")  QCoreApplication::EventFilter   %{EventFilterT%}
%typemap("m3rawrettype") QCoreApplication::EventFilter   %{EventFilterT%}
%typemap("m3wrapintype") QCoreApplication::EventFilter   %{EventFilterT%}
%typemap("m3wraprettype") QCoreApplication::EventFilter   %{EventFilterT%}
%typemap("m3wrapouttype") QCoreApplication::EventFilter   %{EventFilterT%}

%typemap("m3wrapintype") void *   %{ADDRESS%}
%typemap("m3wrapintype") long *   %{UNTRACED REF LONGINT%}
%typemap("m3rawintype") long *   %{UNTRACED REF LONGINT%}

DoType(QStringList,QtStringList)

%include <QtCore/qcoreapplication.h>
