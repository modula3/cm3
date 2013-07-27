%module QtEvent

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

%{
#include <QtCore/qcoreevent.h>
%}

%feature("compactdefaultargs") QEvent::registerEventType;

%insert(m3wrapintf) %{
TYPE
  T = QEvent;
  QByteArray = ADDRESS;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtObject IMPORT QObject;
%}


//raw for imported classes

%typemap("m3rawrettype")   const QByteArray &   %{ADDRESS%}
%typemap("m3rawrettype")   QByteArray           %{ADDRESS%}
%typemap("m3rawintype")    const QByteArray &   %{ADDRESS%}

//Local Enums
EnumMaps(QEvent, Type, ErrMode)

%apply ClassIn     {QObject *};
%apply ClassReturn {QObject *};

//Circular imports with QObject

%typemap("m3wrapintype")  QObject *child   %{REFANY (*QObject*)%}
%typemap("m3wrapouttype") QObject *child   %{REFANY (*QObject*)%}
%typemap("m3wraprettype") QObject *child   %{REFANY%}
%typemap("m3wrapargvar")  QObject *child   %{$1tmp :=  LOOPHOLE(NARROW($1_name,QObject).cxxObj,ADDRESS);%}

%include <QtCore/qcoreevent.h>
