%module QtEventLoop

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtCore/qeventloop.h>
#define ProcessEventsFlags QEventLoop::ProcessEventsFlags

%}

%insert(m3wrapintf) %{
FROM QtObject IMPORT QObject;

TYPE
  T = QEventLoop;
  ProcessEventsFlags = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtObject IMPORT QObject;
%}


//Local Enums
EnumMaps(QEventLoop, ProcessEventsFlag, ErrMode)

//Local flags
EnumFlags(ProcessEventsFlags, ProcessEventsFlags)

%include <QtCore/qeventloop.h>

