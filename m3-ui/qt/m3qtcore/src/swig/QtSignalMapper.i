%module QtSignalMapper

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtCore/qsignalmapper.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QSignalMapper;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtString IMPORT QString;
%}

%typemap("m3rawintype")   QWidget *      %{REFANY%}
%typemap("m3wrapintype")  QWidget *      %{REFANY%}
%typemap("m3wrapinmode")  QWidget *      %{%}

DoType(QObject,QtObject)

%include <QtCore/qsignalmapper.h>
