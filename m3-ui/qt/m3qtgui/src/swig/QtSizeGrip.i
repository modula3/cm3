%module QtSizeGrip

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qsizegrip.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QSizeGrip;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

DoType(QWidget,QtWidget);

%include <QtGui/qsizegrip.h>
