%module QtAbstractScrollArea

%include "m3qt.i"
%include "common.i"

%import "QtFrame.i"

%{
#include <QtGui/qabstractscrollarea.h>
%}

%insert(m3rawintf) %{
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{
FROM QtFrame IMPORT QFrame;

TYPE
  T = QAbstractScrollArea;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%apply ClassIn {QWidget *};
%apply ClassIn {QScrollBar *};
%apply ClassReturn {QScrollBar *};
%apply ClassReturn {QWidgetList};

%apply ClassReturn  {QWidget *viewport};
%apply ClassReturn  {QWidget *cornerWidget};

%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget%}
%typemap("m3wrapintype:import")  QScrollBar * %{QtScrollBar QScrollBar%}

%typemap("m3wrapretvar:import")  QWidgetList %{QGuiStubs QWidgetList%}

%include <QtGui/qabstractscrollarea.h>
