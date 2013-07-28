%module QtMdiSubWindow

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qmdisubwindow.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QMdiSubWindow;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtMdiArea IMPORT QMdiArea;
%}

//Local enums
EnumMaps(QMdiSubWindow, SubWindowOption, ErrMode)

//Local classes
%apply ClassReturn {QMdiArea *};

//circ imports for mdiarea
%typemap("m3wraprettype") QMdiArea *      %{REFANY%}
%typemap("m3wrapouttype") QMdiArea *      %{REFANY%}
%typemap("m3wrapargvar")  QMdiArea *      %{$1tmp :=  LOOPHOLE(NARROW($1_name,QMdiArea).cxxObj,ADDRESS);%}

DoType(QMenu,QtMenu);
DoType(QWidget,QtWidget);

%include <QtGui/qmdisubwindow.h>
