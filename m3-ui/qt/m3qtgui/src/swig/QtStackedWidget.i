%module QtStackedWidget

%include "m3qt.i"
%include "common.i"

%import "QtFrame.i"

%{
#include <QtGui/qstackedwidget.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtFrame IMPORT QFrame;

TYPE
  T = QStackedWidget;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

DoType(QWidget,QtWidget)

%include <QtGui/qstackedwidget.h>
