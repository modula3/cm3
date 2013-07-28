%module QtDesktopWidget

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qdesktopwidget.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QDesktopWidget;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

DoType(QWidget,QtWidget);

%include <QtGui/qdesktopwidget.h>
