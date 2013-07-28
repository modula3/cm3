%module QtToolBar

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qtoolbar.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QToolBar;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

DoType(QObject,QtObject);
DoType(QAction,QtAction);
DoType(QWidget,QtWidget);

%include <QtGui/qtoolbar.h>
