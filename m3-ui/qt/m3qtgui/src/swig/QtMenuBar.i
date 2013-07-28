%module QtMenuBar

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qmenubar.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QMenuBar;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

DoType(QMenu,QtMenu);
DoType(QAction,QtAction);
DoType(QObject,QtObject);
DoType(QWidget,QtWidget);

%include <QtGui/qmenubar.h>
