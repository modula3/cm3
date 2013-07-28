%module QtUndoView

%include "m3qt.i"
%include "common.i"

%import "QtListView.i"

%{
#include <QtGui/qundoview.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtListView IMPORT QListView;

TYPE
  T = QUndoView;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}


DoType(QIcon,QtIcon)

DoType(QUndoStack,QGuiStubs)
DoType(QUndoGroup,QGuiStubs)


%include <QtGui/qundoview.h>
