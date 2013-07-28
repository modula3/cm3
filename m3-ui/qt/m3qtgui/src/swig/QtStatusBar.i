%module QtStatusBar

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qstatusbar.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QStatusBar;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

DoType(QWidget,QtWidget);

%include <QtGui/qstatusbar.h>
