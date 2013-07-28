%module QtFocusFrame

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qfocusframe.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QFocusFrame;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

DoType(QWidget,QtWidget)

%include <QtGui/qfocusframe.h>
