%module QtScrollArea

%include "m3qt.i"
%include "common.i"

%import "QtAbstractScrollArea.i"

%{
#include <QtGui/qscrollarea.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtAbstractScrollArea IMPORT QAbstractScrollArea;
TYPE
  T = QScrollArea;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%apply ClassReturn  {QWidget *widget};
%apply ClassReturn  {QWidget *takeWidget};


%include <QtGui/qscrollarea.h>
