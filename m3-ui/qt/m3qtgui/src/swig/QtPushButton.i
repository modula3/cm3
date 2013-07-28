%module QtPushButton

%include "m3qt.i"
%include "common.i"

%import "QtAbstractButton.i"

%{
#include <QtGui/qpushbutton.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtAbstractButton IMPORT QAbstractButton;
TYPE
  T = QPushButton;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget%}

DoType(QMenu,QtMenu)

%include <QtGui/qpushbutton.h>
