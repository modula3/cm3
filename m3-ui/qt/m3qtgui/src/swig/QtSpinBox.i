%module QtSpinBox

%include "m3qt.i"
%include "common.i"

%import "QtAbstractSpinBox.i"

%{
#include <QtGui/qspinbox.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtAbstractSpinBox IMPORT QAbstractSpinBox;

TYPE
  T = QSpinBox;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget%}


%include <QtGui/qspinbox.h>
