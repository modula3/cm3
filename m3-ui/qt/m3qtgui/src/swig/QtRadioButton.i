%module QtRadioButton

%include "m3qt.i"
%include "common.i"

%import "QtAbstractButton.i"

%{
#include <QtGui/qradiobutton.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtAbstractButton IMPORT QAbstractButton;
TYPE
  T = QRadioButton;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}


%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget%}

%include <QtGui/qradiobutton.h>
