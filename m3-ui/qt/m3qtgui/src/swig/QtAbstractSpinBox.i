%module QtAbstractSpinBox

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qabstractspinbox.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QAbstractSpinBox;

%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%ignore validate;

//Local enums
EnumMaps(QAbstractSpinBox, StepEnabledFlag, ErrMode)
EnumMaps(QAbstractSpinBox, ButtonSymbols, ErrMode)
EnumMaps(QAbstractSpinBox, CorrectionMode, ErrMode)

%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget%}

%typemap("m3wrapintype:import")  const QString & %{QtString QString,QtByteArray QByteArray%}

%include <QtGui/qabstractspinbox.h>
