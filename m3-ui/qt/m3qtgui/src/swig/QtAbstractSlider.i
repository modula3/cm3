%module QtAbstractSlider

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qabstractslider.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{

TYPE
  T = QAbstractSlider;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//Local enums
EnumMaps(QAbstractSlider, SliderAction, ErrMode)

%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget%}

%include <QtGui/qabstractslider.h>
