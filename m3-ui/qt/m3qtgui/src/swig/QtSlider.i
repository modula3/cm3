%module QtSlider

%include "m3qt.i"
%include "common.i"

%import "QtAbstractSlider.i"

%{
#include <QtGui/qslider.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{

FROM QtAbstractSlider IMPORT QAbstractSlider;

TYPE
  T = QSlider;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//fix when add sliderstyleoption class
%ignore qt_qsliderStyleOption;

//Local enums
EnumMaps(QSlider, TickPosition, ErrMode)

%include <QtGui/qslider.h>
