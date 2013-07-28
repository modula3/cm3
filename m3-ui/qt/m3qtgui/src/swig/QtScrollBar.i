%module QtScrollBar

%include "m3qt.i"
%include "common.i"

%import "QtAbstractSlider.i"

%{
#include <QtGui/qscrollbar.h>
%}

%insert(m3rawintf) %{
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{
FROM QtAbstractSlider IMPORT QAbstractSlider;

TYPE
  T = QScrollBar;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%ignore qt_qscrollbarStyleOption;

%apply ClassIn {QStyleOptionSlider *};

%include <QtGui/qscrollbar.h>
