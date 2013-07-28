%module QtStackedLayout

%include "m3qt.i"
%include "common.i"

%import "QtLayoutItem.i"
%import "QtLayout.i"

%{
#include <QtGui/qstackedlayout.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QStackedLayout;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//Local enums
EnumMaps(QStackedLayout, StackingMode, ErrMode)

DoType(QLayout,QtLayout)

%include <QtGui/qstackedlayout.h>
