%module QtGridLayout

%include "m3qt.i"
%include "common.i"

%import "QtLayoutItem.i"
%import "QtLayout.i"

%{
#include <QtGui/qgridlayout.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QGridLayout;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
%}

//remote enums
EnumMaps(Qt, Corner, ErrMode)
EnumMaps(Qt, Orientation, ErrMode)
EnumImport(Qt, Corner, QtNamespace, ErrMode)
EnumImport(Qt, Orientation, QtNamespace, ErrMode)

%apply  intvar* {int *row, int *column, int *rowSpan, int *columnSpan};

DoType(QLayout,QtLayout)

%include <QtGui/qgridlayout.h>
