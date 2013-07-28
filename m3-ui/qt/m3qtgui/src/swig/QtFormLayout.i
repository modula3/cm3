%module QtFormLayout

%include "m3qt.i"
%include "common.i"

%import "QtLayout.i"

%{
#include <QtGui/qformlayout.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QFormLayout;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
FROM QtString IMPORT QString;
%}

//Local enums
EnumMaps(QFormLayout, FieldGrowthPolicy, ErrMode)
EnumMaps(QFormLayout, RowWrapPolicy, ErrMode)
EnumMaps(QFormLayout, ItemRole, ErrMode)

//Got some enum pointer parms need to add a couple of tmaps
%typemap("m3wrapintype") QFormLayout::ItemRole *   %{ItemRole%}
%typemap("m3rawintype")  QFormLayout::ItemRole *   %{C.int%}
%typemap("m3rawinmode")    QFormLayout::ItemRole * %{READONLY%}
%typemap("m3wrapinmode")   QFormLayout::ItemRole * %{VAR%}

%apply  intvar* {int *rowPtr};

DoType(QLayout,QtLayout)

%include <QtGui/qformlayout.h>
