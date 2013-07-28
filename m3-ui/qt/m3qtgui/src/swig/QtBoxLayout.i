%module QtBoxLayout

%include "m3qt.i"
%include "common.i"

%import "QtLayoutItem.i"
%import "QtLayout.i"

%{
#include <QtGui/qboxlayout.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QBoxLayout;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//Local enums
EnumMaps(QBoxLayout, Direction, ErrMode)

DoType(QSpacerItem,QtLayoutItem)
DoType(QLayout,QtLayout)

%include <QtGui/qboxlayout.h>
