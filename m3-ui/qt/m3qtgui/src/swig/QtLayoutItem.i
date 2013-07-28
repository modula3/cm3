%module QtLayoutItem

%include "m3qt.i"
%include "common.i"

%{
#include <QtGui/qlayoutitem.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QLayoutItem;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;

FROM QtLayout IMPORT QLayout;
%}


//remote enums
EnumMaps(QSizePolicy, Policy, ErrMode)
EnumImport(QSizePolicy, Policy, QtSizePolicy, ErrMode)

//remote flags
EnumFlags(Qt::Orientations, Orientations)
EnumFlagsImport(Qt::Orientations, Orientations,QtNamespace)
EnumFlags(Qt::Alignment, AlignmentFlag)
EnumFlagsImport(Qt::Alignment, AlignmentFlag,QtNamespace)
EnumFlags(QSizePolicy::ControlTypes, ControlTypes)
EnumFlagsImport(QSizePolicy::ControlTypes , ControlTypes, QtSizePolicy)


//we dont import QLayout in the interface  - circular imports -
//but we do in the implementation see the import above

%apply ClassReturn  {QLayout *};

%typemap("m3wrapouttype") QLayout *   %{REFANY%}
%typemap("m3wraprettype") QLayout *   %{REFANY%}

DoType(QRect,QtRect)
DoType(QSize,QtSize)
DoType(QWidget,QtWidget)

%include <QtGui/qlayoutitem.h>
