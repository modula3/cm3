%module QtFrame

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qframe.h>
%}


%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QFrame;

%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//Local Enums
EnumMaps(QFrame, Shadow, ErrMode)
EnumMaps(QFrame, Shape, ErrMode)
EnumMaps(QFrame, StyleMask, ErrMode)

DoType(QWidget,QtWidget)

%include <QtGui/qframe.h>
