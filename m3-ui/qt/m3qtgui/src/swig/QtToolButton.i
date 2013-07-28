%module QtToolButton

%include "m3qt.i"
%include "common.i"

%import "QtAbstractButton.i"

%{
#include <QtGui/qtoolbutton.h>
%}

%insert(m3rawintf) %{
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{
FROM QtAbstractButton IMPORT QAbstractButton;
TYPE
  T = QToolButton;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//Local enums
EnumMaps(QToolButton, ToolButtonPopupMode, ErrMode)

%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget%}

DoType(QMenu,QtMenu)

%include <QtGui/qtoolbutton.h>
