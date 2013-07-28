%module QtMdiArea

%include "m3qt.i"
%include "common.i"

%import "QtAbstractScrollArea.i"

%{
#include <QtGui/qmdiarea.h>
%}

%insert(m3rawintf) %{
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{
FROM QtAbstractScrollArea IMPORT QAbstractScrollArea;
TYPE
  T = QMdiArea;

  AreaOptions = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//QList
%ignore subWindowList;

//Local enums
EnumMaps(QMdiArea, AreaOption, ErrMode)
EnumMaps(QMdiArea, WindowOrder, ErrMode)
EnumMaps(QMdiArea, ViewMode, ErrMode)

//Local flags
EnumFlags(QMdiArea::AreaOptions, AreaOptions)
EnumFlags(AreaOptions, AreaOptions)

//remote enums
EnumMaps(QTabWidget, TabShape, ErrMode)
EnumMaps(QTabWidget, TabPosition, ErrMode)

EnumImport(QTabWidget, TabShape, QtTabWidget, 0)
EnumImport(QTabWidget, TabPosition, QtTabWidget, 0)

DoType(QBrush,QtBrush)
DoType(QMdiSubWindow,QtMdiSubWindow)

%include <QtGui/qmdiarea.h>
