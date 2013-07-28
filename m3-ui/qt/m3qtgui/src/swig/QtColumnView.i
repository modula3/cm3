%module QtColumnView

%include "m3qt.i"
%include "common.i"

%import "QtAbstractItemView.i"

%{
#include <QtGui/qcolumnview.h>
%}

%insert(m3rawintf) %{
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{
FROM QtAbstractItemView IMPORT QAbstractItemView;

TYPE
  T = QColumnView;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//QList
%ignore setColumnWidths;
%ignore columnWidths;

//remote enums
EnumMaps(QAbstractItemView, ScrollHint, ErrMode)
EnumImport(QAbstractItemView, ScrollHint, QtAbstractItemView, 0)

DoType(QWidget,QtWidget)

%include <QtGui/qcolumnview.h>
