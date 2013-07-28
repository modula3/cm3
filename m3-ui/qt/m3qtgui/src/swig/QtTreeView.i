%module QtTreeView

%include "m3qt.i"
%include "common.i"

%import "QtAbstractItemView.i"

%{
#include <QtGui/qtreeview.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtAbstractItemView IMPORT QAbstractItemView;

TYPE
  T = QTreeView;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//remote enums
EnumMaps(QAbstractItemView, ScrollHint, ErrMode)
EnumImport(QAbstractItemView, ScrollHint, QtAbstractItemView, 0)

DoType(QHeaderView,QtHeaderView)
DoType(QWidget,QtWidget)

%include <QtGui/qtreeview.h>
