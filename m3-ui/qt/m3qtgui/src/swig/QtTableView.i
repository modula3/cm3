%module QtTableView

%include "m3qt.i"
%include "common.i"

%import "QtAbstractItemView.i"

%{
#include <QtGui/qtableview.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtAbstractItemView IMPORT QAbstractItemView;

TYPE
  T = QTableView;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//remote enums
EnumMaps(QAbstractItemView, ScrollHint, ErrMode)
EnumImport(QAbstractItemView, ScrollHint, QtAbstractItemView, 0)

DoType(QHeaderView,QtHeaderView)
DoType(QAbstractItemModel,QtAbstractItemModel)
DoType(QModelIndex,QtAbstractItemModel)
DoType(QWidget,QtWidget)

DoType(QScrollHint,QGuiStubs)
DoType(QScrollHint,QGuiStubs)

%include <QtGui/qtableview.h>
