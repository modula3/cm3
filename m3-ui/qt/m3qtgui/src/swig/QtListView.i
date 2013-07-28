%module QtListView

%include "m3qt.i"
%include "common.i"

%import "QtAbstractItemView.i"

%{
#include <QtGui/qlistview.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtAbstractItemView IMPORT QAbstractItemView;

TYPE
  T = QListView;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//Local Enums
EnumMaps(QListView, Movement, ErrMode)
EnumMaps(QListView, Flow, ErrMode)
EnumMaps(QListView, ResizeMode, ErrMode)
EnumMaps(QListView, LayoutMode, ErrMode)
EnumMaps(QListView, ViewMode, ErrMode)

//remote Enums
EnumMaps(QAbstractItemView, ScrollHint, ErrMode)
EnumImport(QAbstractItemView, ScrollHint, QtAbstractItemView, 0)


%apply ClassIn {QWidget *};

%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget%}

%include <QtGui/qlistview.h>
