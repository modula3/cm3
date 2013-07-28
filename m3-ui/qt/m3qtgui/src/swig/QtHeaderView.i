%module QtHeaderView

%include "m3qt.i"
%include "common.i"

%import "QtAbstractItemView.i"

%{
#include <QtGui/qheaderview.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtAbstractItemView IMPORT QAbstractItemView;

TYPE
  T = QHeaderView;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//Local enums
EnumMaps(QHeaderView, ResizeMode, ErrMode)

%include <QtGui/qheaderview.h>
