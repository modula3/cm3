%module QtTabWidget

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qtabwidget.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QTabWidget;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

//Local enums
EnumMaps(QTabWidget, TabPosition, ErrMode)
EnumMaps(QTabWidget, TabShape, ErrMode)

DoType(QWidget,QtWidget);

%include <QtGui/qtabwidget.h>
