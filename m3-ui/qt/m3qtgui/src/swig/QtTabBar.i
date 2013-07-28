%module QtTabBar

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qtabbar.h>
#include <QtGui/qicon.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QTabBar;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

//qvariant
%ignore setTabData;
%ignore tabData;

//Local enums
EnumMaps(QTabBar, Shape, ErrMode)
EnumMaps(QTabBar, ButtonPosition, ErrMode)
EnumMaps(QTabBar, SelectionBehavior, ErrMode)

DoType(QWidget,QtWidget);

%include <QtGui/qtabbar.h>
