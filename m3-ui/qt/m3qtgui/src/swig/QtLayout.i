%module QtLayout

%include "m3qt.i"
%include "common.i"

//imports layoutitem and qobject

%import "QtLayoutItem.i"
%import "QtObject.i"


/*
this class is a multi inheritance and so qlayoutitem and qobject
are both imported and the virtual methods get dragged in appropriately.
*/

%{
#include <QtGui/qlayout.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtObject IMPORT QObject;
TYPE
  T = QLayout;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
%}

//Local enums
EnumMaps(QLayout, SizeConstraint, ErrMode)

//fixes the setalignment l qlayout parm
%apply ClassIn  {QLayout *l};

%apply  intvar* {int *left, int *top, int *right, int *bottom};

DoType(QRect,QtRect)
DoType(QSize,QtSize)
DoType(QMargins,QtMargins);
DoType(QLayoutItem,QtLayoutItem)
DoType(QWidget,QtWidget)

%include <QtGui/qlayout.h>
