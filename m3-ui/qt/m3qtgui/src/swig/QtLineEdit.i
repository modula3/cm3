%module QtLineEdit

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qlineedit.h>
%}

%insert(m3rawintf) %{
%}


%insert(m3wrapintf) %{
TYPE
  T = QLineEdit;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
FROM QtByteArray IMPORT QByteArray;
%}

//Local Enums
EnumMaps(QLineEdit, EchoMode, ErrMode)

//remote enums
//EnumMaps(QTabWidget, TabShape, ErrMode)
//EnumImport(QTabWidget, TabPosition&TabShape, QtTabWidget, ErrMode)

%apply  intvar* {int *left, int *top, int *right, int *bottom};

DoType(QMenu,QtMenu)
DoType(QValidator,QGuiStubs)
DoType(QCompleter,QGuiStubs)

DoType(QWidget,QtWidget)

%include <QtGui/qlineedit.h>
