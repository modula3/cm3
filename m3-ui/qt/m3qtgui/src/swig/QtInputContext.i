%module QtInputContext

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtGui/qinputcontext.h>
#include <QtGui/qtextformat.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtObject IMPORT QObject;
TYPE
  T = QInputContext;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//vector
%ignore actions;

//Local Enums
EnumMaps(QInputContext, StandardFormat, ErrMode)

DoType(QTextFormat,QtTextFormat)
DoType(QFont,QtFont)
DoType(QWidget,QtWidget)
DoType(QMouseEvent,QGuiStubs)
DoType(QInputMethodEvent,QGuiStubs)

%include <QtGui/qinputcontext.h>
