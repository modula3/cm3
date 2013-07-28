%module QtIcon

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtGui/qicon.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{

TYPE
  T = QIcon;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
FROM QtString IMPORT QString;
%}

//qvariant problem
%ignore operator QVariant;
%ignore operator =;

//QList
%ignore availableSizes;

//problem
%ignore data_ptr;

//Local enums
EnumMaps(QIcon, Mode, ErrMode)
EnumMaps(QIcon, State, ErrMode)

//Local classes
%apply ClassIn       {QIcon &};
%apply ClassIn       {const QIcon &};
%apply ClassReturn   {QIcon};

DoType(QPainter,QGuiStubs)
DoType(QIconEngine,QGuiStubs)
DoType(QIconEngineV2,QGuiStubs)

DoType(QSize,QtSize)
DoType(QRect,QtRect)
DoType(QPixmap,QtPixmap)
DoType(QStringList,QtStringList)


%include <QtGui/qicon.h>
