%module QtFont

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

%{
#include <QtGui/qfont.h>
#include <QtCore/qstringlist.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QFont;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
FROM QtString IMPORT QString;
%}

//rename some operators
%rename(Op_Assign)  QFont::operator=;
%rename(Op_Equals)  QFont::operator==;
%rename(Op_NotEquals)  QFont::operator!=;
%rename(Op_LessThan)  QFont::operator<;

%ignore operator QVariant;

//dont need a handle yet
%ignore handle;

//Local enums
EnumMaps(QFont, StyleHint, ErrMode)
EnumMaps(QFont, StyleStrategy, ErrMode)
EnumMaps(QFont, Weight, ErrMode)
EnumMaps(QFont, Style, ErrMode)
EnumMaps(QFont, Stretch, ErrMode)
EnumMaps(QFont, Capitalization, ErrMode)
EnumMaps(QFont, SpacingType, ErrMode)
EnumMaps(QFont, ResolveProperties, ErrMode)
EnumMaps(QFont, HintingPreference, ErrMode)

%apply ClassIn {const QFont &};
%apply ClassReturn {QFont};

//for assign operator
%apply SelfReturn {QFont &};

DoType(QStringList,QtStringList)
DoType(QPaintDevice,QGuiStubs)



%include <QtGui/qfont.h>
