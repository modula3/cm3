%module QtFontInfo

%include "m3qt.i"
%include "common.i"

%{
#include <QtGui/qfontinfo.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QFontInfo;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtString IMPORT QString;
FROM QtByteArray IMPORT QByteArray;
%}

%ignore operator=;

//remote enums
EnumMaps(QFont, Style, ErrMode)
EnumMaps(QFont, StyleHint, ErrMode)
EnumImport(QFont, Style, QtFont, ErrMode)
EnumImport(QFont, StyleHint, QtFont, ErrMode)

%apply ClassIn {const QFontInfo &};

DoType(QFont,QtFont)

%include <QtGui/qfontinfo.h>
