%module QtTextFormat

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include  <QtGui/qtextformat.h>
#define PageBreakFlags QTextFormat::PageBreakFlags
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QTextFormat;
  PageBreakFlags = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
FROM QtString IMPORT QString;
%}

%ignore operator=;
%ignore operator+=;
%ignore operator+;
%ignore operator>;
%ignore operator<;
%ignore operator>=;
%ignore operator<=;
%ignore operator[];
%ignore operator>>;
%ignore operator<<;

%ignore operator==;
%ignore operator!=;
%ignore operator+;
%ignore operator-;
%ignore operator*;
%ignore operator*;
%ignore operator/;

%ignore operator QVariant;

//vector
%ignore lengthVectorProperty;
%ignore properties;
%ignore setTabPositions;
%ignore tabPositions;
%ignore setColumnWidthConstraints;
%ignore columnWidthConstraints;

//clash enums
%rename("TextFormat_LayoutDirection") QTextFormat::LayoutDirection;

//Local Enums
EnumMaps(QTextLength, Type, ErrMode)
EnumMaps(QTextFormat, FormatType, ErrMode)
EnumMaps(QTextFormat, Property, ErrMode)
EnumMaps(QTextFormat, ObjectTypes, ErrMode)
EnumMaps(QTextFormat, PageBreakFlag, ErrMode)

EnumMaps(QTextCharFormat, VerticalAlignment, ErrMode)
EnumMaps(QTextCharFormat, UnderlineStyle, ErrMode)
EnumMaps(QTextListFormat, Style, ErrMode)
EnumMaps(QTextFrameFormat, Position, 0)
EnumMaps(QTextFrameFormat, BorderStyle, ErrMode)

//this enum not specified in any method so not used as such
//EnumMaps(QTextBlockFormat, LineHeightTypes, ErrMode)

//remote enums
EnumMaps(QFont, Capitalization, 0)
EnumMaps(QFont, StyleHint, ErrMode)
EnumMaps(QFont, StyleStrategy, ErrMode)
EnumMaps(QFont, HintingPreference, ErrMode)

EnumImport(QFont, Capitalization, QtFont, 0)
EnumImport(QFont, StyleHint, QtFont, ErrMode)
EnumImport(QFont, StyleStrategy, QtFont, ErrMode)
EnumImport(QFont, HintingPreference, QtFont, ErrMode)

//Local flags
EnumFlags(PageBreakFlags, PageBreakFlags)

%apply ClassIn {const QTextFormat &};
%apply ClassIn {const QTextLength &};
%apply ClassReturn {QTextLength lengthProperty};
%apply ClassReturn {QTextLength width};
%apply ClassReturn {QTextLength height};
%apply ClassReturn {QTextBlockFormat toBlockFormat};
%apply ClassReturn {QTextCharFormat toCharFormat};
%apply ClassReturn {QTextListFormat toListFormat};
%apply ClassReturn {QTextTableFormat toTableFormat};
%apply ClassReturn {QTextFrameFormat toFrameFormat};
%apply ClassReturn {QTextImageFormat toImageFormat};
%apply ClassReturn {QTextTableCellFormat toTableCellFormat};


DoType(QFont,QtFont)
DoType(QColor,QtColor)
DoType(QBrush,QtBrush)
DoType(QPen,QtPen)
DoType(QStringList,QtStringList)

%include  <QtGui/qtextformat.h>
