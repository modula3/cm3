%module QtTextEdit

%include "m3qt.i"
%include "common.i"

%import "QtAbstractScrollArea.i"

%{
#include <QtGui/qtextedit.h>
#define  AutoFormatting QTextEdit::AutoFormatting
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtAbstractScrollArea IMPORT QAbstractScrollArea;

TYPE
  T = QTextEdit;
  AutoFormatting = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}


//fixme QList
%ignore ExtraSelection;
%ignore setExtraSelections;
%ignore extraSelections;

//fix qvariant
%ignore loadResource;

//fix qprint
%ignore print;

//Local enums
EnumMaps(QTextEdit, LineWrapMode, ErrMode)
EnumMaps(QTextEdit, AutoFormattingFlag, ErrMode)

//local flags
EnumFlags(QTextEdit::AutoFormatting, AutoFormatting)
EnumFlags(AutoFormatting, AutoFormatting)

//remote enums fix the enumimport macro has changed
EnumMaps(QTextOption, WrapMode, ErrMode)
//EnumImport(QTextOption, WrapMode, QtTextOption, ErrMode)

EnumMaps(QTextDocument, FindFlags, ErrMode)
//EnumImport(QTextDocument, FindFlags, QtTextDocument, ErrMode)

EnumMaps(QTextCursor, MoveOperation, ErrMode)
EnumMaps(QTextCursor, MoveMode, ErrMode)
EnumMaps(QTextCursor, MoveAnchor, ErrMode)
//EnumImport(QTextCursor, MoveOperation&MoveMode&MoveAnchor, QtTextCursor, ErrMode)


//make these dotype
//DoType(QTextDocument, QtTextDocument)
%apply ClassIn     {QTextDocument *};
%apply ClassReturn {QTextDocument *};

//DoType(QTextDocument, QtTextDocument)
%apply ClassIn     {const QTextCursor &};
%apply ClassReturn {QTextCursor};

DoType(QTextCharFormat, QtTextFormat)

%apply ClassIn     {QWidget *};

%typemap("m3wrapintype:import")  QWidget *      %{QtWidget QWidget%}


%typemap("m3wrapretvar:import")  QTextDocument * %{QGuiStubs WrapMode&FindFlags&MoveOperation&MoveMode&QTextDocument&QTextCursor%}


/*
DoType(QTextDocument,QGuiStubs);
DoType(QTextCursor,QGuiStubs);
DoType(QTextCharFormat,QGuiStubs);
*/

//these in parent include
//DoType(QColor,QtColor);
//DoType(QFont,QtFont);
DoType(QMenu,QtMenu);

%include <QtGui/qtextedit.h>
