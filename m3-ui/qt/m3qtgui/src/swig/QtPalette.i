%module QtPalette

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

%{
#include <QtGui/qpalette.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{

TYPE
  T = QPalette;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%ignore operator<<;
%ignore operator QVariant;

//rename some operators
%rename(Op_Brush_Assign)  QPalette::operator=;
%rename(Op_Brush_Equals)  QPalette::operator==;
%rename(Op_Brush_NotEquals)  QPalette::operator!=;

//Local enums
EnumMaps(QPalette, ColorGroup, ErrMode)
EnumMaps(QPalette, ColorRole, ErrMode)

%apply ClassIn     {const QPalette &};
%apply ClassReturn {QPalette};

DoType(QColor,QtColor)
DoType(QBrush,QtBrush)

//for assign operator
%apply SelfReturn {QPalette &};

%include <QtGui/qpalette.h>
