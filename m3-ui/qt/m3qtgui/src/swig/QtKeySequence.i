%module QtKeySequence

%include "m3qt.i"
%include "common.i"

%{
#include <QtGui/qkeysequence.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{

TYPE
  T = QKeySequence;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtString IMPORT QString;
FROM QtByteArray IMPORT QByteArray;
%}

%ignore operator int;
%ignore operator [];
%ignore operator =;
%ignore operator ==;
%ignore operator !=;
%ignore operator <;
%ignore operator >;
%ignore operator <=;
%ignore operator >=;
%ignore operator <<;
%ignore operator >>;
%ignore operator QString;
%ignore operator QVariant;

%ignore data_ptr;

//qlist but fixme
%ignore keyBindings;

//Local enums
EnumMaps(QKeySequence, StandardKey, ErrMode)
EnumMaps(QKeySequence, SequenceMatch, ErrMode)
EnumMaps(QKeySequence, SequenceFormat, ErrMode)

//local clases
%apply ClassIn     {QKeySequence &};
%apply ClassIn     {const QKeySequence &};
%apply ClassReturn {QKeySequence};

%include <QtGui/qkeysequence.h>
