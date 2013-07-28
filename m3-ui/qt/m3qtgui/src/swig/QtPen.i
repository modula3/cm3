%module QtPen

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtGui/qpen.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QPen;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%ignore operator=;
%ignore operator==;
%ignore operator!=;
%ignore operator>>;
%ignore operator<<;
%ignore operator QVariant;

//vector
%ignore dashPattern;
%ignore setDashPattern;

%ignore data_ptr;

//local classes
%apply ClassIn {QPen &};
%apply ClassIn {const QPen &};

DoType(QColor,QtColor)
DoType(QBrush,QtBrush)

%include <QtGui/qpen.h>
