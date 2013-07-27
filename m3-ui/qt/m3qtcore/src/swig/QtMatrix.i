%module QtMatrix

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

//move this to m3qtgui I think its there as well

%{
#include "qmatrix.h"
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{

TYPE
  T = QMatrix;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%ignore operator=;
%ignore operator+=;
%ignore operator+;
%ignore operator*;
%ignore operator*=;
%ignore operator>;
%ignore operator<;
%ignore operator>=;
%ignore operator<=;
%ignore operator[];
%ignore operator QVariant;

%ignore operator==;
%ignore operator!=;

%ignore map(const QPainterPath &);

//this ignores all the map methods even ones we might want
%ignore map;

//dont want to import the gui library
%ignore mapToPolygon;

%apply ClassIn {const QRect &};
%apply ClassIn {const QRectF &};

%apply ClassReturn {QRect};
%apply ClassReturn {QRectF};
//%apply ClassReturn {QPolygon};

%apply ClassIn {const QMatrix &};
%apply ClassReturn {QMatrix &};
%apply ClassReturn {QMatrix inverted};

%typemap("m3wrapintype:import")  const QRect & %{QtRect  QRect&QRectF%}

%include "qmatrix.h"
