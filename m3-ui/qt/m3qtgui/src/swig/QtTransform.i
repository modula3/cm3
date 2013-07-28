%module QtTransform

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

%{
#include <QtGui/qtransform.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{

TYPE
  T = QTransform;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
%}

//rename the result parm in SquareToQuad and others as it shadows a parm in
//the generated code
#define result transformResult

//%ignore operator=;
//%ignore operator+=;
//%ignore operator-=;
//%ignore operator/=;
%ignore operator+;
%ignore operator-;
%ignore operator*;
//%ignore operator*=;
%ignore operator/;
%ignore operator>;
%ignore operator<;
%ignore operator>=;
%ignore operator<=;
%ignore operator[];
%ignore operator QVariant;

//%ignore operator==;
//%ignore operator!=;

//causes link errors inline problem??
%ignore qFuzzyCompare;

//rename some operators
%rename(Op_Assign)  QTransform::operator=;
%rename(Op_Equals)  QTransform::operator==;
%rename(Op_NotEquals)  QTransform::operator!=;
%rename(Op_TimesEquals)   QTransform::operator*=;
%rename(Op_DivEquals)   QTransform::operator/=;
//times causes errors overloaded
//%rename(Op_Times)   QTransform::operator*;
%rename(Op_PlusEquals)   QTransform::operator+=;
%rename(Op_MinusEquals)   QTransform::operator-=;

//Local enums
EnumMaps(QTransform, TransformationType, ErrMode)

//Local classes
%apply ClassIn {QTransform &};
%apply SelfReturn {QTransform &};
%apply ClassReturn {QTransform};

%apply  intvar* {int *tx, int *ty};

DoType(QMatrix,QtMatrix)
DoType(QPoint,QtPoint)
DoType(QPointF,QtPoint)
DoType(QLine,QtLine)
DoType(QLineF,QtLine)
DoType(QRegion,QtRegion)
DoType(QRect,QtRect)
DoType(QRectF,QtRect)
DoType(QPolygon,QtPolygon)
DoType(QPolygonF,QtPolygon)

DoType(QPainterPath,QGuiStubs)


%include <QtGui/qtransform.h>
