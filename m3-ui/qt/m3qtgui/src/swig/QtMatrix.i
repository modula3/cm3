%module QtMatrix

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

%{
#include <QtGui/qmatrix.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QMatrix;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
%}

//%ignore operator=;
//%ignore operator*=;
%ignore operator*;

%ignore operator QVariant;

//rename some operators
%rename(Op_Assign)  QMatrix::operator=;
%rename(Op_Equals)  QMatrix::operator==;
%rename(Op_NotEquals)  QMatrix::operator!=;
%rename(Op_TimesEquals)  QMatrix::operator*=;
//%rename(Op_MatrixTimes)  QMatrix::operator*;

//the QPainterPath generates cxx errors about forward structs
%ignore map(const QPainterPath &p) const;

//causes link errors inline problem??
%ignore qFuzzyCompare;

//Local classes
%apply ClassIn {const QMatrix &};
%apply ClassReturn {QMatrix &};
%apply ClassReturn {QMatrix inverted};

%apply  intvar* {int *tx, int *ty};

DoType(QPoint,QtPoint)
DoType(QPointF,QtPoint)
DoType(QLine,QtLine)
DoType(QLineF,QtLine)
DoType(QRegion,QtRegion)
DoType(QRect,QtRect)
DoType(QRectF,QtRect)
DoType(QPolygon,QtPolygon)
DoType(QPolygonF,QtPolygon)

//DoType(QPainterPath,QGuiStubs)


%include <QtGui/qmatrix.h>
