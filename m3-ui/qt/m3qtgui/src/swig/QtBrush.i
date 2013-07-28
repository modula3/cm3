%module QtBrush

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

%{
#include <QtGui/qbrush.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QBrush;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//rename some operators
%rename(Op_Brush_Assign)  QBrush::operator=;
%rename(Op_Brush_Equals)  QBrush::operator==;
%rename(Op_Grad_Equals)  QGradient::operator==;
%rename(Op_Brush_NotEquals)  QBrush::operator!=;
%rename(OpGrad_NotEquals)  QGradient::operator!=;

%ignore operator QVariant;

%ignore QBrushData;
%ignore data_ptr;

//friend private
%ignore qHasPixmapTexture;

//Local enums
EnumMaps(QGradient, Spread, ErrMode)
EnumMaps(QGradient, Type, ErrMode)
EnumMaps(QGradient, CoordinateMode, ErrMode)
EnumMaps(QGradient, InterpolationMode, ErrMode)

//this file has brush and gradient classes
%apply ClassIn {QBrush &};
%apply ClassIn {const QBrush &};
%apply ClassIn {const QGradient &};
%apply ClassReturn {const QGradient *};

//for assign operator
%apply SelfReturn {QBrush &};

DoType(QPointF,QtPoint)
DoType(QColor,QtColor)
DoType(QImage,QtImage)
DoType(QPixmap,QtPixmap)
DoType(QMatrix,QtMatrix)
DoType(QTransform,QtTransform)

DoType(QGradientStops,QGuiStubs)

%include <QtGui/qbrush.h>
