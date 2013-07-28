%module QtPaintEngine

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtGui/qpaintengine.h>
#define RenderFlags QTextItem::RenderFlags
#define PaintEngineFeatures QPaintEngine::PaintEngineFeatures
#define DirtyFlags QPaintEngine::DirtyFlags
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QPaintEngine;
  RenderFlags = INTEGER;
  DirtyFlags = INTEGER;
  PaintEngineFeatures = INTEGER;  
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
FROM QtString IMPORT QString;
FROM QtByteArray IMPORT QByteArray;
%}

//fixme the TextItem class has no constructor yet is passed to drawTextItem
//how do we manage this??
%nodefaultctor;

//qpainter
%ignore renderHints;
%ignore compositionMode;
%ignore painter;
//qpainterpath
%ignore drawPath;
%ignore clipPath;

//constructor problem
%ignore drawTextItem;

%apply  intvar* {int *x, int *y, int *w, int *h};

//Local Enums
EnumMaps(QTextItem, RenderFlag, ErrMode)
EnumMaps(QPaintEngine, PaintEngineFeature, ErrMode)
EnumMaps(QPaintEngine, DirtyFlag, ErrMode)
EnumMaps(QPaintEngine, PolygonDrawMode, ErrMode)
EnumMaps(QPaintEngine, Type, ErrMode)

//Local flags
EnumFlags(RenderFlags, RenderFlags)
EnumFlags(QTextItem::RenderFlags, RenderFlags)
EnumFlags(DirtyFlags, DirtyFlags)
EnumFlags(QPaintEngine::DirtyFlags, DirtyFlags)
EnumFlags(PaintEngineFeatures, PaintEngineFeatures)
EnumFlags(QPaintEngine::PaintEngineFeatures, PaintEngineFeatures)

DoType(QSize,QtSize)
DoType(QSizeF,QtSize)
DoType(QPoint,QtPoint)
DoType(QPointF,QtPoint)
DoType(QLine,QtLine)
DoType(QLineF,QtLine)
DoType(QRect,QtRect)
DoType(QRectF,QtRect)
DoType(QRegion,QtRegion)
DoType(QFont,QtFont)
DoType(QPen,QtPen)
DoType(QBrush,QtBrush)
DoType(QPixmap,QtPixmap)
DoType(QImage,QtImage)
DoType(QMatrix,QtMatrix)
DoType(QTransform,QtTransform)
DoType(QPaintDevice,QtPaintDevice)


%include <QtGui/qpaintengine.h> 
