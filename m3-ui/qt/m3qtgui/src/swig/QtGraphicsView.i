%module QtGraphicsView

%include "m3qt.i"
%include "common.i"

%import "QtAbstractScrollArea.i"

%{
#include <QtGui/qgraphicsview.h>
#define  OptimizationFlags QGraphicsView::OptimizationFlags
#define  CacheMode QGraphicsView::CacheMode
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtAbstractScrollArea IMPORT QAbstractScrollArea;
TYPE
  T = QGraphicsView;

  OptimizationFlags = INTEGER;
  CacheMode = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}


//Qlist
%ignore items;
%ignore updateScene;

//Local enums
EnumMaps(QGraphicsView, ViewportAnchor, ErrMode)
EnumMaps(QGraphicsView, CacheModeFlag, ErrMode)
EnumMaps(QGraphicsView, DragMode, ErrMode)
EnumMaps(QGraphicsView, ViewportUpdateMode, ErrMode)
EnumMaps(QGraphicsView, OptimizationFlag, ErrMode)


//Local flags
EnumFlags(QGraphicsView::CacheMode, CacheMode)
EnumFlags(CacheMode, CacheMode)
EnumFlags(QGraphicsView::OptimizationFlags, OptimizationFlags)
EnumFlags(OptimizationFlags, OptimizationFlags)

//Remote enums
//these would normally be in the qpainter module and be imported
//EnumMaps(QPainter::RenderHint, RenderHint,1)
//EnumMaps(QPainter::RenderHints, RenderHints,1)

EnumMaps(QPainter, RenderHint, ErrMode)
EnumMaps(QPainter, RenderHints, ErrMode)

//EnumImport(QPainter::RenderHint, RenderHint, QGuiStubs)
//EnumImport(QPainter::RenderHints, RenderHints, QGuiStubs)

//check this assume proper enum
EnumImport(QPainter, RenderHint, QGuiStubs, 0)
EnumImport(QPainter, RenderHints, QGuiStubs, 0)

//normally in qgraphicsscene
EnumMaps(QGraphicsScene, SceneLayers, ErrMode)
//check this assume proper enum
EnumImport(QGraphicsScene, SceneLayers, QGuiStubs, 0)


DoType(QPointF,QtPoint)
DoType(QRect,QtRect)
DoType(QRectF,QtRect)
DoType(QPolygon,QtPolygon)
DoType(QPolygonF,QtPolygon)
DoType(QMatrix,QtMatrix)
DoType(QTransform,QtTransform)
DoType(QBrush,QtBrush)

DoType(QPainterPath,QGuiStubs)
DoType(QGraphicsItem,QGuiStubs)
DoType(QGraphicsScene,QGuiStubs)


%include <QtGui/qgraphicsview.h>
