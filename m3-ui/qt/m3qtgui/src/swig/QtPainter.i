%module QtPainter

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtGui/qpainter.h>
#define RenderHints QPainter::RenderHints
#define PixmapFragmentHints QPainter::PixmapFragmentHints
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{

TYPE
  T = QPainter;
  RenderHints = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%ignore drawLines;
%ignore drawRects;
%ignore drawPixmapFragments;

//painterpath
%ignore clipPath;
%ignore setClipPath;
%ignore strokePath;
%ignore fillPath;
%ignore drawPath;

//textoption
%ignore drawText;
%ignore boundingRect;

//statictext
%ignore drawStaticText;

//glyphrun
%ignore drawGlyphRun;

//Local Enums
EnumMaps(QPainter, RenderHint, ErrMode)
EnumMaps(QPainter, PixmapFragmentHint, ErrMode)
EnumMaps(QPainter, CompositionMode, ErrMode)

//Local Flags
EnumFlags(RenderHints, RenderHints)
EnumFlags(QPainter::RenderHints, RenderHints)
EnumFlags(PixmapFragmentHints, PixmapFragmentHints)
EnumFlags(QPainter::PixmapFragmentHints, PixmapFragmentHints)

DoType(QSize,QtSize)
DoType(QSizeF,QtSize)
DoType(QPoint,QtPoint)
DoType(QPointF,QtPoint)
DoType(QLine,QtLine)
DoType(QLineF,QtLine)
DoType(QRect,QtRect)
DoType(QRectF,QtRect)
DoType(QPolygon,QtPolygon)
DoType(QPolygonF,QtPolygon)
DoType(QRegion,QtRegion)
DoType(QFont,QtFont)
DoType(QPen,QtPen)
DoType(QBrush,QtBrush)
DoType(QPicture,QtPicture)
DoType(QPixmap,QtPixmap)
DoType(QColor,QtColor)
DoType(QImage,QtImage)
DoType(QMatrix,QtMatrix)
DoType(QTransform,QtTransform)
DoType(QPaintDevice,QtPaintDevice)
DoType(QFontInfo,QtFontInfo)
DoType(QFontMetrics,QtFontMetrics)
DoType(QPaintEngine,QtPaintEngine)
DoType(QTextItem,QtPaintEngine)
DoType(QWidget,QtWidget)

%include <QtGui/qpainter.h> 
