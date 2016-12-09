%module QtPixmap

%include "m3qt.i"
%include "common.i"

%import "QtPaintDevice.i"

%{
#include <QtGui/qpixmap.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtPaintDevice IMPORT QPaintDevice;
TYPE
  T = QPixmap;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

/* This module has some methods from QBitmap creating a circular import
  but QBitmap is a super class of this class QPixmap so redefine the QBitmap
  refs here to REFANY. Do the same with QWidget which is a high level class.
*/

%ignore mask;
%ignore createHeuristicMask;
%ignore createMaskFromColor;

//qt4 imagereader
%ignore fromImageReader;

//only construct from a file and ignore the pixmapData class
%ignore QPixmap::QPixmap(const char * const xpm[]);
%ignore QPixmap::QPixmap(QPixmapData *data);
%ignore pixmapData;

//this defines a function with function parms bit hard at moment
%ignore defineIOHandler;

//this causes link errors wierd
%ignore qt_pixmap_id;

%ignore operator=;
%ignore operator<<;
%ignore operator>>;
%ignore operator!;
%ignore operator QVariant;


//data_ptr funny one returns a pointer to something
%typemap("m3rawrettype")    QPixmap::DataPtr &    %{ADDRESS%}
%typemap("m3wraprettype")   QPixmap::DataPtr &    %{UNTRACED REF CHAR%}
%typemap("m3wrapouttype")   QPixmap::DataPtr &    %{UNTRACED REF CHAR%}

//for loaddata
%typemap("m3rawintype")   const unsigned char *    %{ADDRESS%}
%typemap("m3wrapintype")  const unsigned char *    %{UNTRACED REF CHAR%}
%typemap("m3wrapargvar")  const unsigned char *    %{$1tmp :=  LOOPHOLE($1_name,ADDRESS);%}
%typemap("m3wrapargraw") const unsigned char *     %{$1tmp%}

//Local classes
%apply ClassIn      {QPixmap &};
%apply ClassIn      {const QPixmap &};
%apply SelfReturn   {QPixmap};

//static method returns QImage
%apply ClassReturn {QPixmap QPixmap::grabWindow};
%apply ClassReturn {QPixmap QPixmap::grabWidget};
%apply ClassReturn {QPixmap QPixmap::fromImage};


//alias the qbitmap and qwidget classes

%typemap("m3rawintype")   const QBitmap &      %{REFANY%}
%typemap("m3wrapintype")  const QBitmap &      %{REFANY%}
%typemap("m3wrapinmode")  const QBitmap &      %{%}

%typemap("m3rawintype")   const QWidget *      %{REFANY%}
%typemap("m3wrapintype")  const QWidget *      %{REFANY%}
%typemap("m3wrapinmode")  const QWidget *      %{%}

%apply const QWidget * {QWidget *};

//dont import qwidget circular imports as with qbitmap
//%typemap("m3wrapintype:import")  QWidget *  %{QtWidget $1_basetype%}

%typemap("m3wrapintype:import")  const QString & %{QtString QString%}


DoType(QSize,QtSize)
DoType(QPoint,QtPoint)
DoType(QRect,QtRect)
DoType(QByteArray,QtByteArray)

DoType(QImage,QtImage)
DoType(QMatrix,QtMatrix)
DoType(QColor,QtColor)
DoType(QRegion,QtRegion)
DoType(QTransform,QtTransform)
DoType(QPaintDevice,QtPaintDevice)

//fix this circ imports
DoType(QPaintEngine,QGuiStubs)

DoType(QIODevice,QGuiStubs)
//maybe someday
//DoType(QPixmapData,QGuiStubs)

%include <QtGui/qpixmap.h>