%module QtImage

%include "m3qt.i"
%include "common.i"

%import "QtPaintDevice.i"

%{
#include <QtGui/qimage.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtPaintDevice IMPORT QPaintDevice;
TYPE
  T = QImage;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

#define QT_NO_IMAGE_TEXT
#define QT_NO_IMAGEFORMAT_XPM

%ignore operator==;
%ignore operator!=;
%ignore operator=;
%ignore operator QVariant;

%ignore convertToFormat(Format f, const QVector<QRgb> &colorTable,Qt::ImageConversionFlags flags = Qt::AutoColor) const;

%ignore colorTable;
%ignore setColorTable;
%ignore qt_image_colortable;
%ignore qt_image_id;

//confilict with trueMatrix in Pixmap
%rename(Image_TrueMatrix) trueMatrix;
%rename(Image_FromData) fromData;

//Local enums
EnumMaps(QImage, InvertMode, ErrMode)
EnumMaps(QImage, Format, ErrMode)


//data_ptr funny one returns a pointer to something
%typemap("m3rawrettype")    QImage::DataPtr &    %{ADDRESS%}
%typemap("m3wraprettype")   QImage::DataPtr &    %{UNTRACED REF CHAR%}
%typemap("m3wrapouttype")   QImage::DataPtr &    %{UNTRACED REF CHAR%}

//local classes
%apply ClassIn    {QImage &};
%apply ClassIn    {const QImage &};
%apply SelfReturn {QImage};


DoType(QSize,QtSize)
DoType(QPoint,QtPoint)
DoType(QRect,QtRect)
DoType(QMatrix,QtMatrix)
DoType(QTransform,QtTransform)
DoType(QByteArray,QtByteArray)
DoType(QColor,QtColor)
//fix this circ imports
DoType(QPaintEngine,QGuiStubs)

DoType(QIODevice,QGuiStubs)


//specialised tmaps for Rgb to get the type - note its not a class
%typemap("m3rawintype")   QRgb  %{C.unsigned_int%}
%typemap("m3rawrettype")  QRgb  %{C.unsigned_int%}
%typemap("m3wrapintype")  QRgb  %{QtRgb.T%}
%typemap("m3wraprettype") QRgb  %{QtRgb.T%}
%typemap("m3wrapouttype") QRgb  %{QtRgb.T%}
%typemap("m3wrapouttype") QRgb  %{QtRgb.T%}
%typemap("ctype") QRgb     %{QRgb *%}
%typemap("m3wrapintype:import")  QRgb  %{QtRgb%}

%typemap("m3wrapintype:import")  const QString & %{QtString QString%}

%include <QtGui/qimage.h>
