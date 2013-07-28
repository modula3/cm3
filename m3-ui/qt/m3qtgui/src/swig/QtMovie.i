%module QtMovie

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtGui/qmovie.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QMovie;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtString IMPORT QString;
%}

//template
%ignore supportedFormats;

//cxx struct problems to do with qcolor fixme
%ignore backgroundColor;
%ignore currentPixmap;

//local enums
EnumMaps(QMovie, MovieState, ErrMode)
EnumMaps(QMovie, CacheMode, ErrMode)

%typemap("m3wrapintype:import")   QObject *    %{QtObject $1_basetype%}

DoType(QColor,QtColor);
DoType(QPicture,QtPicture);
DoType(QSize,QtSize);
DoType(QRect,QtRect);
DoType(QImage,QtImage);
DoType(QPixmap,QtPixmap);
DoType(QByteArray,QtByteArray);

DoType(QIODevice,QGuiStubs);

%include <QtGui/qmovie.h>