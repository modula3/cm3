%module QtBitmap

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"
%import "QtPixmap.i"

%{
#include <QtGui/qbitmap.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{

TYPE
  T = QBitmap;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%ignore operator=;
%ignore operator QVariant;

//conflicts with fromImage in Pixmap so rename
%rename(FromBitmapImage) fromImage;

//remote enums
EnumMaps(QImage, Format, ErrMode)
//proper enum
EnumImport(QImage, Format, QtImage, 0)

//Local classes
%apply ClassIn      {QBitmap &};
%apply ClassIn      {const QBitmap &};
%apply SelfReturn   {QBitmap};

%apply ClassReturn   {QBitmap QBitmap::fromImage};
%apply ClassReturn   {QBitmap QBitmap::fromData};

%typemap("m3wrapintype:import")  const QPixmap &  %{QtPixmap $1_basetype%}


%include <QtGui/qbitmap.h>
