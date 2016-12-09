%module QtRect

%include "m3qt.i"
%include "common.i"

%{
#include <QtCore/qrect.h>
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{
TYPE
  T = QRect;
%}

//rename out main constructor
%rename(init) QRect::QRect(int,int,int,int);

%apply  intvar* {int *x1, int *y1, int *x2, int *y2};
%apply  intvar* {int *x, int *y, int *w, int *h};

//the operators
%rename(Or)         QRect::operator|;
%rename(And)        QRect::operator&;
%rename(OrEqual)    QRect::operator|=;
%rename(AndEqual)   QRect::operator&=;

//the F ones
%rename(Or)         QRectF::operator|;
%rename(And)        QRectF::operator&;
%rename(OrEqual)    QRectF::operator|=;
%rename(AndEqual)   QRectF::operator&=;

%ignore operator==;
%ignore operator!=;

//Local classes
%apply ClassReturn    {QRect &};
%apply ClassReturn    {QRectF &};
%apply ClassReturn    {QRect};
%apply ClassReturn    {QRectF};

%apply ClassIn       {const QRect &};
%apply ClassIn       {const QRectF &};

DoType(QSize,QtSize)
DoType(QSizeF,QtSize)
DoType(QPoint,QtPoint)
DoType(QPointF,QtPoint)

%include <QtCore/qrect.h>
