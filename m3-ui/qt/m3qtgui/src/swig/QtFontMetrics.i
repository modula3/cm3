%module QtFontMetrics

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtGui/qfontmetrics.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QFontMetrics;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtString IMPORT QString;
FROM QtByteArray IMPORT QByteArray;
%}

%ignore operator=;
%ignore operator==;
%ignore operator!=;

%apply ClassIn {const QFontMetrics &};
%apply ClassIn {const QFontMetricsF &};

%apply  intarr* {int *tabarray};

DoType(QRect,QtRect)
DoType(QRectF,QtRect)
DoType(QSize,QtSize)
DoType(QSizeF,QtSize)
DoType(QFont,QtFont)

DoType(QChar,QGuiStubs)
DoType(QPaintDevice,QGuiStubs)

%include <QtGui/qfontmetrics.h>
