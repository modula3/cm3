%module QtCursor

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtGui/qcursor.h>
#include <QtGui/qpixmap.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QCursor;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%ignore operator=;
%ignore operator QVariant;

%apply ClassIn {const QCursor &};

DoType(QPoint,QtPoint)
DoType(QBitmap,QtBitmap)
DoType(QPixmap,QtPixmap)

%include  <QtGui/qcursor.h>
