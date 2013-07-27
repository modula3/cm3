%module QtPoint

%include "m3qt.i"
%include "common.i"

%{
#include <QtCore/qpoint.h>
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{
TYPE
  T = QPoint;
%}


%rename(PlusEqual)     QPoint::operator+=;
%rename(MinusEqual)    QPoint::operator-=;
%rename(MultiplyEqual) QPoint::operator*=;
%rename(DivideEqual)   QPoint::operator/=;

%rename(PlusEqual)     QPointF::operator+=;
%rename(MinusEqual)    QPointF::operator-=;
%rename(MultiplyEqual) QPointF::operator*=;
%rename(DivideEqual)   QPointF::operator/=;

%ignore operator==;
%ignore operator!=;
%ignore operator+;
%ignore operator-;
%ignore operator*;
%ignore operator/;


//Local classes
%apply ClassIn     {const QPoint &};
%apply ClassIn     {const QPointF &};
%apply SelfReturn  {QPoint &};
%apply SelfReturn  {QPointF &};
%apply ClassReturn {QPoint};

%include <QtCore/qpoint.h>
