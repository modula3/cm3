%module QtRubberBand

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qrubberband.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QRubberBand;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//Local enums
EnumMaps(QRubberBand, Shape, ErrMode)

DoType(QWidget,QtWidget);

%include <QtGui/qrubberband.h>
