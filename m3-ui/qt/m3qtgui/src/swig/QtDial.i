%module QtDial

%include "m3qt.i"
%include "common.i"

%import "QtAbstractSlider.i"

%{
#include <QtGui/qdial.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtAbstractSlider IMPORT QAbstractSlider;
TYPE
  T = QDial;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}


%include <QtGui/qdial.h>
