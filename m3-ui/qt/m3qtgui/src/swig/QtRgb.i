%module QtRgb

%include "m3qt.i"
%include "common.i"


%{
#include <QtGui/qrgb.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{

TYPE
  T = QRgb;
  QRgb = INTEGER; (* or Word.T *)
%}

%insert(m3wrapimpl) %{
%}


%include <QtGui/qrgb.h>
