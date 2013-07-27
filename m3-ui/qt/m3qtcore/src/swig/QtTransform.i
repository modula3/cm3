%module QtTransform

%include "m3qt.i"
%include "common.i"

%{
#include "qtransform.h"
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{

TYPE
  T = QTransform;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%include "qtransform.h" 
