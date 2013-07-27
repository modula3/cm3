%module QtQueue

%include "m3qt.i"
%include "common.i"

%{
#include "qqueue.h"
%}


%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{

%}


//raw return typemaps for external classes
%typemap("m3rawrettype")   QPoint     %{ADDRESS%}

%include "qqueue.h"

%template(intList) QQueue<int>;
