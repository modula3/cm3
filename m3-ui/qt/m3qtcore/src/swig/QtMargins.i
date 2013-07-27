%module QtMargins

%include "m3qt.i"
%include "common.i"

%{
#include <QtCore/qmargins.h>
%}


%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%insert(m3wrapintf) %{

%}

%ignore operator==;
%ignore operator!=;


%include <QtCore/qmargins.h>