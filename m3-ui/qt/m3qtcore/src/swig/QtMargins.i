%module QtMargins

%include "m3qt.i"
%include "common.i"

%{
#include <QtCore/qmargins.h>
%}

%insert(m3wrapintf) %{
TYPE
  T = QMargins;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%ignore operator==;
%ignore operator!=;


%include <QtCore/qmargins.h>