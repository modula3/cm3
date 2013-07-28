%module QtSizePolicy

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

%{
#include <QtGui/qsizepolicy.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QSizePolicy;
  ControlTypes = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}


%ignore operator ==;
%ignore operator !=;
%ignore operator QVariant;

//%ignore Policy;

//Local enums
EnumMaps(QSizePolicy, Policy, ErrMode)
EnumMaps(QSizePolicy, ControlType, ErrMode)

//Local Flags
EnumFlags(QSizePolicy::ControlTypes, ControlTypes)
EnumFlags(QSizePolicy, ControlTypes)


%include <QtGui/qsizepolicy.h>
