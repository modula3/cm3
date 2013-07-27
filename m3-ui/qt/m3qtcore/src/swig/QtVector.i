%module QtVector
//this is crashing swig investigate sometime
%include "m3qt.i"
%include "common.i"


%{
#include "qvector.h"
%}


%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{
TYPE
  T = QVector;
%}

%ignore QvectorData;

%ignore operator==;
%ignore operator!=;
%ignore operator+;
%ignore operator-;
%ignore operator*;
%ignore operator*;
%ignore operator/;


%rename(PlusEqual)     QSize::operator+=;
%rename(MinusEqual)    QSize::operator-=;
%rename(MultiplyEqual) QSize::operator*=;
%rename(DivideEqual)   QSize::operator/=;

%rename(PlusEqual)     QSizeF::operator+=;
%rename(MinusEqual)    QSizeF::operator-=;
%rename(MultiplyEqual) QSizeF::operator*=;
%rename(DivideEqual)   QSizeF::operator/=;

//Enums
EnumMap(Qt::AspectRatioMode, AspectRatioMode)
EnumImp(Qt::AspectRatioMode, AspectRatioMode, QtNamespace)

%typemap("m3rawintype")    QBasicAtomicInt         %{ADDRESS%}

/*
need to extend the class to include the friend operators
but not really working. problems include 2 wrapper functions generated
one with no ref to class which points to problem with modula3.cxx and also
the code in the {} gets converted into som internal swig function which must be
correct to compile
%rename(equalop22) operator==;

%extend QSize {
  bool operator==(const QSize &s1, const QSize &s2){return false;};

}
*/


/*
%apply ClassIn {const QSize &};
%apply ClassIn {const QSizeF &};
*/
%apply SelfReturn {QSize &};
%apply SelfReturn {QSizeF &};
%apply SelfReturn {QSize};
%apply SelfReturn {QSizeF};

/*
//the class returns should be pointer
//can this be in common?
%typemap("ctype") QSize  %{QSize *%}
%typemap("ctype") QSizeF  %{QSizeF *%}
*/

%include "qvector.h"
