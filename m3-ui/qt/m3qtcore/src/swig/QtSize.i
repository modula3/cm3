%module QtSize

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

%{
#include <QtCore/qsize.h>
%}


%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{
TYPE
  T = QSize;
%}

%ignore operator==;
%ignore operator!=;
%ignore operator+;
%ignore operator-;
%ignore operator*;
%ignore operator*;
%ignore operator/;

//no work problem with friend inline
//%rename(Plus) QSize::operator+(const QSize &, const QSize &) const;
//%rename(Equals) QSize::operator==;

%rename(PlusEqual)     QSize::operator+=;
%rename(MinusEqual)    QSize::operator-=;
%rename(MultiplyEqual) QSize::operator*=;
%rename(DivideEqual)   QSize::operator/=;

%rename(PlusEqual)     QSizeF::operator+=;
%rename(MinusEqual)    QSizeF::operator-=;
%rename(MultiplyEqual) QSizeF::operator*=;
%rename(DivideEqual)   QSizeF::operator/=;


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

//Local classes
%apply ClassIn     {const QSize &};
%apply ClassIn     {const QSizeF &};

%apply ClassReturn {QSize &};
%apply ClassReturn {QSizeF &};
%apply ClassReturn {QSize};
%apply ClassReturn {QSizeF};

//Only use DoType for imported classes. The local classes have apply
//DoType(QSize,QtSize)
//DoType(QSizeF,QtSizeF)


%include <QtCore/qsize.h>

