%module QtSize

%include "m3qt.i"
%include "common.i"

%{
#include "qsize.h"
%}


%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{

TYPE
  M3RatioEnum = {a,b,c};
%}

//maybe later this requires a stack of typemaps to return class QSize
%ignore QSizeF::toSize;

//raw for QSize
%typemap("m3rawrettype")   QSize     %{ADDRESS%}
%typemap("m3rawrettype")   QSizeF    %{ADDRESS%}

//rename some overloaded functions
%rename("scalef") scale (const QSize &s, Qt::AspectRatioMode mode);
%rename("scalef") scale (const QSizeF &s, Qt::AspectRatioMode mode);

//Enums
%typemap("m3rawintype")   Qt::AspectRatioMode   %{C.int%}
%typemap("m3wrapintype")  Qt::AspectRatioMode   %{M3RatioEnum%}
%typemap("m3wrapargraw")  Qt::AspectRatioMode   %{ORD($1_name)%}


//%ignore QSize::operator+=;
//%ignore QSize::operator-=;
//%ignore QSize::operator*=;
//%ignore QSize::operator/=;

%rename(PlusEqual)     QSize::operator+=;
%rename(MinusEqual)    QSize::operator-=;
%rename(MultiplyEqual) QSize::operator*=;
%rename(DivideEqual)   QSize::operator/=;

%ignore operator==;
%ignore operator!=;
%ignore operator+;
%ignore operator-;
%ignore operator*;
%ignore operator*;
%ignore operator/;

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

//%ignore QSizeF::operator+=;
//%ignore QSizeF::operator-=;
//%ignore QSizeF::operator*=;
//%ignore QSizeF::operator/=;

%rename(PlusEqual)     QSizeF::operator+=;
%rename(MinusEqual)    QSizeF::operator-=;
%rename(MultiplyEqual) QSizeF::operator*=;
%rename(DivideEqual)   QSizeF::operator/=;


//ref input for QSize
%typemap("m3rawinmode")  const QSize &  %{%}
%typemap("m3wrapinmode") const QSize &  %{%}
%typemap("m3wrapargvar") const QSize &  %{$1tmp :=  LOOPHOLE($1_name,ADDRESS);%}
%typemap("m3wrapargraw") const QSize &  %{$1tmp%}

%apply const QSize & {const QSizeF &};


//here we apply the common set to the real method in the real class and get the correct set of typemaps
%apply TBase * method99 {QSize expandedTo};
%apply TBase * method99 {QSize boundedTo};

%apply TBase * method99 {QSizeF expandedTo};
%apply TBase * method99 {QSizeF boundedTo};


//apply for operators
%apply TBase * method99 {QSize &};
%apply TBase * method99 {QSizeF &};

%include "qsize.h"
