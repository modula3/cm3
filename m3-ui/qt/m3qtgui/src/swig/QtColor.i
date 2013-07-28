%module QtColor

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

%{
#include <QtGui/qcolor.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QColor;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
FROM QtString IMPORT QString;
%}

//rename some operators
%rename(Op_Assign)  QColor::operator=;
%rename(Op_Equals)  QColor::operator==;
%rename(Op_NotEquals)  QColor::operator!=;

%ignore operator QVariant;

//Local enums
EnumMaps(QColor, Spec, ErrMode)

//Local classes
%apply ClassIn     {const QColor &};
%apply ClassReturn {QColor};

%apply  intvar* {int *r, int *g, int *b, int *a};
%apply  intvar* {int *h, int *s, int *v, int *a};
%apply  intvar* {int *c, int *m, int *y, int *k, int *l};

//specialised tmaps for Rgb to get the type - note its not a class
%typemap("m3rawintype")   QRgb  %{C.unsigned_int%}
%typemap("m3rawrettype")  QRgb  %{C.unsigned_int%}
%typemap("m3wrapintype")  QRgb  %{QtRgb.T%}
%typemap("m3wraprettype") QRgb  %{QtRgb.T%}
//cant figure out why these cant just be QRgb
%typemap("m3wrapouttype") QRgb rgba %{QtRgb.T%}
%typemap("m3wrapouttype") QRgb rgb %{QtRgb.T%}

%typemap("m3wrapintype:import") QRgb  %{QtRgb%}

//for assign operator
%apply SelfReturn {QColor &};

DoType(QStringList,QtStringList)

%include <QtGui/qcolor.h>
