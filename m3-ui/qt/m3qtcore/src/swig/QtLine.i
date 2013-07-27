%module QtLine

%include "m3qt.i"
%include "common.i"

%{
#include <QtCore/qline.h>
%}


%insert(m3wrapimpl) %{
IMPORT WeakRef;

%}

%insert(m3wrapintf) %{
%}

//operators rename
%rename(EqualEqual)  operator==;
%rename(NotEqual)    operator!=;


%apply SelfReturn {QLine};
%apply SelfReturn {QLineF};

%apply ClassIn {const QLine &};
%apply ClassIn {const QLineF &};

DoType(QPoint,QtPoint)
DoType(QPointF,QtPoint)

/*
//import both objects with this typemap note the & and no space at end
%typemap("m3wrapintype:import")  const QPoint & %{QtPoint IMPORT $1_basetype&$1_basetypeF%}
*/

//Local enums
EnumMaps(QLineF, IntersectType, ErrMode)

//use this to import the enum in other modules
//EnumImport(QLineF, IntersectType, QtLine, ErrMode)

%include <QtCore/qline.h>
