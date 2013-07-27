%module QtObjectDefs

// Too hard causing swig crashes

%include "m3qt.i"
%include "common.i"


%{
#include "qobjectdefs.h"
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QObject;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtString IMPORT QString;
%}

/*
//ignore these problem ones to do with templates i think

%ignore qFindChild;
%ignore qFindChildren;

//ignore classes we havnt wrapped

//ignore all of QObjectdata
%ignore QObjectData;
//ignore all of QObjectUserdata
%ignore QObjectUserData;
%ignore userData;
%ignore setUserData;



%apply ClassIn     {QObject *};
%apply ClassIn     {QEvent *};
%apply ClassReturn {QObject *parent};
*/

/*
//lets not destroy the parent object for the moment until tested
//maybe we can let it be destroyed and it wont matter in which case remove this tmap
%typemap("m3wrapretcheck") QObject *parent        %{
  result := NEW($1_basetype);
  result.cxxObj := ret;
%};


%typemap("m3wrapintype:import")  QEvent * %{QtEvent QEvent%}

//Need to import QByteArray for QString
%typemap("m3wrapintype:import")  QObject * %{QtByteArray QByteArray%}

//could put this in common so its included
%typemap("ctype") QString  %{QString * %}
*/

%include "qobjectdefs.h"
