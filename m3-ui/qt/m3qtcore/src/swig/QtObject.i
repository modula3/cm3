%module QtObject

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

%{
#include <QtCore/qobject.h>
%}

%feature("compactdefaultargs") QObject::connect;
%feature("compactdefaultargs") QObject::disconnect;


%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QObject;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtString IMPORT QString;
FROM QtByteArray IMPORT QByteArray;
%}

//Need to get the tr functions
#define QT_NO_TRANSLATION

//ignore these problem ones to do with templates i think

%ignore qFindChild;
%ignore qFindChildren;
%ignore setProperty;
%ignore property;
%ignore dynamicPropertyNames;

//ignore classes we havent wrapped

//ignore all of QObjectdata
%ignore QObjectData;
//ignore all of QObjectUserdata
%ignore QObjectUserData;
%ignore userData;
%ignore setUserData;


//QThread
%ignore thread;
%ignore moveToThread;

//QObjectList
%ignore children;

//QMetaObject
%ignore qt_qFindChildren_helper;
%ignore qt_qFindChild_helper;
//%ignore metaObject;

%ignore connect;
%ignore disconnect;

%apply ClassIn     {QObject *};
%apply ClassReturn {QObject *parent};

//lets not destroy the parent object for the moment until tested
//maybe we can let it be destroyed and it wont matter in which case remove this tmap
%typemap("m3wrapretcheck") QObject *parent        %{
  result := NEW($1_basetype);
  result.cxxObj := ret;
%};

DoType(QEvent,QtEvent)


%include <QtCore/qobject.h>
