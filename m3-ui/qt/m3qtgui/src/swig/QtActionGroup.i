%module QtActionGroup

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtGui/qactiongroup.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QActionGroup;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtString IMPORT QString;
FROM QtAction IMPORT QAction;
%}

//qlist
%ignore actions;

//circular imports unless redefine qaction to be refany

%apply ClassIn {QAction *};
%apply ClassReturn {QAction *};

%typemap("m3wrapintype")  QAction *      %{REFANY (*QAction*)%}
%typemap("m3wraprettype") QAction *      %{REFANY%}
%typemap("m3wrapouttype") QAction *      %{REFANY%}
%typemap("m3wrapargvar")  QAction *      %{$1tmp :=  LOOPHOLE(NARROW($1_name,QAction).cxxObj,ADDRESS);%}

DoType(QObject,QtObject);
DoType(QIcon,QtIcon);


%include <QtGui/qactiongroup.h>
