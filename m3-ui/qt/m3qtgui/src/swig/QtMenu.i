%module QtMenu

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qmenu.h>
%}

%insert(m3rawintf) %{
%}


%insert(m3wrapintf) %{
TYPE
  T = QMenu;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

%ignore exec;

//Local classes override a couple of methods which have a QMenu as a parm or return one
%apply ClassIn {QMenu *menu};
%apply SelfReturn {QMenu *addMenu};


//THis was a fix which was not needed after fixing local classes but shows
//how to override a typemap for a constructor
//%typemap("m3wrapargvar")   QMenu* self  %{selfAdr :=  LOOPHOLE($1_name.cxxObj,ADDRESS);%}
//%typemap("m3wrapargraw")   QMenu* self %{selfAdr%}

DoType(QAction,QtAction);
DoType(QObject,QtObject);
DoType(QWidget,QtWidget);

%include <QtGui/qmenu.h>
