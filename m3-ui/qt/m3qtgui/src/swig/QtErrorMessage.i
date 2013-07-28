%module QtErrorMessage

%include "m3qt.i"
%include "common.i"

%import "QtDialog.i"

%{
#include <QtGui/qerrormessage.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{

FROM QtDialog IMPORT QDialog;

TYPE
  T = QErrorMessage;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//need to override this one and its static so the syntax thingy
%apply ClassReturn {QErrorMessage *QErrorMessage::qtHandler};


%include <QtGui/qerrormessage.h>
