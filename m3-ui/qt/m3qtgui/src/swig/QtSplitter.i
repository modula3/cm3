%module QtSplitter

%include "m3qt.i"
%include "common.i"

%import "QtFrame.i"

%{
#include <QtGui/qsplitter.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtFrame IMPORT QFrame;

TYPE
  T = QSplitter;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
%}

%ignore operator<<;
%ignore operator>>;

//Qlist template problems
%ignore sizes;
%ignore setSizes;

%apply  intvar* {int *};

DoType(QWidget,QtWidget)

//workaround to the private class constructor
//note it does not apply to the main class
%apply ClassIn     {QSplitter *parent};

//same deal with this class only works for splitter methd
%apply ClassReturn     {QSplitter *splitter};


%include <QtGui/qsplitter.h>
