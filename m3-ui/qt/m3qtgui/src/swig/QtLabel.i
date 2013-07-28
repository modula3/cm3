%module QtLabel

%include "m3qt.i"
%include "common.i"

%import "QtFrame.i"


%{
#include <QtGui/qlabel.h>
%}

%insert(m3rawintf) %{
%}


%insert(m3wrapintf) %{
FROM QtFrame IMPORT QFrame;

TYPE
  T = QLabel;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}


%apply ClassReturn {QWidget *buddy};


%include <QtGui/qlabel.h>
