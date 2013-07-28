%module QtToolBox

%include "m3qt.i"
%include "common.i"

%import "QtFrame.i"

%{
#include <QtGui/qtoolbox.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtFrame IMPORT QFrame;

TYPE
  T = QToolBox;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

%apply ClassReturn   {QWidget *widget};
%apply ClassReturn   {QWidget *currentWidget};


%include <QtGui/qtoolbox.h>
