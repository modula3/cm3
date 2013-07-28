%module QtWSEmbedWidget

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qwsembedwidget.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QWSEmbedWidget;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//Only use this class if you have embedded linux installed
//otherwise you will get a link error

DoType(QWidget,QtWidget);

%include <QtGui/qwsembedwidget.h>
