%module QtGroupBox

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qgroupbox.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QGroupBox;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

DoType(QWidget,QtWidget)

%include <QtGui/qgroupbox.h>
