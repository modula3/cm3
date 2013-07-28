%module QtCheckBox

%include "m3qt.i"
%include "common.i"

%import "QtAbstractButton.i"

%{
#include <QtGui/qcheckbox.h>
%}

%insert(m3rawintf) %{
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{
FROM QtAbstractButton IMPORT QAbstractButton;
TYPE
  T = QCheckBox;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}


DoType(QWidget,QtWidget)


%include <QtGui/qcheckbox.h>
