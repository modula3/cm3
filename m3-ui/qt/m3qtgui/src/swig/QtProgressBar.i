%module QtProgressBar

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qprogressbar.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QProgressBar;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

//Local enums
EnumMaps(QProgressBar, Direction, ErrMode)

DoType(QWidget,QtWidget);

%include <QtGui/qprogressbar.h>
