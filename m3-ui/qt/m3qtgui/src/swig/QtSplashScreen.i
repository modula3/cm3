%module QtSplashScreen

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qsplashscreen.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QSplashScreen;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

DoType(QColor,QtColor);
DoType(QPixmap,QtPixmap);
DoType(QWidget,QtWidget);

%include <QtGui/qsplashscreen.h>
