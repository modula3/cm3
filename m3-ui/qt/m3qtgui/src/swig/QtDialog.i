%module QtDialog

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qdialog.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QDialog;

%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//Local enums
EnumMaps(QDialog, DialogCode, ErrMode)

%apply ClassReturn {QWidget *extension};

%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget%}

//needs to be in a common include somewhere but not common.i
%typemap("m3wrapintype:import")  const QString & %{QtString QString%}

%include <QtGui/qdialog.h>
