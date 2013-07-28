%module QtProgressDialog

%include "m3qt.i"
%include "common.i"

%import "QtDialog.i"

%{
#include <QtGui/qprogressdialog.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtDialog IMPORT QDialog;

TYPE
  T = QProgressDialog;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

DoType(QLabel,QtLabel)
DoType(QPushButton,QtPushButton)
DoType(QProgressBar,QtProgressBar)

DoType(QObject,QtObject)

%include <QtGui/qprogressdialog.h>
