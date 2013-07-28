%module QtInputDialog

%include "m3qt.i"
%include "common.i"

%import "QtDialog.i"

%{
#include <QtGui/qinputdialog.h>
#define  InputDialogOptions  QInputDialog::InputDialogOptions
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtDialog IMPORT QDialog;

TYPE
  T = QInputDialog;
  InputDialogOptions = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

//Local enums
EnumMaps(QInputDialog, InputDialogOption, ErrMode)
EnumMaps(QInputDialog, InputMode, ErrMode)

//remote enums
EnumMaps(QLineEdit, EchoMode, ErrMode)

/* //Local flags */
EnumFlags(QInputDialog::InputDialogOptions, InputDialogOptions)
EnumFlags(InputDialogOptions, InputDialogOptions)

%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget,QtLineEdit EchoMode%}

DoType(QStringList,QtStringList)
DoType(QObject,QtObject)

%include <QtGui/qinputdialog.h>
