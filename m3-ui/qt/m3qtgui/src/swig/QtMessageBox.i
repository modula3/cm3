%module QtMessageBox

%include "m3qt.i"
%include "common.i"

%import "QtDialog.i"

%{
#include <QtGui/qmessagebox.h>
#define  StandardButtons  QMessageBox::StandardButtons
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtDialog IMPORT QDialog;

TYPE
  T = QMessageBox;
  StandardButtons = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

//qlist
%ignore buttons;

//defined in qapplication
%ignore aboutQt;

//id clashes with enums
%rename ("InfoDialog") QMessageBox::information;
%rename ("QuestionDialog") QMessageBox::question;
%rename ("WarningDialog") QMessageBox::warning;
%rename ("CriticalDialog") QMessageBox::critical;

//Local enums
EnumMaps(QMessageBox, Icon, ErrMode)
EnumMaps(QMessageBox, ButtonRole, ErrMode)
EnumMaps(QMessageBox, StandardButton, ErrMode)

//Local flags
EnumFlags(QMessageBox::StandardButtons, StandardButtons)
EnumFlags(StandardButtons, StandardButtons)

DoType(QStringList,QtStringList)
DoType(QAbstractButton,QtAbstractButton)
DoType(QPushButton,QtPushButton)
DoType(QDialog,QtDialog)

DoType(QObject,QtObject)

%include <QtGui/qmessagebox.h>
