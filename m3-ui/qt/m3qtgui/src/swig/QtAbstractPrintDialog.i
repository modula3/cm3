%module QtAbstractPrintDialog

%include "m3qt.i"
%include "common.i"

%import "QtDialog.i"

%{
#include <QtGui/qabstractprintdialog.h>
#define  PrintDialogOptions  QAbstractPrintDialog::PrintDialogOptions

%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtDialog IMPORT QDialog;

TYPE
  T = QAbstractPrintDialog;
  PrintDialogOptions = INTEGER;

%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//Qlist
%ignore setOptionTabs;

//Local enums
EnumMaps(QAbstractPrintDialog, PrintRange, ErrMode)
EnumMaps(QAbstractPrintDialog, PrintDialogOption, ErrMode)

//Local flags
EnumFlags(QAbstractPrintDialog::PrintDialogOptions, PrintDialogOptions)
EnumFlags(PrintDialogOptions, PrintDialogOptions)

DoType(QPrinter,QGuiStubs)

%include <QtGui/qabstractprintdialog.h>
