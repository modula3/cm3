%module QtPageSetupDialog

%include "m3qt.i"
%include "common.i"

//need to swig this one i guess in meantime use qtdialog.i
//%import "QtAbstractPageSetupDialog.i"
%import "QtDialog.i"

%{
#include <QtGui/qpagesetupdialog.h>
#define  PageSetupDialogOptions  QPageSetupDialog::PageSetupDialogOptions
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QPageSetupDialog;
  QPageSetupDialog_PageSetupDialogOptions = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//Local enums
EnumMaps(QPageSetupDialog, PageSetupDialogOption, ErrMode)

/* //Local flags */
EnumFlags(QPageSetupDialog::PageSetupDialogOptions, QPageSetupDialog_PageSetupDialogOptions)
EnumFlags(PageSetupDialogOptions, QPageSetupDialog_PageSetupDialogOptions)

DoType(QDialog,QtDialog)
DoType(QObject,QtObject)

DoType(QPrinter,QGuiStubs)

%include <QtGui/qpagesetupdialog.h>
