%module QtFontDialog

%include "m3qt.i"
%include "common.i"

%import "QtDialog.i"

%{
#include <QtGui/qfontdialog.h>
#define  FontDialogOptions  QFontDialog::FontDialogOptions
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtDialog IMPORT QDialog;

TYPE
  T = QFontDialog;
  FontDialogOptions = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//Local enums
EnumMaps(QFontDialog, FontDialogOption, ErrMode)

/* //Local flags */
EnumFlags(QFontDialog::FontDialogOptions, FontDialogOptions)
EnumFlags(FontDialogOptions, FontDialogOptions)

DoType(QFont,QtFont)
DoType(QObject,QtObject)

%include <QtGui/qfontdialog.h>
