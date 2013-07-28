%module QtColorDialog

%include "m3qt.i"
%include "common.i"

%import "QtDialog.i"

%{
#include <QtGui/qcolordialog.h>
#define  ColorDialogOptions  QColorDialog::ColorDialogOptions
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtDialog IMPORT QDialog;

TYPE
  T = QColorDialog;
  ColorDialogOptions = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}


//Local enums
EnumMaps(QColorDialog, ColorDialogOption, ErrMode)

//Local flags
EnumFlags(QColorDialog::ColorDialogOptions, ColorDialogOptions)
EnumFlags(ColorDialogOptions, ColorDialogOptions)

//specialised tmaps for Rgb to get the type - note its not a class
%typemap("m3rawintype") QRgb  %{C.unsigned_int%}
%typemap("m3rawrettype") QRgb  %{C.unsigned_int%}
%typemap("m3wrapintype") QRgb  %{QtRgb.T%}
%typemap("m3wraprettype") QRgb  %{QtRgb.T%}
%typemap("m3wrapintype:import")  QRgb  %{QtRgb%}

DoType(QColor,QtColor)
DoType(QObject,QtObject)

%include <QtGui/qcolordialog.h>
