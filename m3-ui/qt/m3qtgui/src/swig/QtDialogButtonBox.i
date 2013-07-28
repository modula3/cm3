%module QtDialogButtonBox

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qdialogbuttonbox.h>
#define StandardButtons QDialogButtonBox::StandardButtons
%}

%insert(m3rawintf) %{
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{
TYPE

  T = QDialogButtonBox;
  StandardButtons = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//QList template
%ignore buttons;

//Local Enums
EnumMaps(QDialogButtonBox, StandardButton, ErrMode)
EnumMaps(QDialogButtonBox, ButtonRole, ErrMode)

//Local flags
EnumFlags(StandardButtons, StandardButtons)

DoType(QAbstractButton,QtAbstractButton);
DoType(QPushButton,QtPushButton);
DoType(QWidget,QtWidget);

%include <QtGui/qdialogbuttonbox.h>
