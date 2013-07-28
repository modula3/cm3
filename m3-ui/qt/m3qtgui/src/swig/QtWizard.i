%module QtWizard

%include "m3qt.i"
%include "common.i"

%import "QtDialog.i"

%{
#include <QtGui/qwizard.h>
#define  WizardOptions  QWizard::WizardOptions
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtDialog IMPORT QDialog;

TYPE
  T = QWizard;
  WizardOptions = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

//qvariant
%ignore field;
%ignore setField;

//qlist
%ignore visitedPages;
%ignore pageIds;
%ignore setButtonLayout;

//Local enums
EnumMaps(QWizard, WizardButton, ErrMode)
EnumMaps(QWizard, WizardPixmap, ErrMode)
EnumMaps(QWizard, WizardStyle, ErrMode)
EnumMaps(QWizard, WizardOption, ErrMode)

/* //Local flags */
EnumFlags(QWizard::WizardOptions, WizardOptions)
EnumFlags(WizardOptions, WizardOptions)

//Local classes
%apply ClassIn {QWizardPage *page};
%apply ClassReturn {QWizardPage *page};
%apply ClassReturn {QWizardPage *currentPage};

DoType(QWidget,QtWidget)
DoType(QAbstractButton,QtAbstractButton)

%include <QtGui/qwizard.h>
