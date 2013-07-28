%module QtFileDialog

%include "m3qt.i"
%include "common.i"

%import "QtDialog.i"

%{
#include <QtGui/qfiledialog.h>
#define  Options  QFileDialog::Options
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtDialog IMPORT QDialog;

TYPE
  T = QFileDialog;
  Options = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}


//qlist
%ignore sidebarUrls;
%ignore setSidebarUrls;

//QDir
%ignore filter;
%ignore setFilter;


//Local enums
EnumMaps(QFileDialog, Option, ErrMode)
EnumMaps(QFileDialog, ViewMode, ErrMode)
EnumMaps(QFileDialog, FileMode, ErrMode)
EnumMaps(QFileDialog, AcceptMode, ErrMode)
EnumMaps(QFileDialog, DialogLabel, ErrMode)

/* //Local flags */
EnumFlags(QFileDialog::Options, Options)
EnumFlags(Options, Options)

//this apply should be in a common area
%apply QString{QString *};

%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget,QtObject QObject%}

DoType(QStringList,QtStringList)
DoType(QAbstractItemDelegate,QtAbstractItemDelegate)

DoType(QFileIconProvider,QGuiStubs)
DoType(QDir,QGuiStubs)
DoType(QAbstractProxyModel,QGuiStubs)

%include <QtGui/qfiledialog.h>
