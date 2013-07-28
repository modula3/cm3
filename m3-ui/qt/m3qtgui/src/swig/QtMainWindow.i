%module QtMainWindow

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qmainwindow.h>
#define DockOptions QMainWindow::DockOptions
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QMainWindow;
  DockOptions = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//if dont want the tabwidget included then define this
#define QT_NO_TABWIDGET 1


%ignore tabifiedDockWidgets;


//Local Enums
EnumMaps(QMainWindow, DockOption, ErrMode)

//Local flags -- need both since both forms used in h file
//not the #define above to fix the cxx file
EnumFlags(QMainWindow::DockOptions, DockOptions)
EnumFlags(DockOptions, DockOptions)

//dont import flags from module in which declared
//EnumFlagsImport(DockOptions, QMainWindow_DockOptions)

//remote enums
EnumMaps(QTabWidget, TabPosition, ErrMode)
EnumMaps(QTabWidget, TabShape, ErrMode)
EnumImport(QTabWidget, TabPosition&TabShape, QtTabWidget, ErrMode)


DoType(QMenuBar,QtMenuBar)
DoType(QStatusBar,QtStatusBar)
DoType(QDockWidget,QtDockWidget)
DoType(QToolBar,QtToolBar)
DoType(QMenu,QtMenu)
DoType(QWidget,QtWidget)

%apply ClassReturn {QWidget *menuWidget};
%apply ClassReturn {QWidget *centralWidget};


%include <QtGui/qmainwindow.h>
