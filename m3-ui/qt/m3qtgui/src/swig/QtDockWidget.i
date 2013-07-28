%module QtDockWidget

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qdockwidget.h>
#define  DockWidgetFeatures QDockWidget::DockWidgetFeatures
%}

%insert(m3rawintf) %{
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{
TYPE
  T = QDockWidget;

  DockWidgetFeatures = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//Local enums
EnumMaps(QDockWidget, DockWidgetFeature, ErrMode)

//Local flags
EnumFlags(DockWidgetFeatures, DockWidgetFeatures)

%apply ClassReturn {QWidget *widget};
%apply ClassReturn {QWidget *titleBarWidget};

%typemap("m3wrapintype:import") QWidget *  %{QtWidget QWidget%}

DoType(QAction,QtAction)

%include <QtGui/qdockwidget.h>
