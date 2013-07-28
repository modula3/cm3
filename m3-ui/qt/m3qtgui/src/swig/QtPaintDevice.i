%module QtPaintDevice

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

%{
#include <QtGui/qpaintdevice.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{

TYPE
  T = QPaintDevice;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//this causes link error
%ignore qt_paint_device_metric;
%ignore PaintDeviceMetric;

//local enums
//dont need these if the function is ignored
//EnumMaps(QPaintDevice, PaintDeviceMetric, ErrMode)

//Local classes
%apply ClassIn {const QPaintDevice *};


DoType(QPaintEngine,QGuiStubs);
DoType(QPaintDeviceMetric,QGuiStubs);

%include <QtGui/qpaintdevice.h>
