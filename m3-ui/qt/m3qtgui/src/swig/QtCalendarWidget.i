%module QtCalendarWidget

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qcalendarwidget.h>
%}

%insert(m3rawintf) %{
%}


%insert(m3wrapintf) %{
TYPE
  T = QCalendarWidget;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}


//a Qmap template
%ignore dateTextFormat;

//compile error
%ignore headerTextFormat;
%ignore weekdayTextFormat;

//Local Enums
EnumMaps(QCalendarWidget, HorizontalHeaderFormat, ErrMode)
EnumMaps(QCalendarWidget, VerticalHeaderFormat, ErrMode)
EnumMaps(QCalendarWidget, SelectionMode, ErrMode)


DoType(QDate,QtDateTime);
DoType(QWidget,QtWidget);
DoType(QTextCharFormat,QtTextFormat);


%include <QtGui/qcalendarwidget.h>
