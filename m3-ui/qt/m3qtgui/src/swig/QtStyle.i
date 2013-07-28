%module QtStyle

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtGui/qstyle.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtObject IMPORT QObject;
TYPE
  T = QStyle;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtString IMPORT QString;
FROM QtWidget IMPORT QWidget;
FROM QtApplication IMPORT QApplication;
%}

%ignore operator <<;


//Local enums
EnumMaps(QStyle, StateFlag, ErrMode)
EnumMaps(QStyle, PrimitiveElement, ErrMode)
EnumMaps(QStyle, ControlElement, ErrMode)
EnumMaps(QStyle, SubElement, ErrMode)
EnumMaps(QStyle, ComplexControl, ErrMode)
EnumMaps(QStyle, SubControl, ErrMode)
EnumMaps(QStyle, PixelMetric, ErrMode)
EnumMaps(QStyle, ContentsType, ErrMode)
EnumMaps(QStyle, RequestSoftwareInputPanel, ErrMode)
EnumMaps(QStyle, StyleHint, ErrMode)
EnumMaps(QStyle, StandardPixmap, ErrMode)

//remote enums
//only in abstract virtual methods and we dont wrap those
//EnumMaps(QIcon, Mode, ErrMode)
//proper enum
//EnumImport(QIcon, Mode, QtIcon, 0)

EnumMaps(QPalette, ColorRole, ErrMode)
EnumImport(QPalette, ColorRole, QtPalette, ErrMode)

EnumMaps(QSizePolicy, ControlType, ErrMode)
EnumImport(QSizePolicy, ControlType, QtSizePolicy, ErrMode)

EnumFlags(QSizePolicy::ControlTypes, ControlTypes)
EnumFlagsImport(QSizePolicy::ControlTypes, ControlTypes, QtSizePolicy)


//circular imports unless redefine qapplication to be refany
%apply ClassIn {QApplication *};
%apply ClassReturn {QApplication *};

%typemap("m3wrapintype")  QApplication *   %{REFANY (* QApplication *)%}
%typemap("m3wrapouttype") QApplication *   %{REFANY%}
%typemap("m3wraprettype") QApplication *   %{REFANY%}
%typemap("m3wrapargvar")  QApplication *   %{$1tmp :=  LOOPHOLE(NARROW($1_name,QApplication).cxxObj,ADDRESS);%}

//circular imports unless redefine qwidget to be refany
%apply ClassIn {QWidget *};
%apply ClassReturn {QWidget *};

%typemap("m3wrapintype")  QWidget *   %{REFANY (*QWidget*)%}
%typemap("m3wrapouttype") QWidget *   %{REFANY%}
%typemap("m3wraprettype") QWidget *   %{REFANY%}
%typemap("m3wrapargvar")  QWidget *   %{$1tmp :=  LOOPHOLE(NARROW($1_name,QWidget).cxxObj,ADDRESS);%}

DoType(QSize,QtSize)
DoType(QPoint,QtPoint)
DoType(QRect,QtRect)
DoType(QIcon,QtIcon)
DoType(QPalette,QtPalette)
DoType(QPixmap,QtPixmap)
DoType(QFontMetrics,QtFontMetrics)

DoType(QPainter,QGuiStubs)
DoType(QStyleOption,QGuiStubs)


//only in abstract virtual methods and we dont wrap those
//DoType(QStyleOptionComplex,QGuiStubs)
//DoType(QStyleHintReturn,QGuiStubs)

%include <QtGui/qstyle.h>

