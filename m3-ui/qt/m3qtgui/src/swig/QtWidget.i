%module QtWidget

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"
%import "QtPaintDevice.i"

%{
#include <QtGui/qwidget.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtObject IMPORT QObject;
TYPE
  T = QWidget;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
FROM QtLayout IMPORT QLayout;
FROM QtInputContext IMPORT QInputContext;
%}


%nodefaultctor;

//Dont allow a constructor yet and no default ones above
//Ok now we have constructors
//%ignore QWidget::QWidget;

%ignore QWIDGETSIZE_MAX;

%ignore internalWinId;
%ignore QPaintDevice;
//bitfields in class
%ignore QWidgetData;
%ignore inputMethodQuery;

//function initialises in declaration
%ignore render;

//qlist
%ignore actions;
%ignore addActions;
%ignore insertActions;

//private data with bit fields
%ignore qt_qwidget_data;
%ignore qt_widget_private;

//not sure what we do with a handle its system specific - see namespace
//maybe add type HANDLE : ADDRESS
%ignore handle;


//these cause g++ errors actually if you use the optimal flag in the common.i
//then the fontmetrics and fontinfo seem to work
//%ignore fontMetrics;
//%ignore fontInfo;
%ignore windowIcon;
%ignore locale;

//renmae the no parms constructor as init_widget as init was causing qstring segv
%rename(init_widget) QWidget::QWidget();

//Local Enums
EnumMaps(QWidget, RenderFlag, ErrMode)

//remote enums
EnumMaps(QSizePolicy, Policy, ErrMode)
EnumImport(QSizePolicy, Policy, QtSizePolicy, ErrMode)

EnumMaps(QPalette, ColorRole, ErrMode)
EnumImport(QPalette, ColorRole, QtPalette, ErrMode)


//override the const for methods putting in readonly
%typemap("m3rawinmode")    const SWIGTYPE * ""
%typemap("m3wrapinmode")   const SWIGTYPE * ""


%apply ClassIn       {QWidget *};
//Need to override these 2 tmaps since the classin for a pointer type stuffs
//everything up and the selfAdr gets clobbered
%typemap("m3wrapargvar")   QWidget* self  %{selfAdr :=  LOOPHOLE($1_name.cxxObj,ADDRESS);%}
%typemap("m3wrapargraw")   QWidget* self %{selfAdr%}

//cannot have a classreturn since the constructors get mucked up
//%apply ClassReturn   {QWidget *};


/*
qwidget does not need qrect * but if use import in qregion then
dont have to have the classin tmaps for a lot of classes. actually
i have used the dotype macro the extend this partially and some of these could
go in seperate i file
*/


//circular imports unless redefine qlayout to be refany
%apply ClassIn {QLayout *};
%apply ClassReturn {QLayout *};
//cant import QLayout since circular imports has to be refany type
//DoType(QLayout,QtLayout)


%typemap("m3wrapintype")  QLayout *   %{REFANY%}
%typemap("m3wrapouttype") QLayout *   %{REFANY%}
%typemap("m3wraprettype") QLayout *   %{REFANY%}
%typemap("m3wrapargvar")  QLayout *   %{$1tmp :=  LOOPHOLE(NARROW($1_name,QLayout).cxxObj,ADDRESS);%}

//circular imports unless redefine qinputcontext to be refany
%apply ClassIn {QInputContext *};
%apply ClassReturn {QInputContext *};
%typemap("m3wrapintype")  QInputContext *   %{REFANY%}
%typemap("m3wrapouttype") QInputContext *   %{REFANY%}
%typemap("m3wraprettype") QInputContext *   %{REFANY%}
%typemap("m3wrapargvar")  QInputContext *   %{$1tmp :=  LOOPHOLE(NARROW($1_name,QInputContext).cxxObj,ADDRESS);%}


%apply  intvar* {int *left, int *top, int *right, int *bottom};

//imported types
DoType(QSize,QtSize)
DoType(QPoint,QtPoint)
DoType(QRect,QtRect)
DoType(QRegion,QtRegion)
DoType(QBitmap,QtBitmap)
DoType(QPalette,QtPalette)
DoType(QMargins,QtMargins)
DoType(QFont,QtFont)
DoType(QIcon,QtIcon)
DoType(QByteArray,QtByteArray)
DoType(QSizePolicy,QtSizePolicy)
DoType(QStyle,QtStyle)
DoType(QAction,QtAction)
DoType(QKeySequence,QtKeySequence)
DoType(QCursor,QtCursor)
DoType(QFontInfo,QtFontInfo)
DoType(QFontMetrics,QtFontMetrics)
DoType(QPaintEngine,QtPaintEngine)

//NOT used in widget
DoType(QMovie,QtMovie)
DoType(QPixmap,QtPixmap)
DoType(QPicture,QtPicture)
DoType(QColor,QtColor)

//these classes are in guistubs - need to swig them

DoType(QLocale,QGuiStubs)
DoType(QInputMethodEvent,QGuiStubs)
DoType(QWindowSurface,QGuiStubs)
DoType(QButtonGroup,QGuiStubs)
DoType(QGraphicsEffect,QGuiStubs)
DoType(QGraphicsProxyWidget,QGuiStubs)


%apply SelfReturn   {QWidget *window};
%apply SelfReturn   {QWidget *nativeParentWidget};
%apply SelfReturn   {QWidget *topLevelWidget};
%apply SelfReturn   {QWidget *focusProxy};
%apply SelfReturn   {QWidget *focusWidget};
%apply SelfReturn   {QWidget *nextInFocusChain};
%apply SelfReturn   {QWidget *previousInFocusChain};
%apply SelfReturn   {QWidget *parentWidget};
%apply SelfReturn   {QWidget *childAt};

//apply to the static members as well
%apply StaticSelfReturn   {QWidget *QWidget::mouseGrabber};
%apply StaticSelfReturn   {QWidget *QWidget::keyboardGrabber};
%apply StaticSelfReturn   {QWidget *QWidget::find};


//needs to be in a common include somewhere but not common.i
//maybe needs to be here since most other widgets import this module
%typemap("m3wrapintype:import")  const QString & %{QtString QString%}

%include <QtGui/qwidget.h>
