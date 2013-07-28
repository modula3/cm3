%module QtAction

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtGui/qaction.h>
%}

%insert(m3rawintf) %{
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{
TYPE
  T = QAction;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtMenu IMPORT QMenu;
FROM QtWidget IMPORT QWidget;
FROM QtString IMPORT QString;
FROM QtByteArray IMPORT QByteArray;
%}

//qlist
%ignore setShortcuts;
%ignore shortcuts;
%ignore associatedWidgets;
%ignore associatedGraphicsWidgets;

//qvariant
%ignore data;
%ignore setData;

//Local enums
EnumMaps(QAction, MenuRole, ErrMode)
EnumMaps(QAction, SoftKeyRole, ErrMode)
EnumMaps(QAction, Priority, ErrMode)
EnumMaps(QAction, ActionEvent, ErrMode)

//Some of these classes cant be imported because of circ imports so we refany

%apply ClassIn     {QMenu *};
%apply ClassReturn {QMenu *};
%apply ClassIn     {QWidget *};
%apply ClassReturn {QWidget *};

%typemap("m3wrapintype")  QMenu *   %{REFANY%}
%typemap("m3wrapouttype") QMenu *   %{REFANY%}
%typemap("m3wraprettype") QMenu *   %{REFANY%}
%typemap("m3wrapargvar")  QMenu *   %{$1tmp :=  LOOPHOLE(NARROW($1_name,QMenu).cxxObj,ADDRESS);%}

%typemap("m3wrapintype")  QWidget *   %{REFANY%}
%typemap("m3wrapouttype") QWidget *   %{REFANY%}
%typemap("m3wraprettype") QWidget *   %{REFANY%}
%typemap("m3wrapargvar")  QWidget *   %{$1tmp :=  LOOPHOLE(NARROW($1_name,QWidget).cxxObj,ADDRESS);%}

%typemap("m3wrapintype:import") QAction * %{QtObject QObject%}

DoType(QIcon, QtIcon);
DoType(QFont, QtFont);
DoType(QKeySequence, QtKeySequence);
DoType(QActionGroup, QtActionGroup);


%include <QtGui/qaction.h>
