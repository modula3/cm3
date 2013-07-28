%module QtAbstractItemView

%include "m3qt.i"
%include "common.i"

%import "QtAbstractScrollArea.i"

%{
#include <QtGui/qabstractitemview.h>
#define  EditTriggers  QAbstractItemView::EditTriggers
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtAbstractScrollArea IMPORT QAbstractScrollArea;
TYPE
  T = QAbstractItemView;
  EditTriggers = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtAbstractItemDelegate IMPORT QAbstractItemDelegate;
%}

#define Q_NO_USING_KEYWORD

//has a variant
%ignore inputMethodQuery;

//dragging in QPoint in even though its abstract virtual
//we dont wrap this method anyway
//need to fix modula3.cxx to ignore import if its abstract
%ignore indexAt;

//Local enums
EnumMaps(QAbstractItemView, SelectionMode, ErrMode)
EnumMaps(QAbstractItemView, SelectionBehavior, ErrMode)
EnumMaps(QAbstractItemView, ScrollHint, ErrMode)
EnumMaps(QAbstractItemView, ScrollMode, ErrMode)
EnumMaps(QAbstractItemView, DragDropMode, ErrMode)

//Local flags
EnumFlags(QAbstractItemView::EditTriggers, EditTriggers)
EnumFlags(EditTriggers, EditTriggers)


//circular imports unless redefine QAbstractItemDelegate to be refany
%apply ClassIn {QAbstractItemDelegate *};
%apply ClassReturn {QAbstractItemDelegate *};

%typemap("m3wrapintype")  QAbstractItemDelegate *   %{REFANY (* QApplication *)%}
%typemap("m3wrapouttype") QAbstractItemDelegate *   %{REFANY%}
%typemap("m3wraprettype") QAbstractItemDelegate *   %{REFANY%}
%typemap("m3wrapargvar")  QAbstractItemDelegate *   %{$1tmp :=  LOOPHOLE(NARROW($1_name,QAbstractItemDelegate).cxxObj,ADDRESS);%}


DoType(QWidget,QtWidget)
DoType(QAbstractItemModel,QtAbstractItemModel)
DoType(QModelIndex,QtAbstractItemModel)
DoType(QItemSelectionModel,QtItemSelectionModel)

%include <QtGui/qabstractitemview.h>
