%module QtTableWidget

%include "m3qt.i"
%include "common.i"

%import "QtTableView.i"

%{
#include <QtGui/qtablewidget.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtTableView IMPORT QTableView;

TYPE
  T = QTableWidget;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

%ignore operator<;
%ignore operator=;

//Qvariant
%ignore data;
%ignore setData;

//QStringList
//%ignore setVerticalHeaderLabels;
//%ignore setHorizontalHeaderLabels;

//qlist
%ignore selectedRanges;
%ignore selectedItems;
%ignore findItems;

//remote enums
EnumMaps(QAbstractItemView, ScrollHint, ErrMode)
EnumImport(QAbstractItemView, ScrollHint, QtAbstractItemView, 0)

//remote enums
EnumMaps(QItemSelectionModel, SelectionFlags, ErrMode)
EnumImport(QItemSelectionModel, SelectionFlags, QtItemSelectionModel, ErrMode)

//not used in this module
//EnumMaps(QTableWidget::ItemType, ItemType, 1)

//EnumFlagsImport(QItemSelectionModel::SelectionFlags , SelectionFlags)

%apply ClassIn {const QTableWidgetSelectionRange &};
%apply ClassIn {const QTableWidgetItem &};
%apply ClassIn {const QTableWidgetItem *};
%apply ClassIn {QTableWidgetItem *};

//Here we have to specialise the map since otherwise the constructor
//for qtablewidgetitem or qtablewidget is ruined

%apply ClassReturn {QTableWidget *tableWidget};

%apply ClassReturn {QTableWidgetItem *item};
%apply ClassReturn {QTableWidgetItem *takeItem};
%apply ClassReturn {QTableWidgetItem *verticalHeaderItem};
%apply ClassReturn {QTableWidgetItem *takeVerticalHeaderItem};
%apply ClassReturn {QTableWidgetItem *horizontalHeaderItem};
%apply ClassReturn {QTableWidgetItem *takeHorizontalHeaderItem};
%apply ClassReturn {QTableWidgetItem *currentItem};
%apply ClassReturn {QTableWidgetItem *itemAt};
%apply ClassReturn {QTableWidgetItem *itemPrototype};

%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget%}

DoType(QColor,QtColor)
DoType(QBrush,QtBrush)
DoType(QFont,QtFont)
DoType(QIcon,QtIcon)
DoType(QStringList,QtStringList)

%include <QtGui/qtablewidget.h>
