%module QtListWidget

%include "m3qt.i"
%include "common.i"

%import "QtListView.i"

%{
#include <QtGui/qlistwidget.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtListView IMPORT QListView;

TYPE
  T = QListWidget;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

%ignore operator=;
%ignore operator<;

//templates
%ignore selectedItems;
%ignore findItems;

//qvariant
%ignore data;
%ignore setData;

//qstringlist
%ignore insertItems;
%ignore addItems;

//QdropEvent class not done
%ignore dropEvent;

//qitemselectionmodel not done
%ignore setCurrentItem;
%ignore setCurrentRow(int row, QItemSelectionModel::SelectionFlags command);


//Local enums
EnumMaps(QListWidget, ItemType, ErrMode)

//remote enums
EnumMaps(QAbstractItemView, ScrollHint, ErrMode)
EnumImport(QAbstractItemView, ScrollHint, QtAbstractItemView, 0)

//fix selectionmodel
//EnumMaps(QItemSelectionModel, SelectionFlags, ErrMode)

//Local classes
%apply ClassIn {QListWidget *};
%apply ClassIn {const QListWidgetItem &};
%apply ClassIn {QListWidgetItem *};

//Here we have to specialise the map since otherwise the constructor
//for qlistwidgetitem or qlistwidget is ruined

%apply ClassReturn {QListWidget *listWidget};

%apply ClassReturn {QListWidgetItem *item};
%apply ClassReturn {QListWidgetItem *takeItem};
%apply ClassReturn {QListWidgetItem *currentItem};
%apply ClassReturn {QListWidgetItem *itemAt};

DoType(QColor,QtColor)
DoType(QBrush,QtBrush)
DoType(QFont,QtFont)
DoType(QIcon,QtIcon)


%include <QtGui/qlistwidget.h>
