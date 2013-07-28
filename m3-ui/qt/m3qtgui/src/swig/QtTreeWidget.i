%module QtTreeWidget

%include "m3qt.i"
%include "common.i"

%import "QtTreeView.i"

%{
#include <QtGui/qtreewidget.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtTreeView IMPORT QTreeView;

TYPE
  T = QTreeWidget;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

%ignore operator<;
%ignore operator=;

//selectionmodel not done
%ignore setCurrentItem;

//qlist
%ignore addChildren;
%ignore insertChildren;
%ignore takeChildren;
%ignore insertTopLevelItems;
%ignore addTopLevelItems;
%ignore selectedItems;
%ignore findItems;


//Local enums
EnumMaps(QTreeWidgetItem, ChildIndicatorPolicy, ErrMode)

//remote enums
EnumMaps(QAbstractItemView, ScrollHint, ErrMode)
EnumImport(QAbstractItemView, ScrollHint, QtAbstractItemView, 0)

//Local classes
%apply ClassIn {const QTreeWidgetItem &};
%apply ClassIn {QTreeWidgetItem *};
%apply ClassIn {QTreeWidget *};

//Here we have to specialise the map since otherwise the constructor
//for qtreewidgetitem or qtreewidget is ruined

%apply ClassReturn {QTreeWidgetItem *invisibleRootItem};
%apply ClassReturn {QTreeWidgetItem *topLevelItem};
%apply ClassReturn {QTreeWidgetItem *takeTopLevelItem};
%apply ClassReturn {QTreeWidgetItem *currentItem};
%apply ClassReturn {QTreeWidgetItem *headerItem};
%apply ClassReturn {QTreeWidgetItem *itemAt};
%apply ClassReturn {QTreeWidgetItem *itemAbove};
%apply ClassReturn {QTreeWidgetItem *itemBelow};

%apply ClassReturn {QTreeWidget *treeWidget};

%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget%}

DoType(QColor,QtColor)
DoType(QBrush,QtBrush)
DoType(QFont,QtFont)
DoType(QIcon,QtIcon)
DoType(QStringList,QtStringList)

DoType(QVariant,QGuiStubs)

%include <QtGui/qtreewidget.h>
