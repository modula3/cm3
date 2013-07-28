%module QtComboBox

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qcombobox.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QComboBox;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

//Local Enums
EnumMaps(QComboBox, InsertPolicy, ErrMode)
EnumMaps(QComboBox, SizeAdjustPolicy, ErrMode)

%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget%}

DoType(QStringList,QtStringList)
DoType(QWidget,QtWidget)
DoType(QIcon,QtIcon)
DoType(QLineEdit,QtLineEdit)
DoType(QAbstractItemView,QtAbstractItemView)
DoType(QAbstractItemDelegate,QtAbstractItemDelegate)
DoType(QAbstractItemModel,QtAbstractItemModel)
DoType(QModelIndex,QtAbstractItemModel)

DoType(QVariant,QGuiStubs)
DoType(QValidator,QGuiStubs)
DoType(QCompleter,QGuiStubs)


%include <QtGui/qcombobox.h>
