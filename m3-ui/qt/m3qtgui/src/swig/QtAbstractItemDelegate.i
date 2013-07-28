%module QtAbstractItemDelegate

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtGui/qabstractitemdelegate.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtObject IMPORT QObject;
TYPE
  T = QAbstractItemDelegate;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtString IMPORT QString;
FROM QtByteArray IMPORT QByteArray;
%}

//Local Enums
EnumMaps(QAbstractItemDelegate, EndEditHint, ErrMode)

DoType(QSize,QtSize)
DoType(QObject,QtObject)
DoType(QWidget,QtWidget)
DoType(QAbstractItemView,QtAbstractItemView)
DoType(QAbstractItemModel,QtAbstractItemModel)
DoType(QModelIndex,QtAbstractItemModel)
DoType(QFontMetrics,QtFontMetrics)

DoType(QStyleOptionViewItem,QGuiStubs)
DoType(QHelpEvent,QGuiStubs)

%include <QtGui/qabstractitemdelegate.h>
