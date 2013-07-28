%module QtItemSelectionModel

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtGui/qitemselectionmodel.h>
#define SelectionFlags QItemSelectionModel::SelectionFlags
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QItemSelectionModel;

  SelectionFlags = INTEGER;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//dummy implementation only valid for MSVC
%ignore qHash;

%ignore operator ==;
%ignore operator !=;

//Local Enums
EnumMaps(QItemSelectionModel, SelectionFlag, ErrMode)

//Local Flags
EnumFlags(QItemSelectionModel::SelectionFlags, SelectionFlags)
EnumFlags(SelectionFlags, SelectionFlags)

//Local classes
%apply ClassIn {QItemSelectionRange *};
%apply ClassIn {const QItemSelectionRange &};
%apply ClassIn {QItemSelectionRange *}


%apply ClassIn {QItemSelectionModel *};
%apply ClassIn {const QItemSelectionModel &};
%apply ClassIn {QItemSelectionModel *};

//Here we have to specialise the map since otherwise the constructor
//for qtablewidgetitem or qtablewidget is ruined

%apply ClassReturn {QItemSelectionRange intersect}
%apply ClassReturn {QItemSelectionRange intersected}

%apply ClassIn {const QItemSelection &};
%apply ClassIn {QItemSelection *};
%apply ClassReturn {QItemSelection selection}


DoType(QObject,QtObject)
DoType(QAbstractItemModel,QtAbstractItemModel)
DoType(QModelIndex,QtAbstractItemModel)

DoType(QModelIndexList,QGuiStubs)


//fixme the template for itemselectionrange

%include <QtGui/qitemselectionmodel.h>
