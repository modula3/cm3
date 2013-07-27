%module QtAbstractItemModel

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

//warning changed h file added name index to qhash function
//but ignoring it anyway
%{
#include <QtCore/qabstractitemmodel.h>
#include <QtCore/qstringlist.h>
#include <QtCore/qsize.h>
%}


%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%insert(m3wrapintf) %{
%}

%ignore operator==;
%ignore operator!=;
%ignore operator<;
%ignore operator=;
%ignore operator const QModelIndex &;

//qlist
%ignore data;
%ignore headerData;
%ignore itemData;
%ignore setItemData;
//qhash
%ignore roleNames;

//void * and dont need anything internal
%ignore internalPointer;

//dont need hashing dont think
%ignore qHash;

//internal classes
%apply ClassIn     {const QModelIndex &};
%apply ClassReturn {QModelIndex};
%apply ClassReturn {QAbstractItemModel *};

%apply ClassIn     {const QPersistentModelIndex & other};
//qhash changed h file parm
//%apply ClassIn     {const QPersistentModelIndex &index};

DoType(QSize,QtSize)
DoType(QObject,QtObject)
DoType(QStringList,QtStringList)

DoType(QVariant,QCoreStubs)
DoType(QMimeData,QCoreStubs)
DoType(QModelIndexList,QCoreStubs)

%include <QtCore/qabstractitemmodel.h>