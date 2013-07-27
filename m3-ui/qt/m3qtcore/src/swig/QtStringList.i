%module QtStringList

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"
%import "QtList.i"

//Dont need regular expressions
# define  QT_NO_REGEXP
# define  Q_INLINE_TEMPLATE

%{
#include <QtCore/qstringlist.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtList IMPORT QStringListBase;
TYPE
  T = QStringList;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtString IMPORT QString;
FROM QtByteArray IMPORT QByteArray;
%}


%ignore QtPrivate;

//ignore copy constructor
%ignore QStringList::QStringList(const QStringList &l) ;

%ignore contains;

%ignore operator +;
%ignore operator <<;

//In baseclasss and generating strange names
%ignore indexOf;
%ignore lastIndexOf;

%apply ClassIn     {const QString &};

%apply ClassIn     {const QRegExp &};
%apply ClassIn     {QRegExp &};
%apply ClassReturn {QBool};

%apply ClassIn     {const QStringList &};
%apply ClassReturn {QStringList};
%apply ClassReturn {QStringList &};


//Need this to allocate a new Qstring based on a text
%typemap("m3wrapargvar")   const QString &  %{
$1qtmp := NEW(QString).initQString($1_name);
$1tmp :=  LOOPHOLE($1qtmp.cxxObj,ADDRESS);%}

//ctypes needed
%typemap("ctype") QStringList     %{QStringList *%}

%typemap("m3wrapintype:import")  const QRegExp & %{QStubs  QRegExp%}

%include <QtCore/qstringlist.h>


