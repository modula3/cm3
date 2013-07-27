%module QtList

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

# define  QT_NO_STL
# define  Q_INLINE_TEMPLATE

%{
#include <QtCore/qstringlist.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QStringListBase;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtString IMPORT QString;
FROM QtByteArray IMPORT QByteArray;
%}

%ignore QVector;

%ignore QListData;

%ignore QtPrivate;
%ignore QStringList::QStringList(const QList<QString> &l);

%ignore contains;

%ignore operator +;
%ignore operator <<;
%ignore operator =;
%ignore operator ==;
%ignore operator !=;
%ignore operator [];
%ignore operator +=;
%ignore operator ==;

%ignore toVector;
%ignore fromVector;
%ignore toSet;
%ignore fromSet;

%ignore  QList(const QList<T> &l);
%ignore  append(const QList<T> &t);

%ignore begin;
%ignore end;
%ignore constBegin;
%ignore constEnd;

%ignore mid;
%ignore swap;
%ignore isSharedWith;

%ignore  insert(iterator before, const T &t);
%ignore erase(iterator pos);
%ignore erase(iterator first, iterator last);


//raw in type
%typemap("m3rawintype") QList < QString >     *  %{QStringListBase%}
%typemap("m3rawintype") QList < QString >        %{QStringListBase%}

//raw ret type
%typemap("m3rawrettype") QList < QString >     *  %{QStringListBase%}
%typemap("m3rawrettype") QList < QString >     &  %{QStringListBase%}
%typemap("m3rawrettype") QList < QString >        %{QStringListBase%}

//wrap in type
%typemap("m3wrapintype") QList < QString >     *  %{QStringListBase%}
%typemap("m3wrapintype") QList < QString >     &  %{QStringListBase%}
%typemap("m3wrapintype") QList < QString >        %{QStringListBase%}

//wrap ret type
%typemap("m3wraprettype") QList < QString >     * %{QStringListBase%}
%typemap("m3wraprettype") QList < QString >     & %{QStringListBase%}
%typemap("m3wraprettype") QList < QString >       %{QStringListBase%}

//wrap out type
%typemap("m3wrapouttype") QList < int >       %{QStringListBase%}

//make the ret check tmap more specific

%typemap("m3wrapretcheck")  QList < QString >  *  %{
  self.cxxObj := result;
  EVAL WeakRef.FromRef(self,Cleanup_QStringListBase);
%};


%include <QtCore/qlist.h>

%template(QStringListBase) QList <QString>;

