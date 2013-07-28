%module QtTextBrowser

%include "m3qt.i"
%include "common.i"

%import "QtTextEdit.i"

%{
#include <QtGui/qtextbrowser.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtTextEdit IMPORT QTextEdit;

TYPE
  T = QTextBrowser;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
FROM QtString IMPORT QString;
%}

//fix qvariant
%ignore loadResource;

//fix stringlists
%ignore searchPaths;
%ignore setSearchPaths;

%apply ClassIn     {const QUrl &};
%apply ClassReturn {QUrl};
%apply ClassIn     {QWidget *};

%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget%}
%typemap("m3wrapretvar:import")  QUrl      %{QGuiStubs QUrl%}


%include <QtGui/qtextbrowser.h>

