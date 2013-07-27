%module QtByteArray

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

%{
#include <QtCore/qbytearray.h>
%}


%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%insert(m3wrapintf) %{
TYPE
  T = QByteArray;
%}


%ignore operator=;
%ignore operator+=;
%ignore operator+;
%ignore operator>;
%ignore operator<;
%ignore operator>=;
%ignore operator<=;
%ignore operator[];

%ignore operator==;
%ignore operator!=;
%ignore operator+;
%ignore operator-;
%ignore operator*;
%ignore operator*;
%ignore operator/;

%ignore operator const char*;
%ignore operator const void*;
%ignore operator char;

%ignore qvsnprintf;
%ignore qsnprintf;
%ignore append;
%ignore indexOf;
%ignore insert;
%ignore replace;
%ignore lastIndexOf;
%ignore contains;
%ignore split;
%ignore begin;
%ignore end;
%ignore constBegin;
%ignore constEnd;
%ignore data_ptr;
%ignore qChecksum;
%ignore leftJustified;
%ignore rightJustified;
%ignore setNum;
%ignore number;
%ignore toLongLong;
%ignore toULongLong;
%ignore qCompress;
%ignore qUncompress;
%ignore at;
%ignore swap;

%ignore qstrcmp;
%ignore qstrdup;
%ignore qstrnlen;
%ignore qstrnicmp;
%ignore qstrlen;
%ignore qstrcpy;
%ignore qstricmp;
%ignore qstrncmp;
%ignore qstrncpy;

%ignore QByteRef;

//can handle static types with the selfreturn apply
//%ignore fromRawData;
//%ignore fromBase64;
//%ignore fromHex;
//%ignore fromPercentEncoding;



%typemap("m3wrapargraw")     char   %{ORD($1_name)%}

%apply ClassIn     {const QByteArray &};
%apply SelfReturn  {QByteArray};
%apply SelfReturn  {QByteArray &};

//ensure class returns are pointers -- why not in common??
%typemap("ctype") QByteArray  %{QByteArray *%}

%include <QtCore/qbytearray.h>
