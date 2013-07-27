%module QtString

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

%{
#include <QtCore/qstring.h>
//#include "qstring.h"
%}


%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%insert(m3wrapintf) %{
FROM QtByteArray IMPORT QByteArray;
TYPE
  T = QString;
%}

//this works (constructor renaming) but change common first for bytearray before implement
//also do we need initbyte as well??

//constructor rename to special name for typemaps
//it should be initQString see common.i
%rename ("initQString") QString::QString(const char *ch);

%ignore operator=;
%ignore operator+=;
%ignore operator+;
%ignore operator>;
%ignore operator<;
%ignore operator>=;
%ignore operator<=;
%ignore operator[];
%ignore operator QChar;

%ignore operator==;
%ignore operator!=;

%ignore sprintf;
%ignore vsprintf;


//%ignore QString::QString();
%ignore QString(const QString &);

%ignore QString::QString(const QChar*);
%ignore QString::QString(const QChar*, int);
%ignore QString::QString(QChar);
%ignore QString::QString(int, QChar);
%ignore QString::QString(const QLatin1String &);

%ignore arg;
//%ignore size;
//%ignore count;

%ignore count(QChar c, Qt::CaseSensitivity cs = Qt::CaseSensitive) const;
%ignore count(const QString &, Qt::CaseSensitivity) const;
%ignore count(const QRegExp &) const;
%ignore count(const QStringRef &s, Qt::CaseSensitivity cs = Qt::CaseSensitive) const;
/*
%ignore count(const QString &s, Qt::CaseSensitivity cs = Qt::CaseSensitive) const;
*/

%ignore compare;

%ignore isEmpty;
%ignore resize;
%ignore fill;
%ignore truncate;
%ignore chop;
%ignore capacity;
%ignore reserve;
%ignore squeeze;
%ignore unicode;
%ignore data;
%ignore constData;
%ignore detach;
%ignore isDetached;
%ignore clear;

%ignore number;
%ignore toAscii;
%ignore fromAscii;
%ignore utf16;
%ignore setUtf16;
//%ignore toUtf8;
%ignore fromUtf8;
%ignore toLatin1;
%ignore fromLatin1;
//%ignore toLocal8Bit;
%ignore toUcs4;
%ignore toWCharArray;

%ignore QLatin1String;
%ignore QCharRef;
%ignore QStringRef;

//%ignore length;
%ignore indexOf;
%ignore from;
%ignore lastIndexOf;
%ignore contains;
%ignore section;
%ignore begin;
%ignore end;
%ignore mid;

%ignore left;
%ignore right;
%ignore leftRef;
%ignore rightRef;
%ignore at;
%ignore midRef;
%ignore startsWith;
%ignore endsWith;
%ignore leftJustified;
%ignore rightJustified;
%ignore trunc;
%ignore toLower;
%ignore toUpper;
%ignore caseFolded;
%ignore toCaseFolded;
%ignore trimmed;
%ignore simplified;
%ignore insert;
%ignore len;
%ignore append;
%ignore prepend;
%ignore remove;
%ignore replace;
%ignore split;
%ignore behaviour;
%ignore normalized;
%ignore repeated;
%ignore localeAwareCompare;
%ignore setUnicode;
%ignore setRawData;

%ignore setNum;
%ignore toShort;
%ignore toInt;
%ignore toLong;
%ignore toUShort;
%ignore toUInt;
%ignore toULong;
%ignore toLongLong;
%ignore toULongLong;
%ignore toFloat;
%ignore toDouble;
%ignore constBegin;
%ignore constEnd;
%ignore push_back;
%ignore push_front;
%ignore toStdString;
%ignore toStdWString;
%ignore isNull;
%ignore isSimpleText;
%ignore isRightToLeft;

%ignore fromLocal8Bit;
%ignore fromUtf16;
%ignore fromUcs4;
%ignore fromRawData;
%ignore fromWCharArray;
%ignore fromStdString;
%ignore null;
%ignore fromStdWString;
%ignore ComparisonHelper;

%ignore data_ptr;

%ignore SectionFlag;
%ignore QString(const Null &);
%ignore qStringComparisonHelper;

%ignore NormalizationForm;
%ignore SplitBehavior;


//raw for QString not needed no QString returned yet
//%typemap("m3rawrettype")   QString     %{ADDRESS%}


//raw return typemaps for external classes
%typemap("m3rawrettype")   QChar                 %{ADDRESS%}
%typemap("m3rawrettype")   QByteArray            %{ADDRESS%}
%typemap("m3rawintype")    const  QByteArray &   %{ADDRESS%}
%typemap("m3rawintype")    QLatin1String         %{ADDRESS%}

%apply ClassIn    {const QByteArray &};
%apply SelfReturn {QByteArray};


//NewString_1 is a copy constructor taking a QString which we change to TEXT
//but the classin apply above does not get used but we need this typemap
//no we dont the copy constr should allocate a NEW Qstring which it does now
//can delete these comments at some stage
//%typemap("m3wrapargvar") const QString &  %{$1tmp :=  LOOPHOLE($1_name,ADDRESS);%}

//ctypes needed
%typemap("ctype") QString     %{QString *%}
%typemap("ctype") QByteArray  %{QByteArray *%}

%typemap("m3wrapintype:import")  const QString & %{QtByteArray  QByteArray%}


%include <QtCore/qstring.h>
//#include "qstring.h"
