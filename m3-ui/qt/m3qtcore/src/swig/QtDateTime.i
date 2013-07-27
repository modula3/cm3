%module QtDateTime

%include "m3qt.i"
%include "common.i"

%import "QtNamespace.i"

%{
#include <QtCore/qdatetime.h>
%}


%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtString IMPORT QString;
FROM QtByteArray IMPORT QByteArray;
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{

%}


%rename(Assign) QTime::operator=;
%rename(Assign) QDate::operator=;
%rename(Assign) QDateTime::operator=;

%rename(Equals) QTime::operator==;
%rename(Equals) QDate::operator==;
%rename(Equals) QDateTime::operator==;
%rename(NotEquals) QTime::operator!=;
%rename(NotEquals) QDate::operator!=;
%rename(NotEquals) QDateTime::operator!=;

%rename(LessThan) QTime::operator<;
%rename(LessThan) QDate::operator<;
%rename(LessThan) QDateTime::operator<;
%rename(GreaterThan) QTime::operator>;
%rename(GreaterThan) QDate::operator>;
%rename(GreaterThan) QDateTime::operator>;

%rename(LessThanEquals) QTime::operator<=;
%rename(LessThanEquals) QDate::operator<=;
%rename(LessThanEquals) QDateTime::operator<=;
%rename(GreaterThanEquals) QTime::operator>=;
%rename(GreaterThanEquals) QDate::operator>=;
%rename(GreaterThanEquals) QDateTime::operator>=;

//gone in v5
%ignore gregorianToJulian;
%ignore julianToGregorian;

%rename(QTime_FromString) QTime::fromString;
%rename(QDate_FromString) QDate::fromString;
%rename(QDate_IsValid) QDate::isValid;

//this renames the whold enum
//%rename(MonthNameT) QDate::MonthNameType;
//This renames an enum item in this case to stop
//a symbol clash with QNamespace
%rename(ADateFormat) DateFormat;

%apply  intvar* {int *yearNum};
%apply  intvar* {int *year, int* month, int *day};

//Local Enums
//EnumMaps(QDate::MonthNameType, MonthNameType, ErrMode)
EnumMaps(QDate, MonthNameType, ErrMode)

%apply ClassIn {const QDate &};
%apply ClassReturn {QDate};
%apply ClassIn {const QTime &};
%apply ClassReturn {QTime};
%apply ClassIn {const QDateTime &};
%apply ClassReturn {QDateTime};
%apply ClassReturn {QDateTime &};

%include <QtCore/qdatetime.h>