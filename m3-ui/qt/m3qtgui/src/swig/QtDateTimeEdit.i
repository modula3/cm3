%module QtDateTimeEdit

%include "m3qt.i"
%include "common.i"

%import "QtAbstractSpinBox.i"

%{
#include <QtGui/qdatetimeedit.h>
#define  Sections QDateTimeEdit::Sections
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{

FROM QtAbstractSpinBox IMPORT QAbstractSpinBox;

TYPE
  T = QDateTimeEdit;

  Sections = INTEGER;

%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

/*
  The extend of the same named method is ignored
  and if extend with a new method it is not put in i3 file so that is a
  bug in modula3.cxx.

%extend QDateTimeEdit {

 //this is ignored
 QDateTimeEdit::Sections displayedSections() const;

 //this is not put in object def in i3 or m3 files but procudure decl
 //produced ok so still a bug in modula3.cxx
 void newmethod(int k);

};
*/

/*
  The Sections flag is not properly supported in the cxx. This is a case
  of a local flags which is not fully named with the class descriptor.
  so the declaration in the h file is Sections displayedSections()
  instead of QDateTimeEdit::Sections displayedSections()

*/


//Local Enums
EnumMaps(QDateTimeEdit, Section, ErrMode)

//Local flags
EnumFlags(QDateTimeEdit::Sections, Sections)
EnumFlags(Sections, Sections)

//dont import flags from module in which declared
//EnumFlagsImport(QDateTimeEdit::Sections, QDateTimeEdit_Sections)

//Local classes
%apply ClassIn     {QDateTime &};
%apply ClassIn     {const QDate &};
%apply ClassIn     {const QTime &};
%apply ClassReturn {QDate};
%apply ClassReturn {QTime};
%apply ClassReturn  {QDateTime};

%apply ClassIn     {QCalendarWidget *};
%apply ClassReturn {QCalendarWidget *};

%typemap("m3wrapintype:import")  QWidget * %{QtWidget QWidget%}

%typemap("m3wrapintype:import")  const QDate & %{QtDateTime QDate%}
%typemap("m3wrapintype:import")  const QTime & %{QtDateTime QTime%}
%typemap("m3wrapintype:import")  const QDateTime & %{QtDateTime QDateTime%}
%typemap("m3wrapintype:import") QCalendarWidget * %{QtCalendarWidget QCalendarWidget%}


DoType(QDate,QtDateTime);
DoType(QTime,QtDateTime);
DoType(QDateTime,QtDateTime);


%include <QtGui/qdatetimeedit.h>
