%module QtAbstractButton

%include "m3qt.i"
%include "common.i"

%import "QtWidget.i"

%{
#include <QtGui/qabstractbutton.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtWidget IMPORT QWidget;
TYPE
  T = QAbstractButton;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtByteArray IMPORT QByteArray;
%}

/*

this is an abstract class since there is a pure virtual function
protected:
    virtual void paintEvent(QPaintEvent *e) = 0;
since the =0 is there then its abstract and swig does not generate
constructors for abstract classes

so problem is getting qwidget imported have to use kludge

*/


//%typemap("m3wrapintype:import")  const QSize & %{QtWidget QWidget%}


%include <QtGui/qabstractbutton.h>
