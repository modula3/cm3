%module QtPicture

%include "m3qt.i"
%include "common.i"

%import "QtPaintDevice.i"

%{
#include <QtGui/qpicture.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtPaintDevice IMPORT QPaintDevice;
TYPE
  T = QPicture;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
FROM QtString IMPORT QString;
%}


//duplicate names rename the static func
%rename(picFormat) QPicture::pictureFormat(const QString &fileName);

//these contain templates or qstringlist which not wrapped
%ignore inputFormats;
%ignore outputFormats;
%ignore inputFormatList;
%ignore outputFormatList;

%ignore operator=;
%ignore operator<<;
%ignore operator>>;

//this defines a function with function parms bit hard at moment
%ignore defineIOHandler;

//data_ptr funny one returns a pointer to something
%typemap("m3rawrettype")    QPicture::DataPtr &    %{ADDRESS%}
%typemap("m3wraprettype")   QPicture::DataPtr &    %{UNTRACED REF CHAR%}
%typemap("m3wrapouttype")   QPicture::DataPtr &    %{UNTRACED REF CHAR%}

//Local classes
%apply ClassIn {QPicture &};
%apply ClassIn {const QPicture &};
%apply ClassReturn {const QPicture &};


//this is an example of an import of 2 classes note the comma after
//the second pair of module class couplets
//%typemap("m3wrapretvar:import") QString    %{QtString  $1_basetype, QtByteArray QByteArray%}


DoType(QRect,QtRect);
DoType(QByteArray,QtByteArray);
DoType(QPaintEngine,QtPaintEngine);

DoType(QIODevice,QGuiStubs);
DoType(QPainter,QGuiStubs);


%include <QtGui/qpicture.h>