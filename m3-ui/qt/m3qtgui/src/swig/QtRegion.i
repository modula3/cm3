%module QtRegion

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtGui/qregion.h>
%}

%insert(m3rawintf) %{
%}


%insert(m3wrapintf) %{
TYPE
  T = QRegion;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

%ignore rects;
%ignore qt_region_strictContains; //friend class

%ignore operator+;
%ignore operator&;
%ignore operator-;
%ignore operator|;
%ignore operator^;

%ignore operator|=;
%ignore operator+=;
%ignore operator&=;
%ignore operator-=;
%ignore operator^=;
%ignore operator==;
%ignore operator!=;

%ignore operator=;
%ignore operator<<;
%ignore operator>>;
%ignore operator!;
%ignore operator QVariant;


//Enums local
EnumMaps(QRegion, RegionType, ErrMode)

//Local classes
%apply ClassIn {QRegion &};
%apply ClassIn {const QRegion &};
%apply SelfReturn {QRegion};


//We avoid importing QBitmap since it creates circular imports later
//%apply ClassIn {const QBitmap &};
//So we override the tmaps
%typemap("m3rawintype")   const QBitmap &      %{REFANY%}
%typemap("m3wrapintype")  const QBitmap &      %{REFANY%}
%typemap("m3wrapinmode")  const QBitmap &      %{%}


DoType(QPoint,QtPoint)
DoType(QRect,QtRect)
DoType(QPolygon,QtPolygon)

%include <QtGui/qregion.h>
