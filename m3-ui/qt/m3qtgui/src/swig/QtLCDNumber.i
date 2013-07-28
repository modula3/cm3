%module QtLCDNumber

%include "m3qt.i"
%include "common.i"

%import "QtFrame.i"

%{
#include <QtGui/qlcdnumber.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
FROM QtFrame IMPORT QFrame;

TYPE
  T = QLCDNumber;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//Local Enums
EnumMaps(QLCDNumber, Mode, ErrMode)
EnumMaps(QLCDNumber, SegmentStyle, ErrMode)

%include <QtGui/qlcdnumber.h>
