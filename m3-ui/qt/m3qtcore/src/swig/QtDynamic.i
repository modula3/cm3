%module QtDynamic

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include "dynamicqobject.h"
%}

%insert(m3rawintf) %{
TYPE
  CallbackProc = PROCEDURE(obj : ADDRESS; args : ADDRESS);

%}

%insert(m3wrapintf) %{
TYPE

  T = DynamicQObject;

  CallbackProc = PROCEDURE(obj : ADDRESS; args : ADDRESS);

PROCEDURE ConvertInt(args : ADDRESS; parmNo : INTEGER) : INTEGER;
PROCEDURE ConvertObj(args : ADDRESS; parmNo : INTEGER) : ADDRESS;

%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
IMPORT M3toC;

PROCEDURE ConvertInt(args : ADDRESS; parmNo : INTEGER) : INTEGER =
TYPE
 arrT =  UNTRACED REF ARRAY[0..10] OF ADDRESS;
 refCInt = REF C.int;
VAR
  ci : C.int;
BEGIN
  (* the conversion to c int *)
  ci := LOOPHOLE(LOOPHOLE(args,arrT)[parmNo],refCInt)^;
  RETURN ci;
END ConvertInt;

PROCEDURE ConvertObj(args : ADDRESS; parmNo : INTEGER) : ADDRESS =
TYPE
 arrT =  UNTRACED REF ARRAY[0..10] OF ADDRESS;
BEGIN
  RETURN LOOPHOLE(args,arrT)[parmNo];
END ConvertObj;

%}


%typemap("m3rawintype")   void (*fn) (void *,void **) %{CallbackProc%}
%typemap("m3wrapintype")  void (*fn) (void *,void **) %{CallbackProc%}


%typemap("m3rawintype")   QMetaObject::Call    %{INTEGER%}
%typemap("m3wrapintype")  QMetaObject::Call    %{INTEGER%}

%typemap("m3rawintype")   void **    %{REF ADDRESS%}
%typemap("m3wrapintype")  void **    %{REF ADDRESS%}
%typemap("m3rawintype")   void *     %{ADDRESS%}
%typemap("m3wrapintype")  void *     %{ADDRESS%}

%apply ClassReturn {AbstractDynamicSlot *};
%apply ClassIn    {DynamicQObject *parent};


%typemap("m3wrapintype:import")  QObject * %{QtObject $1_basetype%}

%include "dynamicqobject.h"