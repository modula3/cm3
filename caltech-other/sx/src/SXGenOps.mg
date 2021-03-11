(* $Id$ *)

GENERIC MODULE SXGenOps(Elem, Elem_ElemFuncOps);
IMPORT SXBool, SXInt, SXBool_IntFuncOps;

PROCEDURE Index(on : SXInt.T; READONLY arr : ARRAY OF Elem.T) : Elem.T =
  BEGIN
    RETURN Elem_ElemFuncOps.IAryFunc(on,arr,IndexB,"Index")
  END Index;

PROCEDURE IndexB(i : INTEGER; READONLY a : ARRAY OF Elem.Base) : Elem.Base =
  BEGIN RETURN a[i] END IndexB;

PROCEDURE Choose(on : SXBool.T; ifTrue, ifFalse : Elem.T) : Elem.T =
  BEGIN
    WITH i = BoolToInt(on) DO
      RETURN Index(i, ARRAY [0..1] OF Elem.T { ifFalse, ifTrue })
    END
  END Choose;

PROCEDURE BoolToInt(bool : SXBool.T) : SXInt.T =
  BEGIN
    RETURN SXBool_IntFuncOps.UnaryFunc(bool,BoolToIntB,"BoolToInt")
  END BoolToInt;

PROCEDURE BoolToIntB(b : BOOLEAN) : INTEGER = 
  BEGIN RETURN ORD(b) END BoolToIntB;

BEGIN END SXGenOps.
