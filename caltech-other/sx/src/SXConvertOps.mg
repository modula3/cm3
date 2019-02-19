(* $Id$ *)

GENERIC MODULE SXConvertOps(Elem, Int_ElemFuncOps, Elem_IntFuncOps, Elem_BoolFuncOps);
IMPORT SXInt, SXBool;

PROCEDURE FloatB(a : SXInt.Base) : Elem.Base =
  BEGIN RETURN FLOAT(a, Elem.Base) END FloatB;

PROCEDURE Float(a : SXInt.T) : Elem.T =
  BEGIN RETURN Int_ElemFuncOps.UnaryFunc(a, FloatB, "Float") END Float;

PROCEDURE RoundB(a : Elem.Base) : SXInt.Base =
  BEGIN RETURN ROUND(a) END RoundB;

PROCEDURE Round(a : Elem.T) : SXInt.T =
  BEGIN RETURN Elem_IntFuncOps.UnaryFunc(a, RoundB, "Round") END Round;

PROCEDURE TruncB(a : Elem.Base) : SXInt.Base =
  BEGIN RETURN TRUNC(a) END TruncB;

PROCEDURE Trunc(a : Elem.T) : SXInt.T =
  BEGIN RETURN Elem_IntFuncOps.UnaryFunc(a, TruncB, "Trunc") END Trunc;

PROCEDURE FloorB(a : Elem.Base) : SXInt.Base =
  BEGIN RETURN FLOOR(a) END FloorB;

PROCEDURE Floor(a : Elem.T) : SXInt.T =
  BEGIN RETURN Elem_IntFuncOps.UnaryFunc(a, FloorB, "Floor") END Floor;

PROCEDURE CeilingB(a : Elem.Base) : SXInt.Base =
  BEGIN RETURN CEILING(a) END CeilingB;

PROCEDURE Ceiling(a : Elem.T) : SXInt.T =
  BEGIN RETURN Elem_IntFuncOps.UnaryFunc(a, CeilingB, "Ceiling") END Ceiling;

PROCEDURE IntRangeB(a : Elem.Base) : SXBool.Base =
  BEGIN RETURN a >= FLOAT(FIRST(INTEGER),Elem.Base) AND 
               a <= FLOAT(LAST(INTEGER), Elem.Base)   END IntRangeB;

PROCEDURE IntRange(a : Elem.T) : SXBool.T =
  BEGIN RETURN Elem_BoolFuncOps.UnaryFunc(a, IntRangeB, "IntRange") END IntRange;

PROCEDURE CardRangeB(a : Elem.Base) : SXBool.Base =
  BEGIN RETURN a >= FLOAT(FIRST(INTEGER),Elem.Base) AND 
               a <= FLOAT(LAST(INTEGER), Elem.Base)   END CardRangeB;

PROCEDURE CardRange(a : Elem.T) : SXBool.T =
  BEGIN RETURN Elem_BoolFuncOps.UnaryFunc(a, CardRangeB, "CardRange") END CardRange;

BEGIN END SXConvertOps.
