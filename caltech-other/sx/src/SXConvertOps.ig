(* $Id$ *)

GENERIC INTERFACE SXConvertOps(Elem);
IMPORT SXInt, SXBool;

PROCEDURE Float(a : SXInt.T) : Elem.T;

PROCEDURE Round(a : Elem.T) : SXInt.T;
PROCEDURE Trunc(a : Elem.T) : SXInt.T;
PROCEDURE Floor(a : Elem.T) : SXInt.T;
PROCEDURE Ceiling(a : Elem.T) : SXInt.T;

PROCEDURE IntRange(a : Elem.T) : SXBool.T;
(* is the given Elem convertable to an Int? *)

PROCEDURE CardRange(a : Elem.T) : SXBool.T;
(* is the given Elem convertable to a Card? *)

END SXConvertOps.
