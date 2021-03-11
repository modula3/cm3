(* $Id$ *)

GENERIC MODULE MapMap(Map1, Map2);

REVEAL
  T = Public BRANDED Brand OBJECT
    map1 : Map1.T;
  OVERRIDES
    init := Init;
    eval := Eval;
  END;

PROCEDURE Init(t : T; map1 : Map1.T) : T =
  BEGIN t.map1 := map1; RETURN t END Init;

PROCEDURE Eval(t : T; x : Map2.Argument) : Map2.Result =
  BEGIN RETURN t.map1.eval(x) END Eval;

PROCEDURE Wrap(map1 : Map1.T) : T =
  BEGIN RETURN NEW(T).init(map1 := map1) END Wrap;

BEGIN END MapMap.
