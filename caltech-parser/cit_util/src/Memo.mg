(* $Id$ *)

GENERIC MODULE Memo(Map,Tbl);

REVEAL
  T = Public BRANDED Brand OBJECT
    tbl : Tbl.T;
  OVERRIDES
    init := Init;
    eval := Eval;
  END;

PROCEDURE Init(t : T; tbl : Tbl.T) : T =
  BEGIN
    IF tbl = NIL THEN tbl := NEW(Tbl.Default).init() END;
    t.tbl := tbl;
    RETURN t
  END Init;

PROCEDURE Eval(t : T; x : Map.Argument) : Map.Result =
  VAR 
    res : Map.Result;
  BEGIN
    IF t.tbl.get(x,res) THEN 
      RETURN res 
    ELSE 
      res := t.realEval(x); 
      EVAL t.tbl.put(x,res); 
      RETURN res 
    END
  END Eval;

BEGIN END Memo.
