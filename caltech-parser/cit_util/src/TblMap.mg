(* $Id$ *)

GENERIC MODULE TblMap(Tbl, Map);
IMPORT MapError;

REVEAL
  T = Public BRANDED Brand OBJECT
    t           : Tbl.T;
    haveDefault := FALSE;
    default     : Map.Result;
  OVERRIDES
    eval := Eval;
    tbl := Tabl;

    error := DefError;
  END;

PROCEDURE DefError(<*UNUSED*>t : T; <*UNUSED*>op : Map.Argument) 
  RAISES { MapError.E } =
  BEGIN RAISE MapError.E(NIL) END DefError;

PROCEDURE Tabl(t : T) : Tbl.T = BEGIN RETURN t.t END Tabl;

PROCEDURE Wrap(tbl : Tbl.T; t : T) : Map.T =
  BEGIN 
    IF t = NIL THEN 
      RETURN NEW(T, t := tbl)
    ELSE 
      t.t := tbl;
      RETURN t
    END
  END Wrap;

PROCEDURE WrapWithDefault(tbl : Tbl.T; default : Map.Result) : Map.T =
  BEGIN 
    RETURN NEW(T, t := tbl, haveDefault := TRUE, default := default) 
  END WrapWithDefault;

PROCEDURE Eval(t : T; from : Map.Argument) : Map.Result 
  RAISES { MapError.E } =
  VAR 
    res : Map.Result;
    fnd := t.t.get(from, res);
  BEGIN
    IF NOT fnd THEN
      IF t.haveDefault THEN RETURN t.default ELSE t.error(from) END
    END;
    RETURN res
  END Eval;

BEGIN END TblMap.
