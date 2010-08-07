(*--------------------------------------------------------------------------*)
GENERIC MODULE Tuple(Elem);

(*--------------------------------------------------------------------------*)
REVEAL
  T = Public BRANDED Brand OBJECT 
    fst, scd : Elem.T 
  OVERRIDES
    init := Init;
    getFst := GetFst;
    getScd := GetScd
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE Init (sf : T; fst, scd : Elem.T) : T =
  BEGIN
    sf.fst := fst;
    sf.scd := scd;
    RETURN sf
  END Init;

(*--------------------------------------------------------------------------*)
PROCEDURE GetFst (sf : T) : Elem.T =
  BEGIN
    RETURN sf.fst
  END GetFst;

(*--------------------------------------------------------------------------*)
PROCEDURE GetScd (sf : T) : Elem.T =
  BEGIN
    RETURN sf.scd
  END GetScd;

(*--------------------------------------------------------------------------*)
BEGIN
END Tuple.
