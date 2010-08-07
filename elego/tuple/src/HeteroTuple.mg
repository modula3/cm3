(*--------------------------------------------------------------------------*)
GENERIC MODULE HeteroTuple (FstElem, ScdElem);

(*--------------------------------------------------------------------------*)
REVEAL
  T = Public BRANDED Brand OBJECT 
    fst : FstElem.T; 
    scd : ScdElem.T 
  OVERRIDES
    init := Init;
    getFst := GetFst;
    getScd := GetScd
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE Init (sf : T; fst : FstElem.T; scd : ScdElem.T) : T =
  BEGIN
    sf.fst := fst;
    sf.scd := scd;
    RETURN sf
  END Init;

(*--------------------------------------------------------------------------*)
PROCEDURE GetFst (sf : T) : FstElem.T =
  BEGIN
    RETURN sf.fst
  END GetFst;

(*--------------------------------------------------------------------------*)
PROCEDURE GetScd (sf : T) : ScdElem.T =
  BEGIN
    RETURN sf.scd
  END GetScd;

(*--------------------------------------------------------------------------*)
BEGIN
END HeteroTuple.
