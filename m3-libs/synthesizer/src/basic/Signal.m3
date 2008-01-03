MODULE Signal;

IMPORT Atom, AtomList;

PROCEDURE Exit (<* UNUSED *> SELF: T; ) =
  BEGIN
  END Exit;


REVEAL
  ErrorRoot =
    ErrorRootPublic BRANDED OBJECT OVERRIDES init := ErrorInit; END;

PROCEDURE ErrorInit
  (err: ErrorRoot; msg: TEXT := ""; oldErr: AtomList.T := NIL; ):
  ErrorRoot =
  BEGIN
    err.head := Atom.FromText(msg);
    err.tail := oldErr;
    RETURN err;
  END ErrorInit;


BEGIN
END Signal.
