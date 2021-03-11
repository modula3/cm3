(* $Id$ *)
MODULE TypeCM3 EXPORTS Type;
IMPORT Atom, Value;

VAR nullAtm: Atom.T;

VAR first, last := NEW(REF LONGINT);

BEGIN
  first^ := FIRST(LONGINT);
  last^ := LAST(LONGINT);

  nullAtm := Atom.FromText("");
  longint := NEW(Subrange, name := NEW(Qid, intf := nullAtm,
                                       item := Atom.FromText("LONGINT")),
                 min := NEW(Value.Longint, val := first),
                 max := NEW(Value.Longint, val := last));
  longint.base := longint;
END TypeCM3.
