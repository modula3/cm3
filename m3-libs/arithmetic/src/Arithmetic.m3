MODULE Arithmetic;
(** Arithmetic for Modula-3, see doc for details

*)

IMPORT Atom;

<* UNUSED *>
CONST
  Module = "Arithmetic.";

PROCEDURE Raise (err: ErrorRoot; msg: TEXT; oldErr: ErrorRoot := NIL; )
  RAISES {Error} =
  BEGIN
    err.head := Atom.FromText(msg);
    err.tail := oldErr;
    RAISE Error(err);
  END Raise;

BEGIN
END Arithmetic.
