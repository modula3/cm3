MODULE Arithmetic;
(** Arithmetic for Modula-3, see doc for details

*)

IMPORT Atom;

<* UNUSED *>
CONST
  Module = "Arithmetic.";

PROCEDURE ErrorInit (err   : ErrorRoot;
                     msg   : TEXT        := "";
                     oldErr: ErrorRoot   := NIL; ): ErrorRoot =
  BEGIN
    err.head := Atom.FromText(msg);
    err.tail := oldErr;
    RETURN err;
  END ErrorInit;

BEGIN
END Arithmetic.
