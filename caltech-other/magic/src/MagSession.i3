(* $Id$ *)

INTERFACE MagSession;
IMPORT MagCellExtendable, Word;

TYPE T = MagCellExtendable.CellWriter;

CONST Brand = "MagSession";

PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE Hash(a : T) : Word.T;

END MagSession.
