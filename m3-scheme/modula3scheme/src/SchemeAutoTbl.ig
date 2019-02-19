(* $Id$ *)

GENERIC INTERFACE SchemeAutoTbl(Tbl, Key, Value);

TYPE (* there are just here to make the compiler shut up for now *)
  Table = Tbl.T;
  K = Key.T;
  V = Value.T;

PROCEDURE Register();

END SchemeAutoTbl.
