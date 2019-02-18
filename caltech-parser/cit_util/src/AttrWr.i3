(* $Id$ *)

INTERFACE AttrWr;
IMPORT Wr, Word, TextRefTbl;

TYPE 
  T = RECORD
    wr    : Wr.T;
    attrs : TextRefTbl.T := NIL;
  END;

CONST Brand = "AttrWr";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

CONST Hash : PROCEDURE (READONLY a : T) : Word.T = NIL;

END AttrWr.
