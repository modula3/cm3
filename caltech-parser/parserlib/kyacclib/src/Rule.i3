(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Rule.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE Rule;
IMPORT Sym;
IMPORT SymList;
IMPORT Prec;
IMPORT TextPrecTbl;
IMPORT TextBooleanTbl;
IMPORT TextIntTbl;
IMPORT CharRange;
IMPORT Rd;
CONST
  Brand = "Rule";
TYPE
  T <: Public;
  Public = OBJECT
    length: INTEGER;
    number: INTEGER;
    name: TEXT;
    syms: SymList.T;
    return: Sym.T;
    prec: Prec.T;
  END;

PROCEDURE CountParams(a: T): INTEGER;

PROCEDURE FromRd(rd: Rd.T; return: Sym.T;
                 allowedChars: CharRange.T;
                 number: INTEGER): T;

PROCEDURE LookupSyms(a: T;
                     prec: TextPrecTbl.T;
                     start: TextBooleanTbl.T;
                     codes: TextIntTbl.T;
                     const: TextIntTbl.T;
                     VAR lastCode: INTEGER);

PROCEDURE Format(a: T; form: TEXT; last: BOOLEAN := TRUE): TEXT;
(* On first call, strings are cached.
   -> must renumber before first format call *)

PROCEDURE Equal(a, b: T): BOOLEAN;


PROCEDURE Compare(a, b: T; assoc: BOOLEAN := FALSE): [-1 .. 1];
(* compare rule precedence *)

PROCEDURE Number(a: T): INTEGER;
(* a.number, or 0 if NIL *)

END Rule.
