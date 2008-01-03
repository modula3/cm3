(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Sym.i3,v 1.2 2001-09-19 15:31:35 wagner Exp $ *)

INTERFACE Sym;
IMPORT TextIntTbl;
IMPORT TextBooleanTbl;
IMPORT TextPrecTbl;
IMPORT Prec;
CONST
  Brand = "Sym";
TYPE
  T <: REFANY;

PROCEDURE FromText(t: TEXT): T;
(* return named symbol that can have code and start flag assigned *)

PROCEDURE FromChar(c: CHAR): T;
(* return a symbol for c *)


PROCEDURE AllocCode(a: T; into: TextIntTbl.T; VAR lastCode: INTEGER);
(* if a is not a char symbol and name is not in table,
   increment lastCode and bind to name *)

PROCEDURE SetAttrs(a: T; start: TextBooleanTbl.T;
                   const: TextIntTbl.T);
(* read attributes from tables *)


PROCEDURE GetName(a: T): TEXT;
(* name or "@c" for char *)

PROCEDURE GetCode(a: T): INTEGER;
(* can call after AllocCode has been called *)

PROCEDURE IsStart(a: T): BOOLEAN;
PROCEDURE IsConst(a: T): BOOLEAN;
(* can call after SetAttrs has been called *)

PROCEDURE GetPrec(a: T; prec: TextPrecTbl.T): Prec.T;
(* return a precedence applying to a, otherwise return NIL *)

PROCEDURE Format(a: T): TEXT;
PROCEDURE Equal(a, b: T): BOOLEAN;
END Sym.
