(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Sym.m3,v 1.2 2001-09-19 15:31:35 wagner Exp $ *)

MODULE Sym;
IMPORT Prec;
IMPORT Text;
IMPORT TextPrecTbl;
IMPORT TextBooleanTbl;
IMPORT TextIntTbl;
(* IMPORT Fmt; *)
IMPORT CharCodes;
(* IMPORT Term; *)
REVEAL
  T = BRANDED REF RECORD
    name: TEXT; (* NIL means CharSym *)
    code: INTEGER;
    start, const: BOOLEAN;
  END;

PROCEDURE AllocCode(a: T; into: TextIntTbl.T; VAR lastCode: INTEGER) =
  BEGIN
    IF a.name # NIL THEN
      IF NOT into.get(a.name, a.code) THEN
        IF Text.Equal(a.name, "EOF") THEN
          a.code := 0;
        ELSE
          INC(lastCode);
          a.code := lastCode;
          EVAL into.put(a.name, a.code);
        END;
      END;
    END;
  END AllocCode;

PROCEDURE SetAttrs(a: T; start: TextBooleanTbl.T;
                   const: TextIntTbl.T) =
  VAR
    dummy: INTEGER;
  BEGIN
    IF a.name = NIL THEN
      a.const := TRUE;
    ELSE
      EVAL start.get(a.name, a.start);
      a.const := const.get(a.name, dummy);
    END;
  END SetAttrs;

PROCEDURE GetName(a: T): TEXT =
  BEGIN
    IF a.name = NIL THEN
      RETURN "@" & Text.FromChar(VAL(a.code, CHAR));
    ELSE
      RETURN a.name; 
    END;
  END GetName;

PROCEDURE Format(a: T): TEXT =
  BEGIN
    IF a.name # NIL THEN
      RETURN a.name (* & "=" & Fmt.Int(a.code) *);
    ELSIF a.code = 0 THEN
      RETURN "EOF";
    ELSE
      RETURN CharCodes.QC(VAL(a.code, CHAR)) (* & "=" & Fmt.Int(a.code) *);
    END;
  END Format;

PROCEDURE GetCode(a: T): INTEGER =
  BEGIN RETURN a.code; END GetCode;

PROCEDURE GetPrec(a: T; prec: TextPrecTbl.T): Prec.T =
  VAR
    key := a.name;
    val: Prec.T := NIL;
  BEGIN
    IF key = NIL THEN
      key := "@" & Text.FromChar(VAL(a.code, CHAR));
    END;
    IF prec.get(key, val) THEN
(*      Term.WrLn("Found prec: " & key & Fmt.Int(val.val));
    ELSE
      Term.WrLn("Not finding prec: " & key); *)
    END;
    RETURN val;
  END GetPrec;

PROCEDURE IsStart(a: T): BOOLEAN =
  BEGIN RETURN a.start; END IsStart;
PROCEDURE IsConst(a: T): BOOLEAN =
  BEGIN RETURN a.const; END IsConst;

PROCEDURE FromText(t: TEXT): T =
  BEGIN
    RETURN NEW(T, name := t, code := -1, start := FALSE);
  END FromText;

PROCEDURE FromChar(c: CHAR): T =
  BEGIN
    RETURN NEW(T, name := NIL, code := ORD(c), start := FALSE);
  END FromChar;

PROCEDURE Equal(a,b: T): BOOLEAN =
  BEGIN RETURN a.code = b.code; END Equal;

BEGIN
END Sym.
