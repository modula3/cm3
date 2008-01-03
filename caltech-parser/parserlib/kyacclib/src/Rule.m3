(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Rule.m3,v 1.2 2001-09-19 15:31:35 wagner Exp $ *)

MODULE Rule;
IMPORT Prec;
IMPORT CharRange;
IMPORT CharCodes;
IMPORT Sym;
IMPORT SymList;
IMPORT SymListParse;
IMPORT TextSubs;
IMPORT TextPrecTbl;
IMPORT TextBooleanTbl;
IMPORT TextIntTbl;
IMPORT Rd, Thread;
IMPORT Wr, TextWr;
IMPORT Fmt;
(* IMPORT Term, Fmt; *)
<*FATAL Rd.Failure, Wr.Failure, Thread.Alerted*>
REVEAL
  T = Public BRANDED OBJECT
    subs: TextSubs.T := NIL;
  END;

PROCEDURE DbgFormat(a: T): TEXT =
  VAR
    acc := a.name & " :";
    cur := a.syms;
  BEGIN
    WHILE cur # NIL DO
      acc := acc & " " & Sym.Format(cur.head);
      cur := cur.tail;
    END;
    RETURN acc;
  END DbgFormat;

PROCEDURE CountParams(a: T): INTEGER =
  VAR
    cur := a.syms;
    i: INTEGER := 0;
  BEGIN
    WHILE cur # NIL DO
      IF NOT Sym.IsConst(cur.head) THEN INC(i); END;
      cur := cur.tail;
    END;
    RETURN i;
  END CountParams;

PROCEDURE FormatParams(a: T; form: TEXT): TEXT =
  VAR
    cur := a.syms;
    wr := TextWr.New();
    i: INTEGER := 0;
    stackRef := SymList.Length(cur);
    name: TEXT;
    subs: TextSubs.T;
  BEGIN
    WHILE cur # NIL DO
      DEC(stackRef);
      IF NOT Sym.IsConst(cur.head) THEN
        INC(i);
        name := Sym.GetName(cur.head);
        subs := NEW(TextSubs.T).init();
        subs.add("%number", Fmt.Int(i));
        subs.add("%type", name);
        name := "";
        IF stackRef # 0 THEN
          name := Fmt.Int(-stackRef);
        END;
        subs.add("%offset", name);
        Wr.PutText(wr, subs.apply(form));
      END;
      cur := cur.tail;
    END;
    RETURN TextWr.ToText(wr);
  END FormatParams;

PROCEDURE Format(a: T; form: TEXT; last: BOOLEAN := TRUE): TEXT =
  VAR
    optionalComma := ",";
    optionalCR := "\n            ";
  PROCEDURE Pform(key, form: TEXT) =
    BEGIN
      a.subs.add(key, FormatParams(a, form));
    END Pform;
  BEGIN
    IF a.subs = NIL THEN
      IF last THEN optionalComma := "";END;
      IF a.syms=NIL THEN optionalCR := "";END;
      a.subs := NEW(TextSubs.T).init();
      a.subs.add("%debug", CharCodes.Q(DbgFormat(a)));
      a.subs.add("%name", a.name);
      a.subs.add("%return", Sym.GetName(a.return));
      a.subs.add("%length", Fmt.Int(a.length));
      a.subs.add("%number", Fmt.Int(a.number));
      a.subs.add("%codeReturn", Fmt.Int(Sym.GetCode(a.return)));
      Pform("%oparams", "; p%number: Original_%type");
      Pform("%sparams", "; p%number: %type");
      Pform("%uparams", ";<*UNUSED*>p%number: %type");
      Pform("%cparams", ", p%number");
      Pform("%fromStack", "p%number:%type:=a[p%offset].value.value;");
      Pform("%narrow", "    n%number := NARROW(p%number, %type);\n");
      a.subs.add("\\,", optionalComma);
      a.subs.add("\\\n", optionalCR);
      a.subs.add("\\\\\n", "");
    END;
    RETURN a.subs.apply(form);
  END Format;


PROCEDURE FromRd(rd: Rd.T; return: Sym.T;
                 allowedChars: CharRange.T;
                 number: INTEGER): T =
  VAR
    self := NEW(T, return := return, prec := NIL, number := number);
    <* FATAL Rd.EndOfFile *>
  BEGIN
    WHILE Rd.GetChar(rd) IN CharRange.WhiteSpace DO END;
    self.name := SymListParse.BackGetName(rd) & "_" &
                     Sym.GetName(self.return);
    self.syms := SymListParse.Parse(rd, allowedChars);
    self.length := SymList.Length(self.syms);
    RETURN self;
  END FromRd;

PROCEDURE LookupSyms(self: T;
                     prec: TextPrecTbl.T;
                     start: TextBooleanTbl.T;
                     codes: TextIntTbl.T;
                     const: TextIntTbl.T;
                     VAR lastCode: INTEGER) =
  PROCEDURE Lookup(sym: Sym.T) =
    BEGIN
      Sym.AllocCode(sym, codes, lastCode);
      Sym.SetAttrs(sym, start, const);
    END Lookup;
  VAR
    cur := self.syms;
    highestPrec := NEW(Prec.T, kind := Prec.Kind.None);
    thisPrec: Prec.T;
  BEGIN
    Lookup(self.return);
    WHILE cur # NIL DO
      Lookup(cur.head);
      thisPrec := Sym.GetPrec(cur.head, prec);
      IF thisPrec # NIL THEN
        IF highestPrec.val < thisPrec.val THEN
          highestPrec := thisPrec;
        END;
      END;          
      cur := cur.tail;
    END;
    IF prec.get(self.name, highestPrec) THEN
(*      Term.WrLn("Found prec: " & self.name & Fmt.Int(highestPrec.val));
    ELSE
      Term.WrLn("Not finding prec: " & self.name); *)
    END;
    self.prec := highestPrec;
    self.prec.used := TRUE;
(*    Term.WrLn("Marking used: " & Fmt.Int(self.prec.val)); *)
  END LookupSyms;

PROCEDURE Equal(<*UNUSED*>a, b: T): BOOLEAN =
  BEGIN <*ASSERT FALSE*> END Equal;

PROCEDURE Compare(a, b: T; assoc: BOOLEAN := FALSE): [-1 .. 1] =
  BEGIN
    IF b = NIL THEN
      RETURN 1;
    ELSIF a = NIL THEN
      RETURN -1;
    ELSE
      VAR
        aprec := a.prec.val;
        bprec := b.prec.val;
      BEGIN
        IF aprec = 0 OR bprec = 0 THEN
          RETURN 0;
        ELSIF aprec > bprec THEN
          RETURN 1;
        ELSIF bprec > aprec THEN
          RETURN -1;
        ELSIF a.prec.kind = Prec.Kind.None OR NOT assoc THEN
          RETURN 0;
        ELSIF a.prec.kind = Prec.Kind.Left THEN
          RETURN 1;
        ELSE
          RETURN -1;
        END;
      END;
    END;
  END Compare;

PROCEDURE Number(a: T): INTEGER =
  BEGIN
    IF a = NIL THEN
      RETURN 0;
    ELSE
      RETURN a.number;
    END;
  END Number;

BEGIN
END Rule.
