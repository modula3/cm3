(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: InitGen.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE InitGen;
IMPORT CharRange;
IMPORT BracedCode;
IMPORT Text;
IMPORT TextSubs;
IMPORT TextWr, Wr;
IMPORT TextRd, Rd;
IMPORT Thread;

<* FATAL Rd.Failure, Wr.Failure, Thread.Alerted *>

PROCEDURE Get(form, src: TEXT): TEXT =
  VAR
    c: CHAR;
    wr := TextWr.New();
    rd := TextRd.New(src);
    nameStart, valStart: INTEGER;
    name: TEXT;
  PROCEDURE PutVal() =
    VAR
      subs: TextSubs.T;
      val: TEXT;
    BEGIN
      IF valStart # -1 THEN
        subs := NEW(TextSubs.T).init();
        val := Text.Sub(src, valStart, Rd.Index(rd)-valStart);
        subs.add("%name", name);
        subs.add("%val", val);
        Wr.PutText(wr, subs.apply(form));
      END;
    END PutVal;
  BEGIN
    TRY
      LOOP
        valStart := -1;
        BracedCode.FindChar(rd, CharRange.Letter);
        nameStart := Rd.Index(rd)-1;
        BracedCode.FindChar(rd, CharRange.T{':'});
        name := Text.Sub(src, nameStart, Rd.Index(rd)-nameStart-1);
        IF Rd.GetChar(rd)='=' THEN
          valStart := Rd.Index(rd);
          BracedCode.FindChar(rd, CharRange.T{';'});
        ELSE
          BracedCode.FindChar(rd, CharRange.T{';',':'});
          Rd.UnGetChar(rd);
          IF Rd.GetChar(rd) = ':' THEN
            c := Rd.GetChar(rd);
            <* ASSERT c = '=' *>
            valStart := Rd.Index(rd);
            BracedCode.FindChar(rd, CharRange.T{';'});
          END;
        END;
        Rd.UnGetChar(rd);
        PutVal();
      END;
    EXCEPT
      Rd.EndOfFile => PutVal();
    END;
    RETURN TextWr.ToText(wr);
  END Get;

BEGIN
END InitGen.
