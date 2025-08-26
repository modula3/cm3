(*                                                                           *)
(*  TextUtils.m3                                                             *)
(*                                                                           *)
(*  Some useful text processing routines for the PL1 compiler.               *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(*                                                                           *)
(* $Id: TextUtils.m3,v 1.25 2008/12/18 00:38:12 mika Exp $ *)

MODULE CitTextUtils;
IMPORT IntList, ScanList;
IMPORT TextReader;
IMPORT Text;
IMPORT TextRd, TextWr;
IMPORT Rd, Wr;
IMPORT Fmt;
IMPORT TextSet, TextSetDef, TextList;
IMPORT Lex, FloatMode;
IMPORT Thread;
IMPORT ASCII;
IMPORT Wx, TextSeq;

<* FATAL Rd.Failure, Wr.Failure, Thread.Alerted *>

CONST TE = Text.Equal;
      TL = Text.Length;

PROCEDURE CountCharOccurences(in: TEXT; c: CHAR): CARDINAL =
  VAR
    count := 0;
  BEGIN
    FOR i := 0 TO TL(in) - 1 DO
      IF Text.GetChar(in,i) = c THEN
        INC(count);
      END;
    END;
    RETURN count;
  END CountCharOccurences;

PROCEDURE ReplaceChar(in : TEXT; old, new : CHAR) : TEXT =
  VAR
    res := NEW(REF ARRAY OF CHAR, TL(in));
  BEGIN
    FOR i := 0 TO TL(in) - 1 DO
      WITH char = Text.GetChar(in,i) DO
        IF char = old THEN res[i] := new ELSE res[i] := char END
      END
    END;
    RETURN Text.FromChars(res^)
  END ReplaceChar;

PROCEDURE Tr(in : TEXT; READONLY old, new : ARRAY OF CHAR) : TEXT =
  VAR
    s   := SET OF CHAR {};
    res := NEW(REF ARRAY OF CHAR, TL(in));
  BEGIN
    <*ASSERT NUMBER(old) = NUMBER(new)*>
    FOR j := FIRST(old) TO LAST(old) DO
      s := s + SET OF CHAR { old[j] }
    END;
    
    FOR i := 0 TO TL(in) - 1 DO
      WITH char = Text.GetChar(in,i) DO
        IF char IN s THEN
          FOR j := FIRST(old) TO LAST(old) DO
            IF char = old[j] THEN res[i] := new[j] END
          END
        ELSE
          res[i] := char
        END
      END
    END;
    RETURN Text.FromChars(res^)
  END Tr;

PROCEDURE Replace(in, old, new : TEXT) : TEXT =
  VAR
    s, p : CARDINAL := 0;
    wx := Wx.New();
    ol := TL(old);
  BEGIN
    <*ASSERT ol>0*>
    WHILE FindSub(in, old, p, s) DO
      Wx.PutText(wx, Text.Sub(in, s, p - s));
      Wx.PutText(wx, new);
      s := p + ol
    END;
    Wx.PutText(wx, Text.Sub(in, s));
    RETURN Wx.ToText(wx)
  END Replace;

(* find first occurrence of sub in in *)
(* not a good algorithm: if necessary, code up Knuth-Morris-Pratt instead. *)
PROCEDURE FindSub(in, sub : TEXT; VAR pos : CARDINAL; start := 0) : BOOLEAN =
  VAR
    inA := NEW(REF ARRAY OF CHAR, TL(in));
    subA := NEW(REF ARRAY OF CHAR, TL(sub));
  BEGIN
    Text.SetChars(inA^,in);
    Text.SetChars(subA^,sub);
    FOR i := start TO LAST(inA^) - LAST(subA^) DO
      VAR
        success := TRUE;
      BEGIN
        FOR j := 0 TO LAST(subA^) DO
          IF subA[j] # inA[i + j] THEN 
            success := FALSE; 
            EXIT 
          END
        END;
        IF success THEN pos := i; RETURN TRUE END
      END
    END;
    RETURN FALSE
  END FindSub;

PROCEDURE FindText(in, sub : TEXT; start := 0) : [-1..LAST(CARDINAL)] =
  VAR r : CARDINAL;
  BEGIN
    IF FindSub(in, sub, r, start) THEN
      RETURN r 
    ELSE
      RETURN -1
    END
  END FindText;

PROCEDURE FindAnyChar(in: TEXT; c: SET OF CHAR;
                      VAR pos: CARDINAL; start := 0): BOOLEAN =
  BEGIN
    WHILE start < TL(in) DO
      IF Text.GetChar(in, start) IN c THEN pos := start; RETURN TRUE END;
      INC(start);
    END;
    RETURN FALSE;
  END FindAnyChar;

PROCEDURE HaveSub(in, sub : TEXT) : BOOLEAN = 
  VAR x : CARDINAL; BEGIN RETURN FindSub(in, sub, x) END HaveSub;


PROCEDURE HavePrefix(in, prefix: TEXT): BOOLEAN =
  BEGIN
    RETURN TE(Text.Sub(in, 0, TL(prefix)), prefix);
  END HavePrefix;

PROCEDURE HaveSuffix(in, suffix: TEXT): BOOLEAN =
  VAR
    pos := TL(in) - TL(suffix);
  BEGIN
    RETURN pos >= 0 AND TE(Text.Sub(in, pos), suffix);
  END HaveSuffix;

PROCEDURE RemovePrefix(in, prefix: TEXT): TEXT =
  BEGIN
    <* ASSERT TE(Text.Sub(in, 0, TL(prefix)),prefix) *>
    RETURN Text.Sub(in, TL(prefix));
  END RemovePrefix;

PROCEDURE RemoveSuffix(in, suffix: TEXT): TEXT =
  VAR
    pos := TL(in) - TL(suffix);
  BEGIN
    <* ASSERT pos >= 0 AND TE(Text.Sub(in, pos), suffix) *>
    RETURN Text.Sub(in, 0, pos);
  END RemoveSuffix;

PROCEDURE CheckSuffix(in, suffix : TEXT) : TEXT =
  BEGIN
    IF HaveSuffix(in, suffix) THEN
      RETURN RemoveSuffix(in, suffix)
    ELSE
      RETURN NIL
    END
  END CheckSuffix;

PROCEDURE CheckPrefix(in, prefix : TEXT) : TEXT =
  BEGIN
    IF HavePrefix(in, prefix) THEN
      RETURN RemovePrefix(in, prefix)
    ELSE
      RETURN NIL
    END
  END CheckPrefix;

PROCEDURE RemoveSuffixes(fn : TEXT; READONLY exts : ARRAY OF TEXT) : TEXT =
  (* remove extension, if any from list *)
  BEGIN
    FOR i := FIRST(exts) TO LAST(exts) DO
      WITH e = exts[i],
           lf = TL(fn),
           le = TL(e) DO
        IF lf > le AND TE(e, Text.Sub(fn, lf-le, le)) THEN
          RETURN Text.Sub(fn,0,lf-le)
        END
      END
    END;
    RETURN fn
  END RemoveSuffixes;

PROCEDURE ReplacePrefix(in, oprefix, nprefix: TEXT): TEXT =
  BEGIN
    RETURN nprefix & RemovePrefix(in, oprefix)
  END ReplacePrefix;

PROCEDURE ReplaceSuffix(in, osuffix, nsuffix: TEXT): TEXT =
  BEGIN
    RETURN RemoveSuffix(in, osuffix) & nsuffix
  END ReplaceSuffix;

PROCEDURE Pluralize(noun : TEXT; n : INTEGER; 
                    ending : TEXT; printNum : BOOLEAN) : TEXT =
  VAR
    res : TEXT;
  BEGIN 
    IF printNum THEN res := Fmt.Int(n) & " " ELSE res := "" END;
    IF n = 1 THEN RETURN res & noun ELSE RETURN res & noun & ending END 
  END Pluralize;

PROCEDURE ListToSet(l : TextList.T) : TextSet.T =
  VAR
    res := NEW(TextSetDef.T).init();
  BEGIN
    WHILE l # NIL DO EVAL res.insert(l.head); l := l.tail END;
    RETURN res
  END ListToSet;
  
PROCEDURE SetToList(set : TextSet.T) : TextList.T =
  VAR
    iter := set.iterate();
    t : TEXT;
    res : TextList.T := NIL;
  BEGIN
    WHILE iter.next(t) DO res := TextList.Cons(t,res) END;
    RETURN res
  END SetToList;

PROCEDURE Shatter(t: TEXT; delims:="\t "; endDelims:="\n;#%";
                  skipNulls:=TRUE): TextList.T =
  BEGIN
    RETURN NEW(TextReader.T).init(t).shatter(delims, endDelims, skipNulls);
  END Shatter;


PROCEDURE ShatterInts(t: TEXT; defaultBase := 10;
                      delims := ":.\t, "; endDelims := "\n;#%"): IntList.T = 
  <* FATAL Lex.Error, FloatMode.Trap *>
  BEGIN
    RETURN ScanList.Int(Shatter(t, delims, endDelims), defaultBase);
  END ShatterInts;

PROCEDURE Filter(in: TEXT; keep: SET OF CHAR): TEXT =
  VAR
    result := NEW(REF ARRAY OF CHAR, TL(in));
    len := 0;
    last := TL(in) - 1;
    c: CHAR;
  BEGIN
    FOR i := 0 TO last DO
      c := Text.GetChar(in, i);
      IF c IN keep THEN
        result[len] := c;
        INC(len);
      END;
    END;
    RETURN Text.FromChars(SUBARRAY(result^, 0, len));
  END Filter;

PROCEDURE FilterIdent(in: TEXT): TEXT =
  TYPE
    SC      = SET OF CHAR;
  CONST
    Lower   = SC { 'a' .. 'z' };
    Upper   = SC { 'A' .. 'Z' };
    Alpha   = Lower + Upper;
    Digit   = SC { '0' .. '9' };
    Ident1  = Alpha + SC { '_' };
    Ident2  = Ident1 + Digit;
  VAR
    result := NEW(REF ARRAY OF CHAR, TL(in));
    len := 0;
    last := TL(in) - 1;
    c: CHAR;
    first := TRUE;
  BEGIN
    FOR i := 0 TO last DO
      c := Text.GetChar(in, i);
      IF c IN Ident1 OR ((NOT first) AND c IN Ident2) THEN
        result[len] := c;
        INC(len);
        first := FALSE;
      END;
    END;
    RETURN Text.FromChars(SUBARRAY(result^, 0, len));
  END FilterIdent;

PROCEDURE FilterOut(in: TEXT; remove := SET OF CHAR{' ', '\t', '\n'}): TEXT =
  BEGIN
    RETURN Filter(in, SET OF CHAR{FIRST(CHAR) .. LAST(CHAR)} - remove);
  END FilterOut;

PROCEDURE FilterEdges(in: TEXT; remove := SET OF CHAR{' ', '\t', '\n'}): TEXT =
  VAR
    i := 0;
  BEGIN
    LOOP
      IF i = TL(in) THEN RETURN ""; END;
      IF NOT Text.GetChar(in, i) IN remove THEN EXIT END;
      INC(i);
    END;
    FOR j := TL(in)-1 TO 0 BY -1 DO
      IF NOT Text.GetChar(in, j) IN remove THEN
        RETURN Text.Sub(in, i, j-i+1);
      END;
    END;
    <* ASSERT FALSE *>
  END FilterEdges;

PROCEDURE Capitalize(t: TEXT; uniqueSuffix := ""): TEXT =
  VAR
    l := Text.GetChar(t, 0);
  BEGIN
    IF l >= 'a' AND l <= 'z' THEN
      WITH u = VAL(ORD(l) - ORD('a') + ORD('A'), CHAR) DO
        RETURN Text.FromChar(u) & Text.Sub(t, 1);
      END;
    ELSE
      RETURN t & uniqueSuffix;
    END;
  END Capitalize;

PROCEDURE BreakLongLines(t: TEXT; atCol := 79): TEXT =
  VAR
    rd := TextRd.New(t);
    wr := TextWr.New();
    line: TEXT;
  BEGIN
    IF atCol = 0 THEN RETURN t; END;
    TRY
      LOOP
        line := Rd.GetLine(rd);
        WHILE TL(line) > atCol DO
          Wr.PutText(wr, Text.Sub(line, 0, atCol) & "\n");
          line := Text.Sub(line, atCol);
        END;
        Wr.PutText(wr, line & "\n");
      END;
    EXCEPT Rd.EndOfFile =>
    END;
    RETURN TextWr.ToText(wr);
  END BreakLongLines;

PROCEDURE GetLines(t: TEXT; n: INTEGER;  firstBreakLongAtCol:=79): TEXT =
  VAR
    pos: INTEGER;
  BEGIN
    t := BreakLongLines(t, firstBreakLongAtCol);
    IF n > 0 THEN
      pos := 0;
      FOR i := 1 TO n DO
        pos := Text.FindChar(t, '\n', pos) + 1;
        IF pos = 0 THEN
          pos := TL(t);
          EXIT;
        END;
      END;
      RETURN Text.Sub(t,0,pos);
    ELSE
      pos := LAST(INTEGER);
      FOR i := 0 TO -n DO
        pos := Text.FindCharR(t, '\n', pos-1);
        IF pos = -1 THEN EXIT END;
      END;
      INC(pos);
      RETURN Text.Sub(t,pos);
    END;
  END GetLines;

PROCEDURE Assemble(t: TextList.T; postDelim:=" "; skipLastDelim:=TRUE): TEXT =
  BEGIN
    IF t = NIL THEN
      RETURN "";
    ELSIF (t.tail = NIL) AND skipLastDelim THEN
      RETURN t.head;
    ELSE
      RETURN t.head & postDelim & Assemble(t.tail, postDelim, skipLastDelim);
    END;
  END Assemble;

(* split string at first "at" *)
PROCEDURE SplitText(text : TEXT; at : CHAR; VAR beg, end : TEXT) =
  VAR
    index : INTEGER;
  BEGIN
    IF text = NIL THEN beg := NIL; end := NIL; RETURN END;
    index := Text.FindChar(text,at);
    IF index < 0 THEN beg := text; end := NIL; RETURN END;
    beg := Text.Sub(text, 0, index);

    (* this must be last so that text and end may point to the same *)
    end := Text.Sub(text, index + 1)
  END SplitText;

PROCEDURE CountChars(text: TEXT; what : CHAR) : CARDINAL =
  VAR
    res := 0;
    i := 0;
  BEGIN
    WHILE i < TL(text) DO
      i := Text.FindChar(text, what, i) + 1;

      IF i <= 0 THEN EXIT END;
      res := res + 1
    END;
    RETURN res
  END CountChars;

PROCEDURE ToChars(text: TEXT): REF ARRAY OF CHAR =
VAR
  result := NEW(REF ARRAY OF CHAR, TL(text));
BEGIN
    Text.SetChars(result^, text);
    RETURN result;
END ToChars;

PROCEDURE ToUpper(x : TEXT) : TEXT =
  VAR
    l := TL(x);
    a := NEW(REF ARRAY OF CHAR, l);
  BEGIN
    FOR i := 0 TO l-1 DO
      a[i] := ASCII.Upper[Text.GetChar(x,i)]
    END;
    RETURN Text.FromChars(a^)
  END ToUpper;

PROCEDURE ToLower(x : TEXT) : TEXT =
  VAR
    l := TL(x);
    a := NEW(REF ARRAY OF CHAR, l);
  BEGIN
    FOR i := 0 TO l-1 DO
      a[i] := ASCII.Lower[Text.GetChar(x,i)]
    END;
    RETURN Text.FromChars(a^)
  END ToLower;

PROCEDURE EqualIgnoringCase(t1, t2 : TEXT) : BOOLEAN =
  BEGIN
    RETURN TE(ToUpper(t1),ToUpper(t2)) 
  END EqualIgnoringCase;

PROCEDURE FormatInfix(seq : TextSeq.T; operator : TEXT) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := 0 TO seq.size()-1 DO
      Wx.PutText(wx, seq.get(i));
      IF i # seq.size()-1 THEN Wx.PutText(wx,operator) END
    END;
    RETURN Wx.ToText(wx)
  END FormatInfix;

PROCEDURE FormatInfixArr(READONLY seq : ARRAY OF TEXT; 
                         operator : TEXT) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := FIRST(seq) TO LAST(seq) DO
      Wx.PutText(wx, seq[i]);
      IF i # LAST(seq) THEN Wx.PutText(wx,operator) END
    END;
    RETURN Wx.ToText(wx)
  END FormatInfixArr;

PROCEDURE MakeM3Literal(of : TEXT) : TEXT =

  CONST DQ = '"';
  CONST BS = '\\';
  
  PROCEDURE Put(c : CHAR) =
    BEGIN
      arr[p] := c;
      INC(p)
    END Put;
    
  VAR
    n   := Text.Length(of);
    arr := NEW(REF ARRAY OF CHAR, n * 2 + 2);
    p   := 0;
  BEGIN
    Put(DQ);
    FOR i := 0 TO n - 1 DO
      WITH c = Text.GetChar(of, i) DO
        CASE c OF
          DQ => Put(BS); Put(DQ)
        |
          BS => Put(BS); Put(BS)
        ELSE
          Put(c)
        END
      END
    END;
    Put(DQ);
    RETURN Text.FromChars(SUBARRAY(arr^, 0, p))
  END MakeM3Literal;
  
BEGIN END CitTextUtils.
