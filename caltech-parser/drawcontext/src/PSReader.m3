(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PSReader.m3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

MODULE PSReader;
IMPORT WYSIWYGify;
IMPORT PSReaderGuts;
IMPORT LinoText;
IMPORT Scan;
IMPORT Text;
IMPORT TextUtils;
IMPORT TextReader;
IMPORT TextTextTbl;
IMPORT CacheDrawContext;
IMPORT DrawContext;
IMPORT DrawContextClass;
IMPORT PSTextBounder;
IMPORT Rd;
IMPORT Wr;
IMPORT TextWr;
IMPORT Thread;
IMPORT FloatMode;
IMPORT Lex;
FROM Debug IMPORT S;

CONST
  DebugLevel = 20;

<* FATAL FloatMode.Trap, Lex.Error *>
<* FATAL Rd.Failure, Wr.Failure, Thread.Alerted *>

PROCEDURE UnComment(rd: Rd.T): TEXT =
  VAR
    wr := TextWr.New();
    line: TEXT;
  BEGIN
    TRY
      LOOP
        line := Rd.GetLine(rd);
        (* S("got line: " & line, DebugLevel); *)
        IF Text.Length(line) # 0 AND Text.GetChar(line, 0) # '%' THEN
          Wr.PutText(wr, line);
          Wr.PutChar(wr, '\n');
        END;
      END;
    EXCEPT
      Rd.EndOfFile =>
    END;
    (* S("uncommented: " & TextWr.ToText(wr), DebugLevel); *)
    RETURN TextWr.ToText(wr);
  END UnComment;

PROCEDURE StripDelims(VAR acc: TEXT; d1, d2: CHAR) =
  BEGIN
    S("strip " & Text.FromChar(d1) & Text.FromChar(d2) &
      " from \"" & acc & "\"", DebugLevel);
    IF Text.GetChar(acc, 0) = d1 AND
      Text.GetChar(acc, Text.Length(acc)-1) = d2 THEN
      acc := Text.Sub(acc, 1, Text.Length(acc)-2);
    END;
  END StripDelims;

PROCEDURE Read(rd: Rd.T; to: DrawContext.T) =
  VAR
    tr := NEW(TextReader.T).init(UnComment(rd));
    word, acc, rest, theText, arg1, arg2: TEXT;
    macros := NEW(TextTextTbl.Default).init();
    guts := NEW(PSReaderGuts.T).init(to);
    sawSW, sawDiv := TRUE;
  PROCEDURE Next(): BOOLEAN =
    VAR
      newWord: TEXT;
    BEGIN
      IF tr.next("\t\n ", word, TRUE) THEN
        (* S("raw word: " & word, DebugLevel); *)
        newWord := word;
        WHILE macros.get(newWord, newWord) DO
          S("lookup: " & newWord, DebugLevel);
          (* allow multi-word recursive macros *)
          tr.pushBack(newWord & " ");
          WITH true = tr.next("\t\n ", newWord, TRUE) DO
            <* ASSERT true *>
          END;
          <* ASSERT NOT Text.Equal(word, newWord) *>
        END;
        word := newWord;
        RETURN TRUE;
      ELSE
        S("NO MORE.", DebugLevel);
        RETURN FALSE;
      END;
    END Next;
  PROCEDURE Is(what: TEXT): BOOLEAN =
    BEGIN
      RETURN Text.Equal(word, what);
    END Is;
  BEGIN
    (*    IF tr.next("", word) THEN
          S("stole: " & word, DebugLevel);
          ELSE
          S("where'd it go?", DebugLevel);
          END; *)

    WHILE Next() DO
      CASE Text.GetChar(word, 0) OF
      | '/' =>
        rest := Text.Sub(word, 1);
        acc := "";
        WHILE Next() AND NOT
          (Is("def") OR Is("findfont")) DO
          IF Text.Length(acc) = 0 OR
            Text.GetChar(acc, Text.Length(acc)-1) # '}' THEN
            IF Text.Length(acc) # 0 THEN
              acc := acc & " ";
            END;
            acc := acc & word;
          END;
        END;
      | '(' =>
        theText := word;
        IF Text.GetChar(word, Text.Length(word)-1) # ')' THEN
          WHILE Next() AND Text.GetChar(word, Text.Length(word)-1) # ')' DO
            IF Text.Length(theText) # 0 THEN
              theText := theText & " ";
            END;
            theText := theText & word;
          END;
          IF Text.Length(theText) # 0 THEN
            theText := theText & " ";
          END;
          theText := theText & word;
        END;
        StripDelims(theText, '(', ')');
        sawDiv := FALSE;
        sawSW := FALSE;
      ELSE
      END;
      S("got word: " & word, DebugLevel);
      IF Is("def") THEN
        StripDelims(acc, '{', '}');
        S("defining " & rest & " = \"" & acc & "\"", DebugLevel);
        EVAL macros.put(rest, acc);
      ELSIF Is("gsave") THEN
        guts.save();
      ELSIF Is("grestore")  THEN
        guts.restore();
      ELSIF Is("scale") THEN
        guts.scale(Scan.Real(arg2), Scan.Real(arg1));
      ELSIF Is("translate") THEN
        guts.translate(Scan.Real(arg2), Scan.Real(arg1));
      ELSIF Is("setlinewidth") THEN
        guts.setLineWidth(Scan.Real(arg1));
      ELSIF Is("scalefont") THEN
        guts.scaleFont(WYSIWYGify.ScanTextSizeFromPS(arg1));
      ELSIF Is("newpath") THEN
        guts.newPath();
      ELSIF Is("moveto") THEN
        guts.moveTo(Scan.Real(arg2), Scan.Real(arg1));
      ELSIF Is("lineto") THEN
        guts.lineTo(Scan.Real(arg2), Scan.Real(arg1));
      ELSIF Is("closepath") THEN
        guts.closePath();
      ELSIF Is("stroke") THEN
        guts.stroke();
      ELSIF Is("show") THEN
        VAR
          attach: LinoText.Attach;
        BEGIN
          (* the following is obviously a hack *)
          IF sawSW THEN
            IF sawDiv THEN
              attach := LinoText.Attach.CenterBase;
            ELSE
              attach := LinoText.Attach.East;
            END;
          ELSE
            attach := LinoText.Attach.West;
          END;
          theText := TextUtils.Replace(theText, "\\", "");
          guts.show(theText, attach);
        END;
        sawSW := FALSE;
        sawDiv := FALSE;
      ELSIF Is("div") THEN
        sawDiv := TRUE;
      ELSIF Is("stringwidth") THEN
        sawSW := TRUE;
      END;
      arg2 := arg1;
      arg1 := word;
    END;
  END Read;

PROCEDURE New(rd: Rd.T := NIL; captureResDPI: INTEGER): T =
  VAR
    self := NEW(CacheDrawContext.T);
  BEGIN
    self.resDPI := captureResDPI;
    self.textBounder := NEW(PSTextBounder.T).init();
    IF rd # NIL THEN
      Read(rd, self);
    END;
    RETURN self;
  END New;

BEGIN
END PSReader.
