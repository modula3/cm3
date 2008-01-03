(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu May 16 15:44:46 PDT 1996 by mhb                      *)

MODULE Lexer;

IMPORT Latin1Key, Lex, Rd, Text, TextRd, TextWr, Wr;

<*FATAL ANY*>

TYPE
  Entity = RECORD t: TEXT; c: CHAR; END;

CONST
  AllChars = SET OF CHAR {FIRST(CHAR) .. LAST(CHAR)};
  StartElement = SET OF CHAR {'<'};
  EndElement = SET OF CHAR {'>'};
  Ampersand = SET OF CHAR {'&'};
  Semicolon = SET OF CHAR {';'};
  Equals = SET OF CHAR {'='};
  Minus = SET OF CHAR {'-'};
  DQuote = SET OF CHAR {'"'};
  NonBlanks = AllChars - Lex.Blanks;

  (* THIS TABLE MUST BE KEPT SORTED *)
  EntityTable = ARRAY OF Entity{
      Entity{"AElig", VAL(Latin1Key.AE,CHAR)},
      Entity{"Aacute", VAL(Latin1Key.Aacute,CHAR)},
      Entity{"Acirc", VAL(Latin1Key.Acircumflex,CHAR)},
      Entity{"Agrave", VAL(Latin1Key.Agrave,CHAR)},
      Entity{"Aring", VAL(Latin1Key.Aring,CHAR)},
      Entity{"Atilde", VAL(Latin1Key.Atilde,CHAR)},
      Entity{"Auml", VAL(Latin1Key.Adiaeresis,CHAR)},
      Entity{"Ccedil", VAL(Latin1Key.Ccedilla,CHAR)},
      Entity{"ETH", VAL(Latin1Key.ETH,CHAR)},
      Entity{"Eacute", VAL(Latin1Key.Eacute,CHAR)},
      Entity{"Ecirc", VAL(Latin1Key.Ecircumflex,CHAR)},
      Entity{"Egrave", VAL(Latin1Key.Egrave,CHAR)},
      Entity{"Euml", VAL(Latin1Key.Ediaeresis,CHAR)},
      Entity{"Iacute", VAL(Latin1Key.Iacute,CHAR)},
      Entity{"Icirc", VAL(Latin1Key.Icircumflex,CHAR)},
      Entity{"Igrave", VAL(Latin1Key.Igrave,CHAR)},
      Entity{"Iuml", VAL(Latin1Key.Idiaeresis,CHAR)},
      Entity{"Ntilde", VAL(Latin1Key.Ntilde,CHAR)},
      Entity{"Oacute", VAL(Latin1Key.Oacute,CHAR)},
      Entity{"Ocirc", VAL(Latin1Key.Ocircumflex,CHAR)},
      Entity{"Ograve", VAL(Latin1Key.Ograve,CHAR)},
      Entity{"Oslash", VAL(Latin1Key.Ooblique,CHAR)},
      Entity{"Otilde", VAL(Latin1Key.Otilde,CHAR)},
      Entity{"Ouml", VAL(Latin1Key.Odiaeresis,CHAR)},
      Entity{"THORN", VAL(Latin1Key.THORN,CHAR)},
      Entity{"Uacute", VAL(Latin1Key.Uacute,CHAR)},
      Entity{"Ucirc", VAL(Latin1Key.Ucircumflex,CHAR)},
      Entity{"Ugrave", VAL(Latin1Key.Ugrave,CHAR)},
      Entity{"Uuml", VAL(Latin1Key.Udiaeresis,CHAR)},
      Entity{"Yacute", VAL(Latin1Key.Yacute,CHAR)},
      Entity{"aacute", VAL(Latin1Key.aacute,CHAR)},
      Entity{"acirc", VAL(Latin1Key.acircumflex,CHAR)},
      Entity{"aelig", VAL(Latin1Key.ae,CHAR)},
      Entity{"agrave", VAL(Latin1Key.agrave,CHAR)},
      Entity{"amp", '&'},
      Entity{"atilde", VAL(Latin1Key.atilde,CHAR)},
      Entity{"atilde", VAL(Latin1Key.atilde,CHAR)},
      Entity{"auml", VAL(Latin1Key.adiaeresis,CHAR)},
      Entity{"ccedil", VAL(Latin1Key.ccedilla,CHAR)},
      Entity{"copy", VAL(Latin1Key.copyright,CHAR)},
      Entity{"eacute", VAL(Latin1Key.eacute,CHAR)},
      Entity{"ecirc", VAL(Latin1Key.ecircumflex,CHAR)},
      Entity{"egrave", VAL(Latin1Key.egrave,CHAR)},
      Entity{"eth", VAL(Latin1Key.eth,CHAR)},
      Entity{"euml", VAL(Latin1Key.ediaeresis,CHAR)},
      Entity{"gt", '>'},
      Entity{"iacute", VAL(Latin1Key.iacute,CHAR)},
      Entity{"icirc", VAL(Latin1Key.icircumflex,CHAR)},
      Entity{"igrave", VAL(Latin1Key.igrave,CHAR)},
      Entity{"iuml", VAL(Latin1Key.idiaeresis,CHAR)},
      Entity{"lt", '<'},
      Entity{"ntilde", VAL(Latin1Key.ntilde,CHAR)},
      Entity{"oacute", VAL(Latin1Key.oacute,CHAR)},
      Entity{"ocirc", VAL(Latin1Key.ocircumflex,CHAR)},
      Entity{"ograve", VAL(Latin1Key.ograve,CHAR)},
      Entity{"oslash", VAL(Latin1Key.oslash,CHAR)},
      Entity{"otilde", VAL(Latin1Key.otilde,CHAR)},
      Entity{"ouml", VAL(Latin1Key.odiaeresis,CHAR)},
      Entity{"quot", '"'},
      Entity{"szlig", VAL(Latin1Key.ssharp,CHAR)},
      Entity{"thorn", VAL(Latin1Key.thorn,CHAR)},
      Entity{"uacute", VAL(Latin1Key.uacute,CHAR)},
      Entity{"ucirc", VAL(Latin1Key.ucircumflex,CHAR)},
      Entity{"ugrave", VAL(Latin1Key.ugrave,CHAR)},
      Entity{"uuml", VAL(Latin1Key.udiaeresis,CHAR)},
      Entity{"yacute", VAL(Latin1Key.yacute,CHAR)},
      Entity{"yuml", VAL(Latin1Key.ydiaeresis,CHAR)}
    };


PROCEDURE Get (s: Rd.T; obeyBlanks: BOOLEAN := FALSE): Token =
  (* Returns the next non-blank token from s. *)
  BEGIN
    IF obeyBlanks THEN
      WITH t = Lex.Scan(s, AllChars - StartElement) DO
        IF NOT Text.Empty(t) THEN
          RETURN NEW(WordToken, word := Unquote(t))
        ELSIF Rd.EOF(s) THEN 
          RETURN NIL
        ELSE
          EVAL Rd.GetChar(s);    (* must be the '<' *)
          RETURN GetElement(s);
        END
      END
    ELSE
      Lex.Skip(s);
      IF Rd.EOF(s) THEN RETURN NIL END;
      IF Rd.GetChar(s) = '<' THEN
        RETURN GetElement(s)
      ELSE
        Rd.UnGetChar(s);
        RETURN GetWord(s)
      END
    END
  END Get;

PROCEDURE GetWord (s: Rd.T): Token =
  (* Returns text, until hitting end-of-file or a StartElement.
     Put a single blank between words. *)
  VAR
    firstTime       := TRUE;
    done            := FALSE;
    t        : TEXT;
    wr              := TextWr.New();
  BEGIN
    WHILE NOT done DO
      Lex.Skip(s);
      t := Lex.Scan(s, NonBlanks - StartElement);
      IF NOT Text.Empty(t) THEN
        IF firstTime THEN
          firstTime := FALSE
        ELSE
          Wr.PutChar(wr, ' ')
        END;
        Wr.PutText(wr, t);
      END;
      done := Rd.EOF(s);
      IF NOT done THEN
        done := (Rd.GetChar(s) = '<');
        Rd.UnGetChar(s);
      END
    END;
    RETURN NEW(WordToken, word := Unquote(TextWr.ToText(wr)))
  END GetWord;

PROCEDURE GetElement (s: Rd.T): Token =
  VAR
    tok                  := NEW(ElementToken);
    tail, att: Attribute := NIL;
    c        : CHAR;
  BEGIN
    IF Rd.EOF(s) THEN RETURN NIL END;
    c := Rd.GetChar(s);
    IF c = '/' THEN
      tok.end := TRUE;
    ELSIF c = '!' THEN
      RETURN GetComment(s);
    ELSE
      Rd.UnGetChar(s);
    END;
    tok.tag := Lex.Scan(s, NonBlanks - EndElement);

    tok.attributes := NIL;
    Lex.Skip(s);
    WHILE NOT Rd.EOF(s) AND Rd.GetChar(s) # '>' DO
      Rd.UnGetChar(s);
      att := GetAttribute(s);
      IF tail = NIL THEN
        tok.attributes := att;
        tail := att;
      ELSE
        tail.next := att;
        tail := att;
      END;
      Lex.Skip(s);
    END;

    IF Rd.EOF(s) THEN RETURN NIL; END;
    RETURN tok;
  END GetElement;

PROCEDURE GetAttribute (s: Rd.T): Attribute =
  (* Read attributes.  Either: KEY, KEY=foo, or KEY="foo", with arbitrary
     white space around the equals sign. *)
  VAR
    c  : CHAR;
    att       := NEW(Attribute);
  BEGIN
    att.name := Lex.Scan(s, NonBlanks - Equals - EndElement);
    c := Rd.GetChar(s);
    IF c = '>' THEN
      Rd.UnGetChar(s);
      RETURN att
    ELSIF c = '=' THEN
      Rd.UnGetChar(s)
    ELSE (* c # '=' *)
      Lex.Skip(s);
      IF Rd.EOF(s) THEN RETURN att END;
    END;
    c := Rd.GetChar(s);
    IF c = '=' THEN
      Lex.Skip(s);
      IF Rd.EOF(s) THEN RETURN att END;
    ELSE
      Rd.UnGetChar(s);
      RETURN att
    END;
    c := Rd.GetChar(s);
    IF Rd.EOF(s) THEN RETURN att END;
    IF c = '"' THEN
      att.value := Unquote(Lex.Scan(s, AllChars - DQuote - EndElement));
      Lex.Skip(s, DQuote);
    ELSE
      Rd.UnGetChar(s);
      att.value := Unquote(Lex.Scan(s, NonBlanks - EndElement));
    END;
    RETURN att;
  END GetAttribute;

PROCEDURE GetComment (s: Rd.T): Token =
  VAR done := FALSE;
  BEGIN
    TRY
      Lex.Match(s, "--");
      WHILE NOT done DO
        Lex.Skip(s, AllChars - Minus);
        TRY
          Lex.Match(s, "-->");
          done := TRUE;
        EXCEPT
          Lex.Error => IF Rd.EOF(s) THEN done := TRUE; END;
        END;
      END
    EXCEPT
      Lex.Error =>
        Lex.Skip(s, AllChars - EndElement);
        IF NOT Rd.EOF(s) THEN EVAL Rd.GetChar(s) END;
    END;
    RETURN NEW(CommentToken);
  END GetComment;

PROCEDURE Unquote (t: TEXT): TEXT =
  BEGIN
    IF Text.FindChar(t, '&') = -1 THEN RETURN t END;
    VAR
      rd := TextRd.New(t);
      wr := TextWr.New();
      wr2 := TextWr.New();
      car: CHAR;
      entityName: TEXT;
    BEGIN
      WHILE Scan(rd, wr, AllChars - Ampersand) DO
        EVAL Rd.GetChar(rd);
        IF NOT Scan(rd, wr2, AllChars - Semicolon) THEN
          Wr.PutText(wr,"&" & TextWr.ToText(wr2));
        ELSE
          EVAL Rd.GetChar(rd);
          entityName := TextWr.ToText(wr2);
          IF FindEntity(entityName,car) THEN
            Wr.PutChar(wr,car);
          ELSE
            Wr.PutText(wr,"&" & entityName & ";");
          END;
        END;
      END;
      RETURN TextWr.ToText(wr)
    END
  END Unquote;

PROCEDURE FindEntity(name: TEXT; VAR car: CHAR): BOOLEAN =
  VAR
    start := 0;
    end := LAST(EntityTable);
    middle: CARDINAL;
    result: INTEGER;
  BEGIN
    WHILE start <= end DO
      middle := (start + end) DIV 2;
      result := Text.Compare(name,EntityTable[middle].t);
      IF result = 0 THEN
        car := EntityTable[middle].c;
        RETURN TRUE;
      ELSIF result < 0 THEN
        end := middle - 1;
      ELSE
        start := middle + 1;
      END;
    END;
    RETURN FALSE;
  END FindEntity;

PROCEDURE Scan (rd: Rd.T; wr: Wr.T; READONLY cs: SET OF CHAR): BOOLEAN =
  (* Read the longest prefix of "rd" composed of characters in "cs", 
     and write that prefix into "wr". Returns FALSE at EOF. *)
  VAR c: CHAR;
  BEGIN
    WHILE NOT Rd.EOF(rd) DO
      c := Rd.GetChar(rd);
      IF NOT (c IN cs) THEN Rd.UnGetChar(rd); RETURN TRUE END;
      Wr.PutChar(wr, c)
    END;
    RETURN FALSE;
  END Scan;

BEGIN 
END Lexer.
