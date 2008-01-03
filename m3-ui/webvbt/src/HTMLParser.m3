(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Aug 27 15:11:03 PDT 1996 by najork                   *)
(*      modified on Wed Apr 10 12:33:15 PDT 1996 by mhb                      *)

MODULE HTMLParser EXPORTS HTML;

IMPORT CIText, CITextElementTbl, Element, Lexer, HTML,
       Rd, Thread, TextWr, Wr;

TYPE
  Closure = REF RECORD
    html: HTML.T;
    rd: Rd.T;
    obeyblanks: BOOLEAN;
  END;

TYPE EndCondition = {EOF, EndTag, ListItem, GlossaryTerm, GlossaryDef};


PROCEDURE FromRd (rd: Rd.T): HTML.T RAISES {Thread.Alerted} =
  VAR s := NEW(Closure);
  BEGIN
    s.rd := rd;
    s.obeyblanks := FALSE;

    (* Parse the source. *)
    s.html := NEW(HTML.T);
    s.html.body := ParseSequence(s);
    RETURN s.html;
  END FromRd;


PROCEDURE ParseSequence(s: Closure;
                        endCondition := EndCondition.EOF;
                        endTag:= Element.T.None;
                        (*OUT*)itemEndOnly: REF BOOLEAN := NIL
                        ): HTML.Sequence RAISES {Thread.Alerted} =
VAR
  done := FALSE;
  head := NEW(HTML.Sequence, next := NIL);
  tail := head;
  tok: Lexer.Token;
BEGIN
  WHILE NOT done DO
    IF Thread.TestAlert() THEN RAISE Thread.Alerted END;
    tok := Lexer.Get (s.rd, s.obeyblanks);
    TYPECASE tok OF
    | NULL =>
      done := TRUE;
      IF itemEndOnly # NIL THEN itemEndOnly^ := FALSE; END;
    | Lexer.CommentToken =>
    | Lexer.WordToken (tok) =>
      VAR word := NEW(HTML.Word);
      BEGIN
        word.word := tok.word;
        tail.next := word;
        tail := word;
      END;
    | Lexer.ElementToken (tok) =>
      VAR tagType: Element.T;
      BEGIN
        IF CIText.Equal(tok.tag, "PRE") THEN
           s.obeyblanks := NOT tok.end
        END;
        IF NOT tags.get(tok.tag, tagType) THEN
          (* ignore unknown tags *)
        ELSIF SequenceEndCondition(tagType, tok, endCondition,
                                   endTag, itemEndOnly) THEN
          done := TRUE;
        ELSIF tok.end THEN
          IF tagType = Element.T.HTML OR
             tagType = Element.T.Head OR
             tagType = Element.T.Body THEN
             (* We ignore these tags. *)
          ELSE
            (* This is bad HTML -- we got a </foo>, but we 
               were not parsing a sequence starting with <foo>.
               silently ignore this and hope for the best... *)
          END;
        ELSE
          tail.next := ParseElement(s, tagType, tok);
          IF tail.next # NIL THEN tail := tail.next; END;
        END;
      END;
    ELSE
    END;
  END;
  tail.next := NIL;
  RETURN head.next;
END ParseSequence;

PROCEDURE SequenceEndCondition(tagType: Element.T;
                               tok: Lexer.ElementToken;
                               endCondition: EndCondition;
                               endTag: Element.T;
                               itemEndOnly: REF BOOLEAN): BOOLEAN =
BEGIN
  CASE endCondition OF

  | EndCondition.EOF => 
      RETURN FALSE;

  | EndCondition.EndTag => 
      RETURN tagType = endTag AND tok.end;

  | EndCondition.ListItem =>
      IF tagType = Element.T.LI OR (tagType = endTag AND tok.end) THEN
        IF tagType # endTag THEN itemEndOnly^ := TRUE;
        ELSE itemEndOnly^ := FALSE; END;
        RETURN TRUE;
      ELSE RETURN FALSE;
      END;

  | EndCondition.GlossaryTerm, EndCondition.GlossaryDef =>
      IF tagType = Element.T.DD OR tagType = Element.T.DT OR
         (tagType = Element.T.DL AND tok.end) THEN
        IF tagType # Element.T.DL THEN itemEndOnly^ := TRUE;
        ELSE itemEndOnly^ := FALSE; END;
        RETURN TRUE;
      ELSE RETURN FALSE;
      END;

  END;
END SequenceEndCondition;

PROCEDURE ParseElement(s: Closure;
                       tagType: Element.T;
                       tok: Lexer.Token): HTML.Sequence RAISES {Thread.Alerted} =
(* Parses the sequenceable element starting at 'tok'.  Returns a
sequence object for the element, or NIL. *)
BEGIN
  CASE tagType OF
  | Element.T.None => RETURN NIL;
  | Element.T.HTML => RETURN NIL;
  | Element.T.Head => RETURN NIL;
  | Element.T.Title =>
      s.html.title := SeqToText(ParseSequence(s, EndCondition.EndTag, Element.T.Title));
      RETURN NIL;
  | Element.T.IsIndex =>
      s.html.isIndex := TRUE;
      RETURN NIL;
  | Element.T.Base =>
      s.html.base := GetAttribute("href", tok);
      RETURN NIL;
  | Element.T.Body => RETURN NIL;
  | Element.T.H1 => RETURN ParseHeading(s, 1, tagType);
  | Element.T.H2 => RETURN ParseHeading(s, 2, tagType);
  | Element.T.H3 => RETURN ParseHeading(s, 3, tagType);
  | Element.T.H4 => RETURN ParseHeading(s, 4, tagType);
  | Element.T.H5 => RETURN ParseHeading(s, 5, tagType);
  | Element.T.H6 => RETURN ParseHeading(s, 6, tagType);
  | Element.T.A =>
      RETURN NEW(HTML.Anchor,
                 href := GetAttribute("href", tok),
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.P => RETURN NEW(HTML.Paragraph);
  | Element.T.BR => RETURN NEW(HTML.LineBreak);
  | Element.T.HR => RETURN NEW(HTML.HorizontalRule);
  | Element.T.BlockQuote =>
      RETURN NEW(HTML.BlockQuote,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.Address =>
      RETURN NEW(HTML.Address,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.DL =>
      RETURN ParseGlossary(s, GetKeyword("compact", tok))
  | Element.T.DT => (* ERROR *)
  | Element.T.DD => (* ERROR *)
  | Element.T.UL => RETURN ParseList(s, HTML.ListKind.Unordered, tagType);
  | Element.T.OL => RETURN ParseList(s, HTML.ListKind.Ordered, tagType);
  | Element.T.Menu => RETURN ParseList(s, HTML.ListKind.Menu, tagType);
  | Element.T.Dir => RETURN ParseList(s, HTML.ListKind.Dir, tagType);
  | Element.T.LI => (* ERROR *)
  | Element.T.Pre =>
      RETURN NEW(HTML.Preformatted,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.TT =>
      RETURN NEW(HTML.Typewriter,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.B =>
      RETURN NEW(HTML.Boldface,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.I =>
      RETURN NEW(HTML.Italic,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.U =>
      RETURN NEW(HTML.Underline,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.Em =>
      RETURN NEW(HTML.Emphasis,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.Strong =>
      RETURN NEW(HTML.Strong,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.Code =>
      RETURN NEW(HTML.Code,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.Samp =>
      RETURN NEW(HTML.Sample,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.Kbd =>
      RETURN NEW(HTML.Keyboard,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.Var =>
      RETURN NEW(HTML.Variable,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.Dfn =>
      RETURN NEW(HTML.Definition,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.Cite =>
      RETURN NEW(HTML.Citation,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.Img =>
      RETURN NEW(HTML.Image,
                 source := GetAttribute("src", tok),
                 ismap := GetKeyword("ismap", tok),
                 align := GetAlignment("align", tok),
                 alternate := GetAttribute("alt", tok));
  | Element.T.Oblet =>
      RETURN NEW(HTML.Oblet,
                 source := GetAttribute("src", tok));
  | Element.T.Table =>
      RETURN NEW(HTML.Table,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));
  | Element.T.TR =>
      RETURN NEW(HTML.TableRow,
                 content := ParseSequence(s, EndCondition.EndTag, tagType));

  END;
  RETURN NIL;
END ParseElement;


PROCEDURE GetAttribute (name: TEXT; tok: Lexer.ElementToken): TEXT =
  (* Returns the value of the attribute of 'tok' with name 'name', or NIL
     if there is no such attribute. *)
  VAR attr := tok.attributes;
  BEGIN
    LOOP
      IF attr = NIL THEN
        RETURN NIL
      ELSIF CIText.Equal(name, attr.name) THEN
        RETURN attr.value
      ELSE
        attr := attr.next
      END
    END
  END GetAttribute;

PROCEDURE GetKeyword (name: TEXT; tok: Lexer.ElementToken): BOOLEAN =
  (* Returns TRUE if there is an attribute of 'tok' with the name 'name',
     and FALSE otherwise. *)
  VAR attr := tok.attributes;
  BEGIN
    LOOP
      IF attr = NIL THEN
        RETURN FALSE
      ELSIF CIText.Equal(name, attr.name) THEN
        RETURN TRUE
      ELSE
        attr := attr.next
      END
    END
  END GetKeyword;

PROCEDURE GetAlignment (name: TEXT; tok: Lexer.ElementToken):
  HTML.Alignment =
  VAR align := GetAttribute(name, tok);
  BEGIN
    IF align = NIL OR CIText.Equal(align, "middle") THEN
      RETURN HTML.Alignment.Middle
    ELSIF CIText.Equal(align, "top") THEN
      RETURN HTML.Alignment.Top
    ELSIF CIText.Equal(align, "bottom") THEN
      RETURN HTML.Alignment.Bottom
    END;
    (* illegal alignment, but let's recover *)
    RETURN HTML.Alignment.Middle
  END GetAlignment;

PROCEDURE ParseHeading(s: Closure; level: INTEGER; tag: Element.T): HTML.Sequence RAISES {Thread.Alerted} =
(* Parses a heading of level 'level' and returns an HTML.Heading object. *)
VAR heading := NEW(HTML.Heading);
BEGIN
  heading.level := level;
  heading.content := ParseSequence(s, EndCondition.EndTag, tag);
  RETURN heading;
END ParseHeading;

PROCEDURE ParseGlossary(s: Closure; compact: BOOLEAN): HTML.Glossary RAISES {Thread.Alerted} =
VAR
  term: HTML.Sequence;
  definition: HTML.Sequence;
  preContent: HTML.Sequence;
  itemEndOnly := NEW(REF BOOLEAN);
  done := FALSE;
  head := NEW(HTML.GlossarySequence);
  tail := head;
BEGIN
  preContent := ParseSequence(s, EndCondition.GlossaryTerm, Element.T.DL, itemEndOnly);
  IF preContent = NIL AND NOT itemEndOnly^ THEN RETURN NIL END;

  WHILE NOT done DO
    term := ParseSequence(s, EndCondition.GlossaryTerm,
                          Element.T.DL, itemEndOnly);
    IF itemEndOnly^ THEN
      definition := ParseSequence(s, EndCondition.GlossaryDef,
                                  Element.T.DL, itemEndOnly);
    END;
    IF NOT itemEndOnly^ THEN done := TRUE; END;
    tail.next  := NEW(HTML.GlossarySequence,
                      term := term, definition := definition);
    tail := tail.next;
  END;
  tail.next := NIL;

  RETURN NEW(HTML.Glossary, compact := compact, preContent := preContent, content := head.next);
END ParseGlossary;

PROCEDURE ParseList(s: Closure;
                    kind: HTML.ListKind;
                    tagType: Element.T): HTML.List RAISES {Thread.Alerted} =
VAR
  preContent, content: HTML.Sequence;
  head := NEW(HTML.ListItem);
  tail := head;
  itemEndOnly := NEW(REF BOOLEAN);
  done := FALSE;
BEGIN

  (* Nonstandard HTML -- there *may* be an item *before* the first LI tag. *)
  preContent := ParseSequence(s, EndCondition.ListItem, tagType, itemEndOnly);
  IF preContent = NIL AND NOT itemEndOnly^ THEN RETURN NIL END;

  (* The list items *)
  WHILE NOT done DO
    content := ParseSequence(s, EndCondition.ListItem, tagType, itemEndOnly);
    IF NOT itemEndOnly^ THEN done := TRUE; END;
    IF content # NIL THEN
      tail.next := NEW(HTML.ListItem, content := content);
      tail := tail.next;
    END
  END;
  tail.next := NIL;

  RETURN NEW(HTML.List, kind := kind, preContent := preContent, content := head.next);
END ParseList;


PROCEDURE SeqToText (seq: HTML.Sequence): TEXT RAISES {Thread.Alerted} =
  <* FATAL Wr.Failure *>
  VAR wr: TextWr.T := NIL;
  BEGIN
    WHILE seq # NIL DO
      IF wr = NIL THEN wr := TextWr.New() ELSE Wr.PutChar(wr, ' '); END;
      TYPECASE seq OF | HTML.Word (word) => Wr.PutText(wr, word.word) ELSE END;
      seq := seq.next;
    END;
    IF wr = NIL THEN RETURN "" ELSE RETURN TextWr.ToText(wr) END;
  END SeqToText;

VAR tags: CITextElementTbl.T;
 (* The HTML element tag table.  We use this table to store
    the element tags for quick (hashed) lookup during parsing. *)

BEGIN 
  tags := NEW(CITextElementTbl.Default).init(50);
  FOR i := FIRST(Element.T) TO LAST(Element.T) DO
    EVAL tags.put(Element.Strings[i], i);
  END;
END HTMLParser.


