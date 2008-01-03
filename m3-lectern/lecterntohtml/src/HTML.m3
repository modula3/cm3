(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(* Last modified on Mon Aug 29 16:41:17 PDT 1994 by mcjones        *)

MODULE HTML;

IMPORT Fmt;

REVEAL T = BRANDED REF TEXT;

PROCEDURE New(t: TEXT): T =
  VAR r := NEW(T);
  BEGIN
    r^ := t;
    RETURN r
  END New;

PROCEDURE N(tag: TEXT; h: T; newline := FALSE): T =
  CONST Close = ARRAY BOOLEAN OF TEXT{">", ">\n"};
  BEGIN
    RETURN New(
      "<" & tag & Close[newline]
      & ToText(h)
      & "</" & tag & Close[newline])
  END N;

PROCEDURE ToText(t: T): TEXT = BEGIN RETURN t^ END ToText;

PROCEDURE Document(
    title: T := NIL;
    body: T := NIL)
  : T =
  BEGIN
    RETURN N(
      "HTML",
      Cat(
        N(
          "HEAD",
          N("TITLE", title, TRUE),
          TRUE),
        N("BODY", body, TRUE)
        ), 
      TRUE
      )
  END Document;

PROCEDURE Section(
    level: [1..6] := 1;
    heading: T := NIL;
    body: T := NIL)
  : T =
  CONST Level = ARRAY [1..6] OF TEXT{"H1", "H2", "H3", "H4", "H5", "H6"};
  BEGIN
    RETURN Cat(N(Level[level], heading, newline := TRUE), body)
  END Section;

PROCEDURE Paragraph(t: T): T =
  BEGIN
    RETURN New("<P>" & ToText(t))
  END Paragraph;

PROCEDURE Break(t: T): T =
  BEGIN
    RETURN New(ToText(t) & "<BR>")
  END Break;

PROCEDURE Text(t: TEXT): T = BEGIN RETURN New(t) END Text;

PROCEDURE Preformatted(t: TEXT; width := -1): T =
  BEGIN
    IF width = -1 THEN
      RETURN New("<PRE>" & t & "</PRE>")
    ELSE
      RETURN New("<PRE " & Fmt.Int(width) & ">" & t & "</PRE>")
    END
  END Preformatted;

PROCEDURE Anchor(
    caption: T;
    url: TEXT := NIL;
    name: TEXT := NIL;
    ): T =
  VAR begin := "<A";
  BEGIN
    IF url # NIL THEN
      begin := begin & " HREF=" & url;
    END;
    IF name # NIL THEN
      begin := begin & " NAME=" & name;
    END;
    RETURN New(begin & ">" & ToText(caption) & "</A>")
  END Anchor;

PROCEDURE List(
    READONLY elts: ARRAY OF T;
    style := ListStyle.Unnumbered)
  : T =
  CONST
    ListBeg = ARRAY ListStyle OF TEXT{"<UL>", "<OL>", "<MENU>", "<DIR>"};
    ListEnd = ARRAY ListStyle OF TEXT{"</UL>", "</OL>", "</MENU>", "</DIR>"};
  VAR t := "";
  BEGIN
    FOR i := 0 TO LAST(elts) DO
      t := t & "\n<LI>" & ToText(elts[i]) & "\n"
    END;
    RETURN New(ListBeg[style] & t & ListEnd[style])
  END List;

PROCEDURE DefinitionList(
    READONLY terms, defs: ARRAY OF T;
    compact := FALSE)
  : T =
  VAR t := ""; beg: TEXT;
  BEGIN
    IF compact THEN beg := "<DL COMPACT>\n" ELSE beg := "<DL>\n" END;
    <*ASSERT NUMBER(terms) = NUMBER(defs)*>
    FOR i := 0 TO LAST(terms) DO
      t := "<DT>" & ToText(terms[i]) & "<DD>" & ToText(defs[i]) & "\n"
    END;
    RETURN New(beg & t & "</DL>\n")
  END DefinitionList;

PROCEDURE BlockQuote(t: T): T = BEGIN RETURN N("BLOCKQUOTE", t, TRUE) END BlockQuote;

PROCEDURE Address(t: T): T = BEGIN RETURN N("ADDRESS", t, TRUE) END Address;

PROCEDURE Defined(t: T): T = BEGIN RETURN N("DFN", t) END Defined;
PROCEDURE Emphasis(t: T): T = BEGIN RETURN N("EM", t) END Emphasis;
PROCEDURE Cite(t: T): T = BEGIN RETURN N("CITE", t) END Cite;
PROCEDURE Code(t: T): T = BEGIN RETURN N("CODE", t) END Code;
PROCEDURE Kbd(t: T): T = BEGIN RETURN N("KBD", t) END Kbd;
PROCEDURE Samp(t: T): T = BEGIN RETURN N("SAMP", t) END Samp;
PROCEDURE Strong(t: T): T = BEGIN RETURN N("STRONG", t) END Strong;
PROCEDURE Var(t: T): T = BEGIN RETURN N("VAR", t) END Var;

PROCEDURE Bold(t: T): T = BEGIN RETURN N("B", t) END Bold;
PROCEDURE Italic(t: T): T = BEGIN RETURN N("I", t) END Italic;
PROCEDURE TT(t: T): T = BEGIN RETURN N("TT", t) END TT;

PROCEDURE Cat(t1, t2, t3: T := NIL): T =
  VAR t := "";
  BEGIN
    IF t1 # NIL THEN t := t & ToText(t1) END;
    IF t2 # NIL THEN t := t & ToText(t2) END;
    IF t3 # NIL THEN t := t & ToText(t3) END;
    RETURN New(t)
  END Cat;

PROCEDURE CatArray(READONLY a: ARRAY OF T): T =
  VAR t := "";
  BEGIN
    FOR i := 0 TO LAST(a) DO
      IF a[i] # NIL THEN t := t & ToText(a[i])
      END
    END;
    RETURN New(t)
  END CatArray;

BEGIN
  Empty := Text("");
  HorizontalRule := New("<HR>")
END HTML.
