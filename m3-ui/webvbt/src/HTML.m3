(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Aug 27 13:23:50 PDT 1996 by najork                   *)
(*      modified on Tue Apr  9 16:35:34 PDT 1996 by mhb                      *)

MODULE HTML;

IMPORT Fmt, Text, TextList, Wr;

<*FATAL ANY*>

CONST IndentAmount = 2;

PROCEDURE Dump (html: T; wr: Wr.T) =

  PROCEDURE Out (t: TEXT) =
    BEGIN
      Wr.PutText(wr, t);
    END Out;

  PROCEDURE NL (indent: INTEGER) =
    BEGIN
      Wr.PutText(wr, "\n");
      FOR i := 1 TO indent DO Wr.PutChar(wr, ' '); END;
    END NL;

  PROCEDURE WalkSequence (seq: Sequence; indent: INTEGER) =
  BEGIN
    NL(indent);
    Out("SEQUENCE");
    INC(indent, IndentAmount);

    WHILE seq # NIL DO
      NL(indent);

      TYPECASE seq OF

      | NULL => Out("<null>")

      | Word (word) => Out("<word>:" & word.word);

      | Paragraph => Out("<paragraph>");

      | LineBreak => Out("<line break>");

      | HorizontalRule => Out("<horizontal rule>");

      | Glossary (glossary) =>
          VAR gs := glossary.content;
          BEGIN
            NL(indent);
            Out("GLOSSARY");
            INC(indent, IndentAmount);
            WHILE gs # NIL DO
              NL(indent);
              Out("TERM");
              WalkSequence(gs.term, indent);
              NL(indent);
              Out("DEF");
              WalkSequence(gs.definition, indent);
              gs := gs.next;
            END;
            DEC(indent, IndentAmount);
          END;

      | List (list) =>
          VAR item := list.content;
          BEGIN
            NL(indent);
            Out("LIST");
            INC(indent, IndentAmount);
            WHILE item # NIL DO
              WalkSequence(item.content, indent);
              item := item.next;
            END;
            DEC(indent, IndentAmount);
          END;

      | Preformatted (pre) =>
          Out("<preformatted>");
          WalkSequence(pre.content, indent);

      | Typewriter (format) =>
          Out("<typewriter>");
          WalkSequence(format.content, indent);

      | Boldface (format) =>
          Out("<boldface>");
          WalkSequence(format.content, indent);

      | Italic (format) =>
          Out("<italic>");
          WalkSequence(format.content, indent);

      | Underline (format) =>
          Out("<underline>");
          WalkSequence(format.content, indent);

      | Emphasis (format) =>
          Out("<emphasis>");
          WalkSequence(format.content, indent);

      | Strong (format) =>
          Out("<strong>");
          WalkSequence(format.content, indent);

      | Code (format) =>
          Out("<code>");
          WalkSequence(format.content, indent);

      | Sample (format) =>
          Out("<sample>");
          WalkSequence(format.content, indent);

      | Keyboard (format) =>
          Out("<keyboard>");
          WalkSequence(format.content, indent);

      | Definition (format) =>
          Out("<definition>");
          WalkSequence(format.content, indent);

      | Variable (format) =>
          Out("<variable>");
          WalkSequence(format.content, indent);

      | Citation (format) =>
          Out("<citation>");
          WalkSequence(format.content, indent);

      | Anchor (anchor) =>
          IF anchor.href = NIL THEN
            Out("NAME-ANCHOR:");
            WalkSequence(anchor.content, indent);
          ELSE
            Out("ANCHOR:");
            WalkSequence(anchor.content, indent);
          END;

      | Heading (heading) =>
          NL(indent);
          Out("HEADING" & Fmt.Int(heading.level));
          WalkSequence(heading.content, indent);

      | Address (addr) =>
          Out("<address>");
          WalkSequence(addr.content, indent);

      | BlockQuote (quote) =>
          Out("<block quote>");
          WalkSequence(quote.content, indent);

      | Image (image) =>
          Out("<image>");
          VAR alt := image.alternate;
          BEGIN
            IF alt = NIL THEN alt := "<<IMAGE>>" END;
            Out(alt);
          END;

      | Oblet (oblet) => Out("<oblet:" & oblet.source & ">");

      | Table (format) =>
          Out("<table>");
          WalkSequence(format.content, indent);

      | TableRow (format) =>
          Out("<table row>");
          WalkSequence(format.content, indent);

      ELSE
        Out("<????>");

      END;
      seq := seq.next;
    END;
    DEC(indent, IndentAmount);
  END WalkSequence;

  BEGIN
    Out("TITLE: " & html.title);
    IF html.body # NIL THEN
      Out("BODY");
      NL(0);
      WalkSequence(html.body, 0);
    END;
  END Dump;



PROCEDURE GetLinks (html: T): TextList.T =
  VAR links: TextList.T;

  PROCEDURE LinksInSequence (seq: Sequence) =
    BEGIN
      WHILE seq # NIL DO
        TYPECASE seq OF

        | NULL =>

        | Word =>

        | Paragraph =>

        | LineBreak =>

        | HorizontalRule =>

        | Glossary (glossary) =>
            VAR g := glossary.content;
            BEGIN
              WHILE g # NIL DO
                LinksInSequence(g.term);
                LinksInSequence(g.definition);
                g := g.next;
              END;
            END;

        | List (list) =>
            VAR item := list.content;
            BEGIN
              WHILE item # NIL DO
                LinksInSequence(item.content);
                item := item.next;
              END;
            END;

        | Preformatted (pre) => LinksInSequence(pre.content);

        | Typewriter (format) => LinksInSequence(format.content);

        | Boldface (format) => LinksInSequence(format.content);

        | Italic (format) => LinksInSequence(format.content);

        | Underline (format) => LinksInSequence(format.content);

        | Emphasis (format) => LinksInSequence(format.content);

        | Strong (format) => LinksInSequence(format.content);

        | Code (format) => LinksInSequence(format.content);

        | Sample (format) => LinksInSequence(format.content);

        | Keyboard (format) => LinksInSequence(format.content);

        | Definition (format) => LinksInSequence(format.content);

        | Variable (format) => LinksInSequence(format.content);

        | Citation (format) => LinksInSequence(format.content);

        | Anchor (anchor) =>
            IF anchor.href # NIL THEN
              VAR pos := Text.FindChar(anchor.href, '#', 0);
              BEGIN
                IF pos = -1 THEN
                  links := TextList.Cons(anchor.href, links);
                END
              END
            END;

        | Heading (heading) => LinksInSequence(heading.content);

        | Address (addr) => LinksInSequence(addr.content);

        | BlockQuote (quote) => LinksInSequence(quote.content);

        | Image (<*NOWARN*> image ) => 
          (* links := TextList.Cons(image.source, links); *)
        | Oblet =>
        ELSE
        END;
        seq := seq.next;
      END;
    END LinksInSequence;
  BEGIN
    links := NIL;
    LinksInSequence(html.body);
    RETURN TextList.ReverseD(links);
  END GetLinks;
 
BEGIN
END HTML.
