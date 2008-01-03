(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Sep 28 21:17:31 PDT 1995 by mhb                      *)

MODULE HTMLVBTText;

IMPORT Filter, Font, HTML, HTMLVBT, Text, TextEditVBT, TextPort, 
  TextPortButton, TextPortWithButtons, VBT;

CONST
 FontName = "-*-fixed-bold-r-semicondensed-*-*-120-*-*-*-*-iso8859-1";
 IndentAmount = 4;

REVEAL
  T = Public BRANDED OBJECT
        title: TEXT;
      OVERRIDES
        init    := Init;
        hotlink := HotLink;
      END;

TYPE
 URLButton = TextPortButton.T OBJECT
    v: T; 
    url: TEXT;
  OVERRIDES
    callback := URLButtonCallback;
  END;

PROCEDURE Init (v: T; html: HTML.T): T =
  VAR
    tp := NEW(TextPortWithButtons.T).init(readOnly := FALSE);
    te := NEW(TextEditVBT.T);
  BEGIN
    WalkHTML(v, html, tp);
    tp.setFont(Font.FromName(ARRAY OF TEXT{FontName}));
    te.tp := tp;
    EVAL TextEditVBT.T.init(te);
    EVAL HTMLVBT.T.init(v, html);
    EVAL Filter.Replace(v, te);
    RETURN v
  END Init;

PROCEDURE HotLink (<* UNUSED *> self: T; 
                   <* UNUSED *> url: TEXT;
                   <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
  END HotLink;

PROCEDURE URLButtonCallback (button: URLButton; READONLY cd: VBT.MouseRec) =
  BEGIN
    button.v.hotlink (button.url, cd)
  END URLButtonCallback;

TYPE
  WalkInfo = RECORD
    v: T;
    insert: BOOLEAN;  (* If TRUE, insert into tp, else return the text. *)
    indent: INTEGER;
    lastChar: CHAR;
    tp: TextPortWithButtons.T;
  END;

PROCEDURE WalkHTML (v: T; html: HTML.T; tp: TextPort.T) =
  VAR info := NEW(REF WalkInfo, v := v, insert := FALSE, indent := 0, tp := tp);
  BEGIN
    info.insert := TRUE;
    IF html.body # NIL THEN
      EVAL WalkSequence(html.body, info);
    END;
  END WalkHTML;

PROCEDURE Consume (info: REF WalkInfo; text: TEXT): TEXT =
  BEGIN
    IF info.insert AND NOT Text.Empty(text) THEN
      IF info.lastChar # '\n' OR NOT Text.Equal(text, "\n") THEN
        TextPort.Insert(info.tp, text);
        info.lastChar := Text.GetChar(text, Text.Length(text) - 1);
      END;
      RETURN "";
    ELSE
      RETURN text;
    END;
  END Consume;

PROCEDURE WalkSequence (seq: HTML.Sequence; info: REF WalkInfo):
  TEXT =
  VAR
    this     : TEXT;
    returnVal       := "";
  BEGIN
    WHILE seq # NIL DO
      this := "";

      TYPECASE seq OF
      | NULL => RETURN "";
      | HTML.Word (word) => 
          this := Consume(info, word.word);
      | HTML.Paragraph =>
          this := Consume(info, "\n\n" & Spaces(info.indent));
      | HTML.LineBreak =>
          this := Consume(info, "\n" & Spaces(info.indent));
      | HTML.HorizontalRule =>
          this := Consume(info,
              "\n-----------------------------------------------------------------\n")
              & Spaces(info.indent);
      | HTML.Glossary (glossary) =>
          this := WalkGlossary(glossary, info);
      | HTML.List (list) => 
          this := WalkList(list, info);
      | HTML.Preformatted (pre) =>
          this := WalkSequence(pre.content, info);
      | HTML.Typewriter (format) =>
          this := WalkSequence(format.content, info);
      | HTML.Boldface (format) =>
          this := WalkSequence(format.content, info);
      | HTML.Italic (format) =>
          this := WalkSequence(format.content, info);
      | HTML.Underline (format) =>
          this := WalkSequence(format.content, info);
      | HTML.Emphasis (format) =>
          this := WalkSequence(format.content, info);
      | HTML.Strong (format) =>
          this := WalkSequence(format.content, info);
      | HTML.Code (format) =>
          this := WalkSequence(format.content, info);
      | HTML.Sample (format) =>
          this := WalkSequence(format.content, info);
      | HTML.Keyboard (format) =>
          this := WalkSequence(format.content, info);
      | HTML.Definition (format) =>
          this := WalkSequence(format.content, info);
      | HTML.Variable (format) =>
          this := WalkSequence(format.content, info);
      | HTML.Citation (format) =>
          this := WalkSequence(format.content, info);
      | HTML.Anchor (anchor) =>
          this := WalkAnchor(anchor, info);
      | HTML.Heading (heading) =>
          this := WalkHeading(heading, info);
      | HTML.Address (addr) =>
          this := Consume(info, "\n")
                    & WalkSequence(addr.content, info)
                    & Consume(info, "\n");
      | HTML.BlockQuote (quote) =>
          INC(info.indent, IndentAmount);
          this := Consume(info, "\n" & Spaces(info.indent))
                    & WalkSequence(quote.content, info)
                    & Consume(info, "\n");
          DEC(info.indent, IndentAmount);
      | HTML.Image (image) => 
           VAR alt := image.alternate; BEGIN
              IF alt = NIL THEN alt := "<<IMAGE>>" END;
              this := Consume(info, alt);
           END;
      | HTML.Oblet (oblet) => 
           this := Consume(info, "\n\nThis view cannot display oblets; sorry. [" 
                     & oblet.source & "]\n\n");
      ELSE
        this := Consume(info, "????");
      END;
      
      returnVal := returnVal & this;
      IF (ISTYPE(seq, HTML.Word) OR ISTYPE(seq,HTML.Image)) AND seq.next # NIL AND 
         (ISTYPE(seq.next, HTML.Word) OR ISTYPE(seq.next, HTML.Image)) THEN 
        returnVal := returnVal & Consume(info, " ") 
      END;
      seq := seq.next;
    END;                         (* WHILE *)
    RETURN returnVal;
  END WalkSequence;

PROCEDURE Spaces (num: INTEGER): TEXT =
  CONST 
    Indent0 = "";
    Indent4 = "    ";
    Indent8  = Indent4  & Indent4;
    Indent12 = Indent8  & Indent4;
    Indent16 = Indent12 & Indent4;
  VAR 
    this := "";
  BEGIN
    IF num = 0 THEN RETURN Indent0 
    ELSIF num = 4 THEN RETURN Indent4
    ELSIF num = 8 THEN RETURN Indent8 
    ELSIF num = 12 THEN RETURN Indent12 
    ELSIF num = 16 THEN RETURN Indent16
    END;
    FOR i := 1 TO num DO this := this & " "; END;
    RETURN this;
  END Spaces;

PROCEDURE WalkHeading (heading: HTML.Heading; info: REF WalkInfo):
  TEXT =
  VAR nl: TEXT;
  BEGIN
    IF Text.Empty(TextPort.GetText(info.tp)) THEN nl := "" 
    ELSE nl := "\n" END;
    CASE heading.level OF
    | 1 =>
        RETURN Consume(info, nl & nl & "** ")
                 & WalkSequence(heading.content, info)
                 & Consume(info, " **\n\n");
    | 2 =>
        RETURN Consume(info, nl & nl & "* ")
                 & WalkSequence(heading.content, info)
                 & Consume(info, " *\n");
    | 3 =>
        RETURN Consume(info, nl & nl)
                 & WalkSequence(heading.content, info)
                 & Consume(info, "\n");
    | 4 .. 6 =>
        RETURN Consume(info, nl)
                 & WalkSequence(heading.content, info)
                 & Consume(info, "\n");
    ELSE (*Error*)
      RETURN "";
    END;
  END WalkHeading;

PROCEDURE WalkAnchor(anchor: HTML.Anchor; info: REF WalkInfo): TEXT =
VAR button := NEW(URLButton);
BEGIN
  button.v := info.v;
  button.url := "Bogus default URL";
  IF anchor.href = NIL THEN
    (* Probably a NAME anchor -- ignore it. *)
    RETURN WalkSequence(anchor.content, info);
  ELSE
    VAR
      pos := Text.FindChar(anchor.href, '#', 0);
      href := anchor.href;
    BEGIN
      IF pos # -1 THEN
	(* Pointer to a NAME anchor. *)
	IF pos = 0 THEN
	  RETURN WalkSequence(anchor.content, info); (* '#' is first char. *)
	ELSE
	  href := Text.Sub(href, 0, pos);  (* Kill off what's after the '#'. *)
	END;
      END;
      info.insert := FALSE;
      button.label := WalkSequence(anchor.content, info);
      info.insert := TRUE;
      button.url := href;
      button.v := info.v;
      info.tp.insertButton(button);   (* Add button to tp. *)
      RETURN "";
    END;
  END;
END WalkAnchor;

PROCEDURE WalkGlossary (glossary: HTML.Glossary;
                        info    : REF WalkInfo   ): TEXT =
  VAR
    this := Consume(info, "\n" & Spaces(info.indent));
    gs   := glossary.content;
  BEGIN
    WHILE gs # NIL DO
      this := this & WalkSequence(gs.term, info)
                & Consume(info, "\n" & Spaces(info.indent));
      INC(info.indent, IndentAmount);
      this := this & WalkSequence(gs.definition, info);
      DEC(info.indent, IndentAmount);
      gs := gs.next;
    END;
    RETURN this;
  END WalkGlossary;


PROCEDURE WalkList (list: HTML.List; info: REF WalkInfo): TEXT =
  VAR
    this := "";
    item := list.content;
  BEGIN
    INC(info.indent, IndentAmount);
    WHILE item # NIL DO
      this := this & Consume(info, "\n" & Spaces(info.indent))
                & WalkSequence(item.content, info);
      item := item.next;
    END;
    DEC(info.indent, IndentAmount);
    RETURN this;
  END WalkList;

BEGIN END HTMLVBTText.
