(*
   FBEPostScript.m3
   A Postscript backend for Formatter.i3.
   David Nichols, Xerox PARC
   July, 1991

   $Id: FBEPostScript.m3,v 1.2 2005-11-17 03:23:56 hosking Exp $
*)
(* Copyright (c) 1991 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works based
   upon this software are permitted.  Any distribution of this software or
   derivative works must comply with all applicable United States export
   control laws.  This software is made available AS IS, and Xerox Corporation
   makes no warranty about the software, its performance or its conformity to
   any specification. *)

MODULE FBEPostScript;

IMPORT FBE, FileRd, Fmt, RefList, Rd, Lex, Text, Wr, Thread, Env;
IMPORT OSError, FloatMode, TextRd, Rsrc, AFMBundle;

<* FATAL Thread.Alerted *>

CONST
  PtsPerInch        = 72.0;
  FullPageWidth     = 8.5 * PtsPerInch;
  FullPageHeight    = 11.0 * PtsPerInch;
  DefaultMargin     = 0.5 * PtsPerInch;
  DefaultGully      = 10.0;
  DefaultPageWidth  = 7.0 * PtsPerInch;
  DefaultPageHeight = 9.5 * PtsPerInch;
  DefaultSpacing    = 2.0;
  MaxChars          = 128;       (* chars to buffer in the FBE *)
(*
  AFMDir            = "/usr/local/lib/ps/";
*)

(* There are three positions that are interesting: 1) the position we're at due
   to all the commands we've received, 2) the position that the text we've
   saved but not yet output should start at, and 3) the position we left the
   PostScript interpreter in last time we did something.  We don't track the
   last one but instead do a moveto before each show.

   We have a similar situation for fonts, except there is no analogue for (1)
   since we don't have a notion of "current font" in this interface.  We do
   track the PostScript font to avoid doing setfonts before each show. *)
TYPE
  T = FBE.T OBJECT
        wr             : Wr.T;   (* the writer *)
        prologWritten           := FALSE;
        allFonts       : RefList.T := NIL;
        allFontFamilies: RefList.T := NIL;
        title          : TEXT;   (* title for each page *)
        comment  : ARRAY [0 .. 1] OF TEXT;  (* comment for each page *)
        landscape: BOOLEAN;                 (* print landscape mode? *)
        physPageWidth          := FullPageWidth; (* physical size *)
        physPageHeight         := FullPageHeight;
        pageNum                := 0; (* current page number *)
        topMargin              := DefaultMargin; (* margins around code *)
        bottomMargin           := DefaultMargin;
        leftMargin             := DefaultMargin;
        rightMargin            := DefaultMargin;
        gully                  := DefaultGully;
        topGully               := DefaultGully;
        pageWidth              := DefaultPageWidth; (* logical size *)
        pageHeight             := DefaultPageHeight;
        spacing                := DefaultSpacing; (* between line spacing *)
        curX, curY             := 0.0; (* where we are on the page *)
        lineHeight             := 0.0; (* height of each line *)
        psFont        : Font;    (* font the PS code is in *)
        (* Buffer of chars we haven't output yet. *)
        chars : ARRAY [0 .. MaxChars - 1] OF CHAR;
        nChars: INTEGER;
        charX, charY         := 0.0; (* where chars go on page *)
        charFont    : Font;      (* font they're in *)
      OVERRIDES
        GetFont   := GetFont;
        PageWidth := PageWidth;
        TextWidth := TextWidth;
        CharWidth := CharWidth;
        NewLine   := NewLine;
        Goto      := Goto;
        GetPos    := GetPos;
        PutText   := PutText;
        PutChar   := PutChar;
        Flush     := Flush;
        Close     := Close;
      END;
  Font = FBE.Font OBJECT
           family: TEXT;         (* font family *)
           scale : REAL;         (* scale factor *)
           psName: TEXT;         (* name of the symbol used in PS code *)
           widths: ARRAY [0 .. 255] OF REAL;
         END;

VAR genCounter := 0;

(* Generate a PostScript id for a font. *)
PROCEDURE GenName (): TEXT =
  BEGIN
    INC(genCounter);
    RETURN Fmt.F("font%s", Fmt.Int(genCounter));
  END GenName;

(* Parse the next integer from a string. *)
PROCEDURE NextInt (t: TEXT; VAR i: INTEGER): INTEGER  RAISES {Lex.Error} =
  VAR
    len            := Text.Length(t);
    start: INTEGER;
  BEGIN
    WHILE i < len AND (Text.GetChar(t, i) < '0' OR Text.GetChar(t, i) > '9') DO
      INC(i);
    END;
    start := i;
    WHILE i < len AND ('0' <= Text.GetChar(t, i) AND Text.GetChar(t, i) <= '9') DO
      INC(i);
    END;
    len := i - start;
    IF (len <= 0) THEN RAISE Lex.Error; END;
    TRY
      RETURN Lex.Int(TextRd.New(Text.Sub(t, start, len)));
    EXCEPT FloatMode.Trap, Rd.Failure =>
      RAISE Lex.Error;
    END;
  END NextInt;

PROCEDURE GetAFMFile( family: TEXT ): Rd.T RAISES {FBE.Failed} =
  BEGIN
    TRY
      RETURN Rsrc.Open(family & ".afm", Rsrc.BuildPath(AFMBundle.Get()));
    EXCEPT
    | Rsrc.NotFound =>
      RAISE FBE.Failed(
                NEW(FBE.Failure, info := "couldn't open font width file"));
    END;
  END GetAFMFile;
    
(*
(* look for an AFM file in a path of possible directories *)
PROCEDURE GetAFMFile( family: TEXT ) : Rd.T RAISES {FBE.Failed} =
  PROCEDURE TryOpen(dir: TEXT) : Rd.T RAISES {OSError.E} =
    BEGIN
      RETURN FileRd.Open(dir & family & ".afm");
    END TryOpen;
  VAR
    r     : Rd.T;
    dir   : TEXT;
    dirs  : TEXT;
    i     : INTEGER := 0;
  BEGIN
    dirs := Env.Get("AFMPATH");
    IF dirs = NIL THEN
      TRY
        r := TryOpen(AFMDir);
        RETURN r;
      EXCEPT OSError.E =>
      END;
    ELSE
      REPEAT
        i := Text.FindChar(dirs, ':', 0);
        CASE i OF
        | -1 => 
          (* last element of path *)
          dir := dirs & "/";
          dirs := NIL;
        | 0  => 
          (* leading ":", ignore it *)
          dir := NIL; 
          dirs := Text.Sub(dirs, 1); 
        ELSE
          (* get path element *)
          dir := Text.Sub(dirs, 0, i) & "/";
          dirs := Text.Sub(dirs, i+1);
        END;
        IF dir # NIL THEN
          TRY
            r := TryOpen(dir);
            RETURN r;
          EXCEPT OSError.E =>
          END;
        END;
      UNTIL i = -1;
    END;
    RAISE FBE.Failed(
              NEW(FBE.Failure, info := "couldn't open font width file"));
  END GetAFMFile;
*)

(* Find the AFM file with the char widths and read it in. *)
PROCEDURE GetFont (o: T; fontName: TEXT): FBE.Font RAISES {FBE.Failed} =
  VAR
    family: TEXT;                (* font family name *)
    scale : REAL;                (* scale factor *)
    r     : Rd.T;
    t     : TEXT;
    i     : INTEGER;
    font  : Font;
    c, w  : INTEGER;
  BEGIN
    (* parse fontname *)
    i := Text.Length(fontName);
    WHILE i > 0 AND ('0' <= Text.GetChar(fontName, i - 1)
                       AND Text.GetChar(fontName, i - 1) <= '9') DO
      DEC(i);
    END;
    IF i = 0 OR i = Text.Length(fontName) THEN
      RAISE FBE.Failed(NEW(FBE.Failure, info := "Illegal font name."));
    END;
    family := Text.Sub(fontName, 0, i);
    TRY
      scale := FLOAT(Lex.Int(TextRd.New(Text.Sub(fontName, i,LAST(INTEGER)))));
    EXCEPT
      Lex.Error, Rd.Failure, FloatMode.Trap =>
        RAISE FBE.Failed(NEW(FBE.Failure, info := "bad font name"));
    END;
    (* open metrics file *)
    r := GetAFMFile(family);

    (* allocate new font object *)
    font := NEW(Font);
    font.family := family;
    font.scale := scale;
    font.psName := GenName();
    o.allFonts := RefList.Cons(font, o.allFonts);
    IF NOT RefList.Member(o.allFontFamilies, family) THEN
      o.allFontFamilies := RefList.Cons(family, o.allFontFamilies);
    END;
    o.lineHeight := MAX(scale + o.spacing, o.lineHeight);
    FOR i := 0 TO 255 DO font.widths[i] := 0.0; END;
    (* read metrics *)
    TRY
      LOOP
        TRY t := Rd.GetLine(r) EXCEPT | Rd.EndOfFile => EXIT; END;
        IF Text.Length(t) >= 2 AND Text.GetChar(t, 0) = 'C'
             AND Text.GetChar(t, 1) = ' ' THEN
          (* This line has a char width. *)
          i := 2;
          c := NextInt(t, i);
          w := NextInt(t, i);
          IF 0 <= c AND c <= 255 THEN
            font.widths[c] := FLOAT(w) * scale / 1000.0;
          END;
        END;
      END;
    EXCEPT
    | Rd.Failure, Lex.Error =>
        RAISE FBE.Failed(
                NEW(FBE.Failure, info := "error reading font metric file"));
    END;
    RETURN font;
  END GetFont;

PROCEDURE PageWidth (o: T): REAL =
  BEGIN
    RETURN o.pageWidth;
  END PageWidth;

PROCEDURE TextWidth (<*UNUSED*> o: T; t: TEXT; font: FBE.Font): REAL =
  VAR
    width        := 0.0;
    myFont: Font := font;
  BEGIN
    FOR i := 0 TO Text.Length(t) - 1 DO
      width := width + myFont.widths[ORD(Text.GetChar(t, i))];
    END;
    RETURN width;
  END TextWidth;

PROCEDURE CharWidth (<*UNUSED*> o: T; c: CHAR; font: FBE.Font): REAL =
  VAR myFont: Font := font;
  BEGIN
    RETURN myFont.widths[ORD(c)];
  END CharWidth;

(* Output one char, quoting it for PostScript strings. *)
PROCEDURE QuoteChar (o: T; c: CHAR) RAISES {Wr.Failure} =
  BEGIN
    IF c < ' ' OR c > '~' OR c = '(' OR c = ')' OR c = '\\' THEN
      Wr.PutText(o.wr, Fmt.F("\\%03s", Fmt.Int(ORD(c), 8)));
    ELSE
      Wr.PutChar(o.wr, c);
    END;
  END QuoteChar;

(* Flush any text that hasn't been output yet. *)
PROCEDURE FlushText (o: T) RAISES {Wr.Failure} =
  BEGIN
    IF o.nChars = 0 THEN RETURN END;
    IF o.psFont # o.charFont THEN
      <* ASSERT Text.Equal(Text.Sub(o.charFont.psName, 0, 4), "font") *>
      Wr.PutText(o.wr, o.charFont.psName);
      Wr.PutText(o.wr, " setfont\n");
      o.psFont := o.charFont;
    END;
    Wr.PutText(o.wr, "(");
    FOR i := 0 TO o.nChars - 1 DO QuoteChar(o, o.chars[i]); END;
    Wr.PutText(o.wr, Fmt.F(") %s %s T\n", Fmt.Real(o.charX), Fmt.Real(o.charY)));
    o.nChars := 0;
  END FlushText;

(* Print out code to begin each page.  Forgets current font so pages will be
   independent. *)
PROCEDURE StartPage (o: T) RAISES {Wr.Failure} =
  BEGIN
    INC(o.pageNum);
    IF o.landscape THEN
      IF o.pageNum MOD 2 = 1 THEN
        WITH realPageNum = (o.pageNum + 1) DIV 2,
             rpnStr      = Fmt.Int(realPageNum)   DO
          Wr.PutText(o.wr, Fmt.F("%%Page: %s %s\n", rpnStr, rpnStr));
          Wr.PutText(o.wr, Fmt.F("(%s) StartPage\n", rpnStr));
        END;
      ELSE
        Wr.PutText(o.wr, "NextColumn\n");
      END;
    ELSE
      WITH rpnStr = Fmt.Int(o.pageNum) DO
        Wr.PutText(o.wr, Fmt.F("%%Page: %s %s\n", rpnStr, rpnStr));
        Wr.PutText(o.wr, Fmt.F("(%s) StartPage\n", rpnStr));
      END;
    END;
    o.psFont := NIL;
  END StartPage;

PROCEDURE EndPage (o: T; finalTime := FALSE) RAISES {Wr.Failure} =
  BEGIN
    FlushText(o);
    IF finalTime OR NOT o.landscape OR o.pageNum MOD 2 = 0 THEN
      Wr.PutText(o.wr, "showpage\n");
    END;
    o.curY := o.pageHeight - o.lineHeight;
    o.curX := 0.0;
  END EndPage;

(* Output old page. *)
PROCEDURE NewPage (o: T) RAISES {Wr.Failure} =
  BEGIN
    EndPage(o);
    StartPage(o);
  END NewPage;

PROCEDURE NewLine (o: T) RAISES {FBE.Failed} =
  BEGIN
    TRY
      IF NOT o.prologWritten THEN WriteProlog(o); END;
      FlushText(o);
      IF o.curY - o.lineHeight < 0.0 THEN NewPage(o); END;
      o.curX := 0.0;
      o.curY := o.curY - o.lineHeight;
    EXCEPT
      Wr.Failure (r) =>
        RAISE
          FBE.Failed(NEW(FBE.WrFailure, info := "writer failure", wrRef := r));
    END;
  END NewLine;

PROCEDURE Goto (o: T; pos: REAL) RAISES {FBE.Failed} =
  BEGIN
    TRY
      IF NOT o.prologWritten THEN WriteProlog(o); END;
      FlushText(o);
      o.curX := pos;
    EXCEPT
      Wr.Failure (r) =>
        RAISE
          FBE.Failed(NEW(FBE.WrFailure, info := "writer failure", wrRef := r));
    END;
  END Goto;

PROCEDURE GetPos (o: T): REAL =
  BEGIN
    RETURN o.curX;
  END GetPos;

PROCEDURE PutText (o: T; t: TEXT; font: FBE.Font) RAISES {FBE.Failed} =
  VAR
    c     : CHAR;
    myFont: Font := font;
  BEGIN
    TRY
      IF NOT o.prologWritten THEN WriteProlog(o); END;
      IF myFont # o.charFont THEN FlushText(o); END;
      o.charFont := myFont;
      FOR i := 0 TO Text.Length(t) - 1 DO
        IF o.nChars >= MaxChars THEN FlushText(o); END;
        c := Text.GetChar(t, i);
        IF c = '\n' THEN
          NewLine(o);
        ELSIF c = '\r' THEN
          (* ignore incoming carriage return characters *)
        ELSE
          IF o.nChars = 0 THEN
            (* Need to remember start point. *)
            o.charX := o.curX;
            o.charY := o.curY;
          END;
          o.chars[o.nChars] := c;
          o.curX := o.curX + myFont.widths[ORD(c)];
          INC(o.nChars);
        END;
      END;
    EXCEPT
      Wr.Failure (r) =>
        RAISE
          FBE.Failed(NEW(FBE.WrFailure, info := "writer failure", wrRef := r));
    END;
  END PutText;

PROCEDURE PutChar (o: T; c: CHAR; font: FBE.Font) RAISES {FBE.Failed} =
  VAR myFont: Font := font;
  BEGIN
    TRY
      IF NOT o.prologWritten THEN WriteProlog(o); END;
      IF c = '\n' THEN
        NewLine(o);
      ELSIF c = '\r' THEN
        (* ignore incoming carriage return characters *)
      ELSE
        IF myFont # o.charFont OR o.nChars > MaxChars THEN FlushText(o); END;
        IF o.nChars = 0 THEN
          (* Need to remember start point. *)
          o.charX := o.curX;
          o.charY := o.curY;
        END;
        o.charFont := myFont;
        o.chars[o.nChars] := c;
        o.curX := o.curX + myFont.widths[ORD(c)];
        INC(o.nChars);
      END;
    EXCEPT
      Wr.Failure (r) =>
        RAISE
          FBE.Failed(NEW(FBE.WrFailure, info := "writer failure", wrRef := r));
    END;
  END PutChar;

PROCEDURE Flush (o: T) RAISES {FBE.Failed} =
  BEGIN
    TRY
      IF NOT o.prologWritten THEN WriteProlog(o); END;
      FlushText(o);
      Wr.Flush(o.wr);
    EXCEPT
      Wr.Failure (r) =>
        RAISE
          FBE.Failed(NEW(FBE.WrFailure, info := "writer failure", wrRef := r));
    END;
  END Flush;

PROCEDURE Close (o: T) RAISES {FBE.Failed} =
  BEGIN
    TRY
      IF NOT o.prologWritten THEN WriteProlog(o); END;
      FlushText(o);
      EndPage(o, TRUE);
      (* Write the trailer. *)
      Wr.PutText(o.wr, "%%Trailer\n");
      IF o.landscape THEN
        Wr.PutText(
          o.wr, Fmt.F("%%Pages: %s\n", Fmt.Int((o.pageNum + 1) DIV 2)));
      ELSE
        Wr.PutText(o.wr, Fmt.F("%%Pages: %s\n", Fmt.Int(o.pageNum)));
      END;
      Wr.PutText(o.wr, "%%EOF\n");
      Wr.Flush(o.wr);
    EXCEPT
      Wr.Failure (r) =>
        RAISE
          FBE.Failed(NEW(FBE.WrFailure, info := "writer failure", wrRef := r));
    END;
  END Close;

(****************************************************************)

PROCEDURE WriteProlog (o: T) RAISES {Wr.Failure} =
  VAR headY := o.physPageHeight - o.topMargin - 36.0;

  PROCEDURE WriteOneFamily (element: REFANY) RAISES {Wr.Failure} =
    VAR family := NARROW(element, TEXT);
    BEGIN
      Wr.PutText(o.wr, " " & family);
    END WriteOneFamily;
  PROCEDURE WriteFontSetup (element: REFANY) RAISES {Wr.Failure} =
    VAR font := NARROW(element, Font);
    BEGIN
      Wr.PutText(o.wr, Fmt.F("/%s /%s findfont %s scalefont def\n",
                             font.psName, font.family, Fmt.Real(font.scale)));
    END WriteFontSetup;

  BEGIN
    Wr.PutText(o.wr, "%!PS-Adobe-3.0\n" (* now conforming *)
                       & "%%Pages: (atend)\n");
    IF o.landscape THEN
      Wr.PutText(o.wr, "%%Orientation: Landscape\n");
    ELSE
      Wr.PutText(o.wr, "%%Orientation: Portrait\n");
    END;
    Wr.PutText(o.wr, "%%DocumentNeededResources: font");
    VAR x := o.allFontFamilies; BEGIN
      WHILE (x # NIL) DO WriteOneFamily (x.head); x := x.tail; END;
    END;
    Wr.PutText(o.wr, "\n");
    Wr.PutText(o.wr, "%%EndComments\n");
    Wr.PutText(
      o.wr, "%%BeginSetup\n" & "/T { moveto show } bind def\n"
              & "/Box { moveto dup 0 exch rlineto\n"
              & " exch 0 rlineto neg 0 exch rlineto closepath\n"
              & "} bind def\n" & "/Center { dup stringwidth pop neg 2 div\n"
              & " 4 -1 roll add 3 -1 roll moveto show\n" & "} bind def\n"
              & "/StartPage {\n");
    IF o.landscape THEN
      Wr.PutText(
        o.wr, Fmt.F(" %s 0 translate 90 rotate\n", Fmt.Real(o.physPageHeight)));
    END;
    Wr.PutText(
      o.wr, Fmt.F(" 72 36 %s %s Box 0.8 setgray fill\n",
                  Fmt.Real(o.leftMargin), Fmt.Real(headY))
              & Fmt.F(" 72 36 %s %s Box 0.8 setgray fill\n",
                      Fmt.Real(o.physPageWidth - 72.0 - o.rightMargin),
                      Fmt.Real(headY))
              & Fmt.F(" %s 20 %s %s Box 0.9 setgray fill\n",
                      Fmt.Real(o.physPageWidth - 2.0 * 72.0 - o.rightMargin
                                 - o.leftMargin),
                      Fmt.Real(o.leftMargin + 72.0), Fmt.Real(headY))
              & " 0 setgray\n" & "TitleFont setfont\n"
              & Fmt.F(" %s %s (", Fmt.Real(o.physPageWidth / 2.0),
                      Fmt.Real(headY + 6.0)));
    IF (o.title # NIL) THEN
      FOR i := 0 TO Text.Length(o.title) - 1 DO
        QuoteChar(o, Text.GetChar(o.title, i));
      END;
    END;
    Wr.PutText(o.wr, ") Center\n" & "CommentFont setfont\n"
                       & Fmt.F(" %s %s (", Fmt.Real(o.leftMargin + 36.0),
                               Fmt.Real(headY + 18.0)));
    FOR i := 0 TO Text.Length(o.comment[0]) - 1 DO
      QuoteChar(o, Text.GetChar(o.comment[0], i));
    END;
    Wr.PutText(
      o.wr, ") Center\n" & Fmt.F(" %s %s (", Fmt.Real(o.leftMargin + 36.0),
                                 Fmt.Real(headY + 6.0)));
    FOR i := 0 TO Text.Length(o.comment[1]) - 1 DO
      QuoteChar(o, Text.GetChar(o.comment[1], i));
    END;
    Wr.PutText(
      o.wr, ") Center\n" & "PageFont setfont\n"
              & Fmt.F(" %s exch %s exch Center\n",
                      Fmt.Real(o.physPageWidth - o.rightMargin - 36.0),
                      Fmt.Real(headY + 6.0))
              & Fmt.F(" %s %s translate\n", Fmt.Real(o.leftMargin),
                      Fmt.Real(o.bottomMargin)) & "} bind def\n"
              & Fmt.F("/NextColumn { %s 0 translate } bind def\n",
                      Fmt.Real(o.pageWidth + o.gully))
              & "/CommentFont /Times-Bold findfont 12 scalefont def\n"
              & "/TitleFont /Times-Roman findfont 12 scalefont def\n"
              & "/PageFont /Helvetica-Bold findfont 30 scalefont def\n");
    VAR x := o.allFonts; BEGIN
      WHILE (x # NIL) DO WriteFontSetup (x.head);  x := x.tail END;
    END;
    Wr.PutText(o.wr, "%%EndSetup\n");
    StartPage(o);
    o.prologWritten := TRUE;
  END WriteProlog;

(* Returns a fixed-width FBE.T that writes to the underlying Wr.T. *)
PROCEDURE New (wr        : Wr.T;
               title     : TEXT;
               comment   := ARRAY [0 .. 1] OF TEXT{"", ""};
               landscape := TRUE                            ): FBE.T =
  VAR
    o         := NEW(T);
    tmp: REAL;
  BEGIN
    o.wr := wr;
    o.landscape := landscape;
    o.title := title;
    o.comment := comment;
    IF landscape THEN
      tmp := o.physPageWidth;
      o.physPageWidth := o.physPageHeight;
      o.physPageHeight := tmp;
    END;
    o.pageHeight :=
      o.physPageHeight - 36.0 - o.topMargin - o.bottomMargin - o.topGully;
    IF landscape THEN
      o.pageWidth :=
        (o.physPageWidth - 10.0 - o.leftMargin - o.rightMargin) / 2.0;
    ELSE
      o.pageWidth := o.physPageWidth - o.leftMargin - o.rightMargin;
    END;
    o.curY := o.pageHeight;
    o.psFont := NIL;             (* no font yet *)
    o.nChars := 0;
    RETURN o;
  END New;

BEGIN
END FBEPostScript.
