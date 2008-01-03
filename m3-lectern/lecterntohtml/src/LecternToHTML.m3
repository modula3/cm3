(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Tue Feb 13 15:30:02 PST 1996 by mcjones *)

MODULE LecternToHTML EXPORTS Main;

IMPORT ASCII, Atom, File, FileRd, FileWr, Fmt, FS, FSPosix, HTML,
  HTMLSeq, LecternDoc, OSError, Params, Pathname, Process, Rd, Stdio,
  Text, TextArraySort, TextSeq, TextWr, Thread, TimeHTML,
  TimeHTMLArraySort, TimeHTMLSeq, Wr;

CONST MaxRecent = 50; (* how many recent acquistions to display? *)

PROCEDURE CIEqual(t1, t2: TEXT): BOOLEAN =
(* "Case-insensitive" version of Text.Equal. *)
  VAR l1 := Text.Length(t1); l2 := Text.Length(t2);
  BEGIN
    IF l1 # l2 THEN RETURN FALSE END;
    FOR i := 0 TO l1 - 1 DO
      IF ASCII.Upper[Text.GetChar(t1, i)]
          # ASCII.Upper[Text.GetChar(t2, i)] THEN
        RETURN FALSE
      END
    END;
    RETURN TRUE
  END CIEqual;

PROCEDURE ToHTMLArray(s: HTMLSeq.T): REF ARRAY OF HTML.T =
(* Return a new array containing "s.getlo()", ..., "s.gethi()". *)
  VAR r := NEW(REF ARRAY OF HTML.T, s.size());
  BEGIN
    FOR i := 0 TO LAST(r^) DO
      r[i] := s.get(i)
    END;
    RETURN r
  END ToHTMLArray;

PROCEDURE ToTextArray(s: TextSeq.T): REF ARRAY OF TEXT =
(* Return a new array containing "s.getlo()", ..., "s.gethi()". *)
  VAR r := NEW(REF ARRAY OF TEXT, s.size());
  BEGIN
    FOR i := 0 TO LAST(r^) DO
      r[i] := s.get(i)
    END;
    RETURN r
  END ToTextArray;

PROCEDURE ToTimeHTMLArray(s: TimeHTMLSeq.T): REF ARRAY OF TimeHTML.T =
(* Return a new array containing "s.getlo()", ..., "s.gethi()". *)
  VAR r := NEW(REF ARRAY OF TimeHTML.T, s.size());
  BEGIN
    FOR i := 0 TO LAST(r^) DO
      r[i] := s.get(i)
    END;
    RETURN r
  END ToTimeHTMLArray;

PROCEDURE DirToSeq(
    pn: TEXT;
    VAR (*OUT*) dirs, files: TextSeq.T) RAISES {OSError.E} =
  VAR
    it := FS.Iterate(pn);
    name: TEXT;
    status: File.Status;
    a: REF ARRAY OF TEXT;
  BEGIN
    IF dirs = NIL THEN dirs := NEW(TextSeq.T).init() END;
    IF files = NIL THEN files := NEW(TextSeq.T).init() END;
    LOOP
      TRY
	IF NOT it.nextWithStatus(name, status) THEN EXIT END;
	IF status.type = FS.DirectoryFileType THEN
	  dirs.addhi(name)
	ELSE
	  files.addhi(name)
	END
      EXCEPT OSError.E => (* e.g, dangling symbolic link *)
        Wr.PutText(Stdio.stderr, pn & ": skipping bad entry\n")
      END;
    END;
    a := ToTextArray(dirs);
    TextArraySort.Sort(a^);
    dirs := dirs.fromArray(a^);
    a := ToTextArray(files);
    TextArraySort.Sort(a^);
    files := files.fromArray(a^);
  END DirToSeq;

PROCEDURE GetText(dir, pn: TEXT): TEXT RAISES {Rd.Failure} =
  VAR rd: Rd.T; t: TEXT;
  BEGIN
    TRY
      rd := FileRd.Open(Pathname.Join(dir, pn, NIL));
      TRY
        t := Rd.GetText(rd, 10000);
        WITH n = Text.Length(t) DO
          IF n > 0 AND Text.GetChar(t, n - 1) = '\n' THEN
              t := Text.Sub(t, 0, n - 1)
          END
        END;
        RETURN t
      FINALLY Rd.Close(rd)
      END
    EXCEPT OSError.E => RETURN ""
    END
  END GetText;

PROCEDURE PutText(pn, text: TEXT) RAISES {} =
(* Write "text" to file with pathname "pn". *)
  VAR wr: Wr.T;
  BEGIN
    TRY
      wr := FileWr.Open(pn);
      TRY Wr.PutText(wr, text) FINALLY Wr.Close(wr) END
    EXCEPT OSError.E(code) =>
      Wr.PutText(
	Stdio.stderr, pn & ": " & Atom.ToText(code.head) & "\n")
    END;
  END PutText;

PROCEDURE DirToHTML(
    pn: TEXT;
    backURL: TEXT;
    VAR (*OUT*) descendants: TimeHTMLSeq.T;
    VAR (*OUT*) anchor: HTML.T;
    VAR (*OUT*) pages: CARDINAL)
  RAISES {OSError.E, Rd.Failure} =
  CONST base = ARRAY BOOLEAN OF TEXT {".descending", ".ascending"};
  VAR
    sDir, sFil: TextSeq.T := NIL;
    shDir, shFil, shFilRev := NEW(HTMLSeq.T).init();
    h, hTitle, hDir: HTML.T;
    d, d1: TimeHTMLSeq.T;
    pnDoc, index, title, prefix, postfix: TEXT;
    tha: REF ARRAY OF TimeHTML.T;
    ha: REF ARRAY OF HTML.T;

    PROCEDURE PutIndex(ascending: BOOLEAN; shFil: HTMLSeq.T; backURL: TEXT) RAISES {} =
    (* Write ".ascending.html" or ".descending.html". *)
      VAR hFil: HTML.T;
      BEGIN
	IF shFil.size() # 0 THEN
	  hFil := HTML.Section(
	    level := 1,
	    heading := HTML.Text("Documents"),
	    body := HTML.Cat(
	      HTML.Paragraph(
		HTML.Anchor(
		  caption := HTML.Text(
		    Text.Sub(base[NOT ascending], 1) & " order"),
		  url :=
		    "\"" &
		    Pathname.Join("", base[NOT ascending], "html") &
		    "\""
		  )
		),
	      HTML.Paragraph(HTML.List(ToHTMLArray(shFil)^))
              )
            )
        ELSE
          hFil := HTML.Empty;
        END;
	PutText(
	  Pathname.Join(pn, base[ascending], "html"),
	  HTML.ToText(
	    HTML.Document(
	      title := hTitle,
	      body := HTML.CatArray(
		ARRAY OF HTML.T{
		  HTML.Paragraph(HTML.Text(prefix)),
		  HTML.Paragraph(
                    HTML.Anchor(
		      caption := HTML.Text("recent acquisitions"),
		      url := "\"" & ".recent.html\""
		      )
                    ),
		  hDir,
		  hFil,
                  HTML.Paragraph(
                    HTML.Anchor(
                      caption := HTML.Text("[back]"),
                      url := backURL)
                  ),
		  HTML.Paragraph(HTML.Text(postfix))
		  }
		)
	      )
	    )
	  )
      END PutIndex;

  BEGIN
    IF NOT quiet THEN Wr.PutText(Stdio.stderr, pn & "\n") END;
    index := "index.html";
    TRY
      EVAL FSPosix.LinkStatus(Pathname.Join(pn, index, NIL))
    EXCEPT OSError.E =>
      index := ".ascending.html"
    END;
    d := NEW(TimeHTMLSeq.T).init();
    DirToSeq(pn, sDir, sFil);
    WHILE sDir.size() # 0 DO
      DirToHTML(
        pn := Pathname.Join(pn, sDir.remlo(), NIL),
        backURL := "\"../" & index & "\"",
        descendants := d1,
        anchor := h,
        pages := pages
      );
      IF d1.size() > 0 THEN
        shDir.addhi(h);
        FOR i := 0 TO d1.size()-1 DO
          d.addhi(d1.get(i))
        END
      END
    END;
    WHILE sFil.size() # 0 DO
      VAR nm := sFil.remlo(); BEGIN
        IF Text.Equal(Pathname.LastExt(nm), "lect") THEN
          TRY
            pnDoc := Pathname.Join(pn, nm, NIL);
            IF NOT quiet THEN Wr.PutText(Stdio.stderr, pnDoc & "\n") END;
            h := DocToHTML(pnDoc, pages);
            shFil.addhi(h); shFilRev.addlo(h);
            d.addhi(TimeHTML.T{FS.Status(pnDoc).modificationTime, h})
          EXCEPT LecternDoc.NotLectern =>
            Wr.PutText(Stdio.stderr, pnDoc & ": not a Lectern document\n")
          END
        END
      END
    END;
    IF shDir.size() # 0 THEN
      hDir := HTML.Section(
        level := 1,
        heading := HTML.Text("Subdirectories"),
        body := HTML.List(ToHTMLArray(shDir)^))
    ELSE
      hDir := HTML.Empty
    END;

    title := GetText(pn, ".title");
    IF Text.Empty(title) THEN title := pn END;
    hTitle := HTML.Text(title);
    prefix := GetText(pn, ".prefix");
    postfix := GetText(pn, ".postfix");

    IF d.size() > 0 THEN
      tha := ToTimeHTMLArray(d);
      TimeHTMLArraySort.Sort(tha^);
      ha := NEW(REF ARRAY OF HTML.T, MIN(d.size(), MaxRecent));
      FOR i := 0 TO LAST(ha^) DO ha[i] := tha[i].html END;
      PutText(
        Pathname.Join(pn, ".recent.html", NIL),
        HTML.ToText(
          HTML.Document(
            title := HTML.Text("Recent acquisitions"),
            body := HTML.List(ha^)
            )
          )
        );
      PutIndex(TRUE, shFil, backURL); PutIndex(FALSE, shFilRev, backURL)
    END;
    descendants := d;
    anchor := HTML.Anchor(
      caption := hTitle,
      url := "\"" & Pathname.Join(Pathname.Last(pn), index, NIL) & "\""
      )
  END DirToHTML;

PROCEDURE DocToHTML(pn: TEXT; VAR (*OUT*) pages: CARDINAL): HTML.T
  RAISES {OSError.E, Rd.Failure, LecternDoc.NotLectern} =
  (* Return an .html fragment describing the Lectern document whose
     path is "pn". Increment "pages" by count of pages in document. *)
  <*FATAL Thread.Alerted*>
  VAR
    rd: Rd.T;
    dd: LecternDoc.Dir;
    title, author, date: TEXT;
    hTitle, hAuthor, hDate, hPages, hHavePostScript, hAnchor: HTML.T := NIL;
  PROCEDURE GetAttr(key: TEXT): TEXT =
    BEGIN
      FOR i := 0 TO LAST(dd.attributes^) DO
        IF CIEqual(key, dd.attributes[i].key) THEN
          RETURN dd.attributes[i].value
        END
      END;
      RETURN NIL
    END GetAttr;
  BEGIN
    rd := FileRd.Open(pn);
    TRY
      LecternDoc.ReadDir(rd, dd)
    FINALLY Rd.Close(rd)
    END;
    title := GetAttr("title");
    IF title # NIL THEN
      hTitle := HTML.Cat(HTML.Bold(HTML.Text(title)), HTML.Text(". "))
    END;
    author := GetAttr("author");
    IF author # NIL THEN hAuthor := HTML.Text(author & " ") END;
    date := GetAttr("date");
    IF date # NIL THEN hDate := HTML.Text("(" & date & ") ") END;
    hPages := HTML.Text(Fmt.Int(NUMBER(dd.pages^)) & " pp. ");
    IF dd.original.start # 0 AND dd.original.length # 0 THEN
      hHavePostScript := HTML.Text("PS")
    END;
    hAnchor := HTML.Anchor(HTML.Text(Pathname.Last(pn)), RelayURL(pn));
    INC(pages, NUMBER(dd.pages^));
    RETURN HTML.CatArray(ARRAY OF HTML.T{
      hAnchor, HTML.Text(" "), hTitle, hAuthor, hDate, hPages,
      hHavePostScript})
END DocToHTML;

PROCEDURE URLEscape(t: TEXT): TEXT =
  CONST
    HexDigit = ARRAY OF CHAR {
      '0', '1', '2', '3', '4', '5', '6', '7',
      '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
    Other = ASCII.Set{'$', '-', '_', '@', '.', '&', '/'};
  VAR wr := TextWr.New(); c: CHAR;
  BEGIN
    FOR i := 0 TO Text.Length(t) - 1 DO
      c := Text.GetChar(t, i);
      IF c IN ASCII.AlphaNumerics + Other THEN
        Wr.PutChar(wr, c)
      ELSE
        Wr.PutChar(wr, '%');
        Wr.PutChar(wr, HexDigit[ORD(c) DIV 16]);
        Wr.PutChar(wr, HexDigit[ORD(c) MOD 16])
      END;
    END;
    RETURN TextWr.ToText(wr)
  END URLEscape;

CONST URLOfRelay = "http://src-www.pa.dec.com/SRC/virtualpaper/cgi-bin/relay?";

PROCEDURE RelayURL(pn: TEXT): TEXT =
  BEGIN
    RETURN URLOfRelay & URLEscape(pn)
  END RelayURL;

VAR
  k: INTEGER;
  cmd, pn: TEXT;
  quiet := FALSE;

EXCEPTION Syntax;
<* FATAL Wr.Failure, Thread.Alerted *>
VAR d: TimeHTMLSeq.T; h: HTML.T; pages: CARDINAL;
BEGIN
  TRY
    IF Params.Count < 2 THEN RAISE Syntax END;
    cmd := Pathname.Last(Params.Get(0));
    IF Text.Equal(Params.Get(1), "-quiet") THEN
      IF Params.Count < 3 THEN RAISE Syntax END;
      quiet := TRUE;
      k := 2
    ELSE
      IF Text.GetChar(Params.Get(1), 0) = '-' THEN RAISE Syntax END;
      k := 1
    END;
    pn := Params.Get(k);
    pages := 0;
    DirToHTML(pn, "\"../\"", d, h, pages);
    IF NOT quiet THEN
      Wr.PutText(Stdio.stderr, Fmt.Int(d.size()) & " documents, " &
                               Fmt.Int(pages) & " total pages\n")
    END
  EXCEPT
  | Syntax =>
      Wr.PutText(
        Stdio.stderr,
        "usage: LecternToHTML [-quiet] path\n");
      Process.Exit(1)
  | OSError.E(code) =>
      Wr.PutText(
	Stdio.stderr,
	cmd & ": file error " & Atom.ToText(code.head) & "\n");
      Process.Exit(1)
  | Rd.Failure(code) =>
      Wr.PutText(
	Stdio.stderr,
	cmd & ": reader error " & Atom.ToText(code.head) & "\n");
      Process.Exit(1)
  END
END LecternToHTML.
