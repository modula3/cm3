(* Last modified on Fri Apr 12 09:36:41 PDT 1996 by heydon         *)
<* PRAGMA LL *>

MODULE JunoConfig;

IMPORT JunoRsrc;
IMPORT MultiSplit, MultiClass, TextEditVBT, FileBrowserVBT;
IMPORT VBT, Split, Font, TextVBT, TextPort, ListVBT, Region, Rect, Point;
IMPORT OSError, Pathname, Process, Fmt, Rd, FileRd, Env, Thread, Lex;
IMPORT   Sx, RefSeq, RefList, Atom, TextRd, TextSeq, Text, Wr, Rsrc;
FROM Stdio IMPORT stderr;

TYPE
  FontWeight = { Any, Medium, Bold };

CONST
  DefaultConfigName = "DefaultConfig.sx";
  (* Name of default configuration file looked up on "JunoRsrc.Path" *)

PROCEDURE SetDefaults() =
  <* FATAL Wr.Failure, Thread.Alerted *>
  VAR rd: Rd.T; BEGIN
    TRY
      rd := Rsrc.Open(DefaultConfigName, JunoRsrc.Path);
      ParseConfigFile(rd)
    EXCEPT
      Rsrc.NotFound =>
        Wr.PutText(stderr, "Error: unable to open default "
          & "configuration file \"" & DefaultConfigName & "\"\n");
        Process.Exit(1);
    | Error (msg) =>
        Wr.PutText(stderr, "Error reading default configuration file:\n");
        Wr.PutText(stderr, "  " & msg & "\n");
        Process.Exit(1);
    END
  END SetDefaults;

PROCEDURE TryOpening(nm: Pathname.T; VAR (*INOUT*) rd: Rd.T): BOOLEAN =
(* Try opening the file named "nm" as a regular file. If it exists and is a
   regular file, then set "rd" to be a reader on the file and return TRUE.
   Otherwise, leave "rd" unchanged and return FALSE. *)
  BEGIN
    TRY rd := FileRd.Open(nm) EXCEPT
      OSError.E => RETURN FALSE
    END;
    RETURN TRUE
  END TryOpening;

PROCEDURE Init(filename: Pathname.T := NIL): Pathname.T
    RAISES {OSError.E, Error} =
  CONST DefaultName = ".juno-config.sx";
  VAR rd: Rd.T := NIL; res: Pathname.T := NIL; BEGIN
    SetDefaults();
    IF filename = NIL THEN
      IF TryOpening(DefaultName, (*OUT*) rd) THEN
        res := DefaultName
      ELSE
        (* try looking in the home directory *)
        VAR homeDir := Env.Get("HOME"); BEGIN
          IF homeDir # NIL THEN
            VAR fname := homeDir & "/" & DefaultName; BEGIN
              IF TryOpening(fname, (*OUT*) rd) THEN res := fname END
            END
          END
        END
      END
    ELSE
      rd := FileRd.Open(filename);
      res := filename;
    END;
    IF rd # NIL THEN ParseConfigFile(rd) END;
    RETURN res
  END Init;

PROCEDURE ParseConfigFile(rd: Rd.T) RAISES {Error} =
  BEGIN
    TRY ParseSx(ReadLists(rd)) EXCEPT
      Sx.ReadError (msg) => RAISE Error(msg)
    | Rd.EndOfFile => RAISE Error("encountered premature end-of-file")
    | Rd.Failure => RAISE Error("failure reading file")
    END
  END ParseConfigFile;

PROCEDURE ReadLists(rd: Rd.T): RefSeq.T
  RAISES {Sx.ReadError, Rd.EndOfFile, Rd.Failure} =
(* The file "rd" should contain a sequence of symbolic expressions. This
   returns a sequence of "Sx.T" values in read from "rd". *)
  <* FATAL Thread.Alerted *>
  VAR res := NEW(RefSeq.T).init(); BEGIN
    LOOP
      Lex.Skip(rd);
      IF Rd.EOF(rd) THEN EXIT END;
      res.addhi(Sx.Read(rd))
    END;
    RETURN res
  END ReadLists;

PROCEDURE ParseSx(refs: RefSeq.T) RAISES {Error} =
  BEGIN
    WHILE refs.size() > 0 DO
      TYPECASE refs.remlo() OF RefList.T (l) => ParseList(l)
      ELSE RAISE Error("top-level element is not a list")
      END
    END
  END ParseSx;

VAR (*CONST*)
  CodeFontSym 	:= Atom.FromText("CodeFont");
  TextFontSym 	:= Atom.FromText("TextFont");
  LabelFontSym  := Atom.FromText("LabelFont");
  DotSizeSym    := Atom.FromText("DotSize");
  CrossSizeSym  := Atom.FromText("CrossSize");
  ChkptIntvSym  := Atom.FromText("ChkptIntv");
  RealPrecSym   := Atom.FromText("RealPrec");
  PreviewCmdSym := Atom.FromText("PreviewCmd");
  PrintCmdSym   := Atom.FromText("PrintCmd");
  OriginSym     := Atom.FromText("Origin");
  OrientSym     := Atom.FromText("Orientation");

PROCEDURE ParseList(l: RefList.T) RAISES {Error} =
  BEGIN
    IF l = NIL THEN RAISE Error("top-level list is empty") END;
    TYPECASE l.head OF Atom.T (cmd) =>
      IF    cmd = CodeFontSym  	THEN codeFont  	 := ParseFont(l.tail, cmd)
      ELSIF cmd = TextFontSym  	THEN textFont  	 := ParseFont(l.tail, cmd)
      ELSIF cmd = LabelFontSym 	THEN labelFont 	 := ParseFont(l.tail, cmd)
      ELSIF cmd = DotSizeSym   	THEN SetDot(ParseReal(l.tail, cmd))
      ELSIF cmd = CrossSizeSym 	THEN SetCross(ParseCard(l.tail, cmd))
      ELSIF cmd = ChkptIntvSym 	THEN chkptIntv 	 := ParseCard(l.tail, cmd)
      ELSIF cmd = RealPrecSym 	THEN realPrec 	 := ParseCard(l.tail, cmd) - 1
      ELSIF cmd = PreviewCmdSym THEN previewCmd  := ParseText(l.tail, cmd)
      ELSIF cmd = PrintCmdSym   THEN printCmd    := ParseText(l.tail, cmd)
      ELSIF cmd = OriginSym     THEN origin      := ParseOrig(l.tail, cmd)
      ELSIF cmd = OrientSym     THEN orientation := ParseOrient(l.tail, cmd)
      END
    ELSE
      RAISE Error("first element of list is not an identifier")
    END
  END ParseList;

PROCEDURE Circle(r: REAL): Region.T =
  VAR res := Region.Empty; lo := FLOOR(-r); hi := CEILING(r); BEGIN
    FOR h := lo TO hi DO
      FOR v := lo TO hi DO
        IF h * h + v * v <= FLOOR(r * r) THEN
          WITH rect = Rect.FromPoint(Point.T{h, v}) DO
            res := Region.JoinRect(rect, res)
          END
        END
      END
    END;
    RETURN res
  END Circle;

PROCEDURE SetDot(sz: REAL) =
  BEGIN dot := Circle(sz) END SetDot;

PROCEDURE Cross(rad: CARDINAL): Region.T =
  VAR
    width := MAX(1, ROUND(FLOAT(rad) / 4.0));
    lower := - FLOOR(FLOAT(width) / 2.0);
    upper := CEILING(FLOAT(width) / 2.0);
    extra := width MOD 2; (* make cross symmetric for odd widths *)
  BEGIN
    RETURN Region.Join(
      Region.FromRect(
        Rect.T{west := -rad, east := rad + extra,
               north := lower, south := upper}),
      Region.FromRect(
        Rect.T{west := lower, east := upper,
               north := -rad, south := rad + extra}))
  END Cross;

PROCEDURE SetCross(sz: CARDINAL) =
  BEGIN
    cross := Cross(sz);
    crossBdry := Region.Inset(cross, -1)
  END SetCross;

PROCEDURE CheckLen(l: RefList.T; nm: Atom.T; cnt: CARDINAL) RAISES {Error} =
(* Check that the list "l" has length "cnt"; if not, raise "Error" with an
   error message that refers to "nm" *)
  BEGIN
    IF RefList.Length(l) # cnt THEN
      RAISE Error("\"" & Atom.ToText(nm) &
        "\" decl has wrong number of args")
    END
  END CheckLen;

PROCEDURE ParseText(l: RefList.T; nm: Atom.T): TEXT RAISES {Error} =
  BEGIN
    CheckLen(l, nm, 1);
    TYPECASE l.head OF
      TEXT (t) => RETURN t
    | Atom.T (a) => RETURN Atom.ToText(a)
    ELSE
      RAISE Error("expected arg to \"" & Atom.ToText(nm)
        & "\" to be a text or identifier")
    END
  END ParseText;

PROCEDURE ParseCard(l: RefList.T; nm: Atom.T): CARDINAL RAISES {Error} =
  BEGIN
    CheckLen(l, nm, 1);
    TYPECASE l.head OF REF INTEGER (ri) =>
      IF ri^ >= 0 THEN RETURN ri^ END
    ELSE (* SKIP *)
    END;
    RAISE Error("expected arg to \"" & Atom.ToText(nm)
      & "\" to be a cardinal")
  END ParseCard;

PROCEDURE ParseReal(l: RefList.T; nm: Atom.T): REAL RAISES {Error} =
  BEGIN
    CheckLen(l, nm, 1);
    TYPECASE l.head OF REF REAL (rr) => RETURN rr^
    ELSE
      RAISE Error("expected arg to \"" & Atom.ToText(nm)
        & "\" to be a real")
    END
  END ParseReal;

PROCEDURE ParseAtom(l: RefList.T; nm: Atom.T): Atom.T RAISES {Error} =
  BEGIN
    CheckLen(l, nm, 1);
    TYPECASE l.head OF
      Atom.T (a) => RETURN a
    ELSE
      RAISE Error("expected arg to \"" & Atom.ToText(nm)
        & "\" to be an identifier")
    END
  END ParseAtom;

VAR (*CONST*)
  CenterSym     := Atom.FromText("center");
  SouthWestSym  := Atom.FromText("southwest");

PROCEDURE ParseOrig(l: RefList.T; nm: Atom.T): Origin RAISES {Error} =
  VAR val := ParseAtom(l, nm); BEGIN
    IF val = CenterSym THEN RETURN Origin.Center
    ELSIF val = SouthWestSym THEN RETURN Origin.SW
    ELSE
      RAISE Error("\"" & Atom.ToText(val) & "\" is not a legal value for \""
        & Atom.ToText(nm) & "\"")
    END
  END ParseOrig;

VAR (*CONST*)
  PortraitSym   := Atom.FromText("portrait");
  LandscapeSym  := Atom.FromText("landscape");

PROCEDURE ParseOrient(l: RefList.T; nm: Atom.T): Orientation RAISES {Error} =
  VAR val := ParseAtom(l, nm); BEGIN
    IF val = PortraitSym THEN RETURN Orientation.Portrait
    ELSIF val = LandscapeSym THEN RETURN Orientation.Landscape
    ELSE
      RAISE Error("\"" & Atom.ToText(val) & "\" is not a legal value for \""
        & Atom.ToText(nm) & "\"")
    END
  END ParseOrient;

VAR (*CONST*)
  MediumSym     := Atom.FromText("medium");
  BoldSym       := Atom.FromText("bold");

PROCEDURE ParseFont(l: RefList.T; nm: Atom.T): Font.T RAISES {Error} =
  VAR fonts := NEW(REF ARRAY OF TEXT, RefList.Length(l)); i := 0; BEGIN
    WHILE l # NIL DO
      TYPECASE l.head OF
        TEXT (xfont) => fonts[i] := xfont
      | RefList.T (f) => fonts[i] := ParseLFontSpec(f, nm);
      ELSE RAISE Error("invalid font specification for " & Atom.ToText(nm))
      END;
      l := l.tail;
      INC(i)
    END;
    RETURN Font.FromName(fonts^)
  END ParseFont;

PROCEDURE XFontName(family: TEXT; weight: FontWeight; sz: CARDINAL): TEXT =
  CONST Pre = "-*-"; Mid = "-r-*--*-"; Post = "0-*-*-*-*-*-*";
  CONST Wt = ARRAY FontWeight OF TEXT{"*", "medium", "bold"};
  BEGIN
    RETURN Pre & family & "-" & Wt[weight] & Mid & Fmt.Int(sz) & Post
  END XFontName;

PROCEDURE ParseLFontSpec(l: RefList.T; nm: Atom.T): TEXT RAISES {Error} =
(* Lookup the font specified by "l", which should be a list of the form
   "( FontName FontWeight FontSize )". Return the X font name corresponding to
   the font. *)
  VAR fontNm: TEXT; fontWt: FontWeight; sz: CARDINAL; BEGIN
    CheckLen(l, nm, 3);
    TYPECASE l.head OF
      Atom.T (a) => fontNm := Atom.ToText(a)
    | TEXT (t) => fontNm := t
    ELSE
      RAISE Error("font name for \"" & Atom.ToText(nm)
        & "\" not a text or identifier");
    END;
    l := l.tail;
    TYPECASE l.head OF Atom.T (a) =>
      IF a = MediumSym  THEN fontWt := FontWeight.Medium
      ELSIF a = BoldSym THEN fontWt := FontWeight.Bold
      ELSE RAISE Error("font weight \"" & Atom.ToText(a) & "\" for \""
        & Atom.ToText(nm) & "\" is not a legal value")
      END
    ELSE
      RAISE Error("font weight for \"" & Atom.ToText(nm)
        & "\" not an identifier");
    END;
    sz := ParseCard(l.tail, nm);
    RETURN XFontName(fontNm, fontWt, sz)
  END ParseLFontSpec;

PROCEDURE FindString(t, s: TEXT): INTEGER =
(* Return the index of the first occurrence in "t" of "s", or -1 if "s" does
   not occur within "t". *)
  VAR startChar := Text.GetChar(s, 0); sLen := Text.Length(s); i := 0; BEGIN
    LOOP
      i := Text.FindChar(t, startChar, i);
      IF i < 0 OR Text.Equal(s, Text.Sub(t, i, sLen)) THEN EXIT END;
      INC(i); (* otherwise increment for next time 'round *)
    END;
    RETURN i
  END FindString;

PROCEDURE SubstForVar(VAR (*INOUT*) arg: TEXT; varName, val: TEXT): TEXT
  RAISES {Error} =
(* Return the result of replacing the (first) occurence (if any) of "varName"
   in "arg" by "val". If such an occurrence is found but "val" is NIL, raise
   "Error". *)
  VAR i := FindString(arg, varName); res := arg; BEGIN
    IF i >= 0 THEN
      IF val # NIL THEN
        res := Text.Sub(arg, 0, i) & val &
          Text.Sub(arg, i + Text.Length(varName))
      ELSE
        RAISE Error("variable \"" & varName
          & "\" not allowed in this command")
      END
    END;
    RETURN res
  END SubstForVar;

PROCEDURE ParseCmd(cmdLine: TEXT; VAR (*OUT*) cmd: TEXT;
    VAR (*OUT*) args: REF ARRAY OF TEXT;
    titleVal, displayVal, filenameVal: TEXT := NIL)
    RAISES {Error} =
  <* FATAL Rd.Failure, Thread.Alerted *> (* TextRd.T's cannot fail *)
  VAR rd := TextRd.New(cmdLine); seq := NEW(TextSeq.T).init(); BEGIN
    IF Rd.EOF(rd) THEN RAISE Error("no command specified") END;
    cmd := Lex.Scan(rd); Lex.Skip(rd);
    WHILE NOT Rd.EOF(rd) DO
      VAR arg := Lex.Scan(rd); BEGIN
        arg := SubstForVar(arg, "$Title",    titleVal);
        arg := SubstForVar(arg, "$Display",  displayVal);
        arg := SubstForVar(arg, "$Filename", filenameVal);
        seq.addhi(arg);
      END;
      Lex.Skip(rd)
    END;
    args := NEW(REF ARRAY OF TEXT, seq.size());
    FOR i := 0 TO LAST(args^) DO
      args[i] := seq.remlo()
    END
  END ParseCmd;

PROCEDURE SetFonts(v: VBT.T) =
  <* LL.sup = VBT.mu *>
  BEGIN
    TYPECASE v OF
      TextEditVBT.T (t) =>
        t.tp.setFont(codeFont)
    | TextVBT.T (t) =>
        TextVBT.SetFont(t, textFont, TextVBT.GetQuad(t))
    | TextPort.T (t) =>
        t.setFont(textFont)
    | FileBrowserVBT.DirMenu (t) =>
        t.setFont(textFont)
    | ListVBT.T (t) =>
        SetSplitFonts(t);
        TYPECASE t.painter OF
          NULL => (* SKIP *)
        | ListVBT.TextPainter (p) => p.setFont(t, textFont)
        ELSE (* SKIP *)
        END
    | Split.T (t) =>
        SetSplitFonts(t)
    ELSE
        IF MultiClass.Resolve(v) # NIL THEN
          SetSplitFonts(v)
        END
    END
  END SetFonts;

PROCEDURE SetSplitFonts(v: VBT.T) =
  <* LL.sup = VBT.mu *>
  <* FATAL MultiSplit.NotAChild *>
  VAR curr := MultiSplit.Succ(v, NIL); BEGIN
    WHILE curr # NIL DO
	SetFonts(curr);
	curr := MultiSplit.Succ(v, curr)
    END
  END SetSplitFonts;

BEGIN
END JunoConfig.
