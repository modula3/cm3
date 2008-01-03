(* Copyright (C) 1989, 1992, Digital Equipment Corporation                   *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri May 26 08:44:29 PDT 1995 by kalsow                   *)
(*      modified on Tue Jun 23 10:12:17 PDT 1992 by schilit@xerox.com        *)
(*      modified on Wed Mar  4 19:35:46 PST 1992 by muller                   *)
(*      modified on Mon Oct 14 14:54:30 PDT 1991 by meehan                   *)
(*      modified on Wed Apr 24 11:43:17 1991 by nichols@xerox.com            *)

(* Main program for the Modula-3 pretty-printer *)

MODULE Main;

IMPORT Rd, Wr, Text, Thread, Process, Env, FS, FmtTime;
IMPORT Lex, FileRd, NewFormatter AS Formatter, Parse, Params;
IMPORT FloatMode, OSError, TextRd, Date, Fmt;
IMPORT FBE, FBEPostScript, FBEWr;
FROM Stdio IMPORT stdout, stderr;

<* FATAL Thread.Alerted, Wr.Failure *>

TYPE
  Argv = REF ARRAY [0 .. 255] OF TEXT;
  BackEndType = {Wr, PostScript};

VAR
  argv: Argv;                   (* parameters we're currently parsing *)
  argc: INTEGER;                (* count of them *)
  argvSource: TEXT;             (* where this argv came from *)
  program   : TEXT;             (* name of this program *)

  formatOptions: Parse.Options;

  emacs    : BOOLEAN          := FALSE;
  emacsLoop: BOOLEAN          := FALSE;
  output   : Formatter.T;
  infile   : Text.T           := NIL;

  backEnd                            := BackEndType.Wr;
  margin:      INTEGER               := 75;
  landscape                          := TRUE;

  (* These are used if set to non-zero value by arg parser. *)
  offset:           INTEGER := 0;
  commentColumn:    INTEGER := 0;
  bodyFont:         TEXT    := NIL;
  keywordFont:      TEXT    := NIL;
  builtinIDFont:    TEXT    := NIL;
  procNameFont:     TEXT    := NIL;
  commentFont:      TEXT    := NIL;
  fixedCommentFont: TEXT    := NIL;
  fixedFont:        TEXT    := NIL;


PROCEDURE Usage () =
  BEGIN
    Wr.PutText (
      stderr,
	"This program reformats a Modula-3 interface or module. "
	& "It can produce\n"
        & "either text or PostScript output.\n"		
        & "Usage: " & program & " [options] [file]\n"
        & "Options (first letter of each is enough):\n"
        & " -caps      -- allow lower-case keywords\n"
        & " -margin n  -- set right margin to n\n"
        & " -offset n  -- set indentation offset to n\n"
        & " -xcolumn n -- set comment column to n\n"
        & " -src       -- use SRC-style formatting\n"
        & " -eric      -- use Muller-style formatting\n"
        & " -callspace -- write f (x, y) instead of f(x, y)\n"
        & " -noalign   -- don\'t align declarations in columns\n"
        & " -follow    -- format comments assuming they follow decls\n"
        & " -break     -- same as '-whenbreak always'\n"
        & " -whenbreak {always | early | late}\n"
        & "            -- when to break long lines\n"
	& " -text      -- select text output (default)\n"
        & "These are PostScript-related options.  Except for -portrait,\n"
        & "they cannot be abbreviated.\n"
        & " -ps        -- select PostScript output\n"
        & " -portrait  -- print one-column portrait mode.\n"
        & "               default is two-column landscape\n"
        & " -bf font   -- font for body text (default: Times-Roman10)\n"
        & " -kf font   -- font for keywords (Helvetica7)\n"
        & " -bif font  -- font for built-in identifiers (Times-Roman8)\n"
        & " -pf font   -- font for procedure names (Times-Bold10)\n"
        & " -cf font   -- font for comments (Times-Italic10)\n"
        & " -fcf font  -- font for fixed-format comments (Courier-Oblique9)\n"
        & " -ff font   -- font for text and char literals (Courier9)\n");
  END Usage;

PROCEDURE GetNextInt (VAR i: INTEGER): INTEGER
  RAISES {Lex.Error, Thread.Alerted} =
  BEGIN
    INC (i);
    IF i >= argc THEN RAISE Lex.Error END;
    TRY
      RETURN Lex.Int (TextRd.New (argv[i]));
    EXCEPT FloatMode.Trap, Rd.Failure =>
      RAISE Lex.Error;
    END;
  END GetNextInt;

PROCEDURE GetNextString (VAR i: INTEGER): TEXT =
  BEGIN
    INC(i);
    RETURN argv[i];
  END GetNextString;

PROCEDURE GetParameters () =
  VAR
    i: INTEGER;
    s: TEXT;
  BEGIN
    i := 0;
    TRY
      WHILE i < argc DO
        s := argv [i];
	IF Text.Equal(s, "-t") OR Text.Equal(s, "-text") THEN
	  backEnd := BackEndType.Wr;
	ELSIF Text.Equal(s, "-ps") OR Text.Equal(s, "-postscript") THEN
	   backEnd := BackEndType.PostScript; 
        ELSIF Text.Equal (s, "-c") OR Text.Equal (s, "-caps") THEN
          formatOptions.lowerCase := TRUE
        ELSIF Text.Equal (s, "-m") OR Text.Equal (s, "-margin") THEN
          margin := GetNextInt (i);
        ELSIF Text.Equal (s, "-o") OR Text.Equal (s, "-offset") THEN
          offset := GetNextInt (i);
        ELSIF Text.Equal (s, "-x") OR Text.Equal (s, "-xcolumn") THEN
          commentColumn := GetNextInt (i);
        ELSIF Text.Equal (s, "-ZZ") OR Text.Equal (s, "-filter") THEN
          emacs := TRUE;
          emacsLoop := TRUE;
        ELSIF Text.Equal (s, "-ZZG") THEN
          emacs := TRUE;
        ELSIF Text.Equal (s, "-s") OR Text.Equal (s, "-src") THEN
          formatOptions.style := Parse.Style.SRC;
        ELSIF Text.Equal (s, "-eric") THEN
          formatOptions.style := Parse.Style.EMULLER;
        ELSIF Text.Equal (s, "-callspace") THEN
          formatOptions.callSpace := TRUE;
        ELSIF Text.Equal (s, "-n") OR Text.Equal (s, "-noalign") THEN
          formatOptions.alignDecls := FALSE;
        ELSIF Text.Equal (s, "-f") OR Text.Equal (s, "-follow") THEN
          formatOptions.follow (* comBreakNLs *) := TRUE;
        ELSIF Text.Equal (s, "-b") OR Text.Equal (s, "-break") THEN
          formatOptions.breakType := Formatter.BreakType.NonOptimal;
        ELSIF Text.Equal (s, "-w") OR Text.Equal (s, "-whenbreak") THEN
          INC (i);
          IF i >= argc THEN RAISE Lex.Error; END;
          WITH arg = argv [i] DO
            IF Text.Equal (arg, "always") OR Text.Equal (arg, "a") THEN
              formatOptions.breakType := Formatter.BreakType.NonOptimal;
            ELSIF Text.Equal (arg, "early") OR Text.Equal (arg, "e") THEN
              formatOptions.breakType := Formatter.BreakType.OptimalBreak;
            ELSIF Text.Equal (arg, "late") OR Text.Equal (arg, "l") THEN
              formatOptions.breakType := Formatter.BreakType.OptimalNoBreak;
            ELSE
              RAISE Lex.Error;
            END
          END
	ELSIF Text.Equal(s, "-p") OR Text.Equal(s, "-portrait") THEN
	  landscape := FALSE;
	ELSIF Text.Equal(s, "-bf") THEN
	  bodyFont := GetNextString(i);
	ELSIF Text.Equal(s, "-kf") THEN
	  keywordFont := GetNextString(i);
	ELSIF Text.Equal(s, "-bif") THEN
	  builtinIDFont := GetNextString(i);
	ELSIF Text.Equal(s, "-pf") THEN
	  procNameFont := GetNextString(i);
	ELSIF Text.Equal(s, "-cf") THEN
	  commentFont := GetNextString(i);
	ELSIF Text.Equal(s, "-fcf") THEN
	  fixedCommentFont := GetNextString(i);
	ELSIF Text.Equal(s, "-ff") THEN
	  fixedFont := GetNextString(i);
        ELSIF Text.GetChar (s, 0) = '-' THEN
          RAISE Lex.Error; (* bad switch *)
        ELSE
          infile := s;
        END;

        INC (i);
      END;
    EXCEPT
      Lex.Error =>
        Wr.PutText (stderr, "Error in parsing " & argvSource & "\n");
        Usage ();
        Process.Exit (1);
    END;
  END GetParameters;

(* Read a file for more options. We parse up the first line of text and
   call GetParameters with it. *)
PROCEDURE ReadProfile (fileName: TEXT) =
  VAR
    r          : Rd.T;
    t          : TEXT;          (* input text *)
    start, stop: INTEGER;       (* limits of an argument *)
    i, len     : INTEGER;
  BEGIN
    (* Read the first line. *)
    TRY
      r := FileRd.Open (fileName);
      t := Rd.GetLine (r);
      Rd.Close (r);
    EXCEPT
      Rd.Failure, Rd.EndOfFile, OSError.E => RETURN;
    END;
    (* Scan it, saving the arguments. *)
    i := 0;
    len := Text.Length (t);
    argc := 0;
    LOOP
      WHILE i < len AND Text.GetChar (t, i) = ' ' DO INC (i) END;
      start := i;
      IF start >= len THEN EXIT END;
      stop := Text.FindChar (t, ' ', start);
      IF stop = -1 THEN stop := len END;
      IF argc <= LAST (argv^) THEN
        argv [argc] := Text.Sub (t, start, stop - start);
      END;
      INC (argc);
      i := stop;
    END;
    (* Now process them. *)
    argvSource := fileName;
    GetParameters ();
  END ReadProfile;

PROCEDURE InitializeOptions (VAR a: Parse.Options) =
  BEGIN
    a.lowerCase := FALSE;
    a.style := Parse.Style.SRC;
    a.alignDecls := TRUE;
    a.follow := FALSE;
    a.breakType := Formatter.BreakType.OptimalBreak;
    a.callSpace := FALSE;
  END InitializeOptions;

(* Return an array of the file modification date and time *)
PROCEDURE FileTimes (infile: Text.T): ARRAY [0 .. 1] OF TEXT =
  VAR r: ARRAY [0 .. 1] OF TEXT;  date: Date.T;
  BEGIN
    r[0] := "";
    r[1] := "";
    TRY
      date := Date.FromTime (FS.Status(infile).modificationTime);
    EXCEPT OSError.E =>
      RETURN r;
    END;
    r[0] := Fmt.F ("%s %s, %s", FmtTime.Month [date.month],
                   Fmt.Int (date.day), Fmt.Int (date.year));
    r[1] := Fmt.F ("%s:%02s:%02s", Fmt.Int (date.hour),
                   Fmt.Int (date.minute), Fmt.Int (date.second));
    RETURN r;
  END FileTimes;

BEGIN
  TRY
    InitializeOptions(formatOptions);
    (* Deal with parameters. *)
    argv := NEW(Argv);
    program := Params.Get(0);
    (* Try profiles in increasing order of priority. *)
    WITH home = Env.Get ("HOME") DO
      IF (home # NIL) THEN ReadProfile(home & "/.m3pp.pro.1"); END;
    END;
    ReadProfile(".m3pp.pro.1");
    (* Now convert argv and process it. *)
    FOR i := 1 TO Params.Count - 1 DO argv[i - 1] := Params.Get(i); END;
    argc := Params.Count - 1;
    argvSource := "command line";
    GetParameters();

    CASE backEnd OF
      BackEndType.Wr =>
        output := Formatter.New(FBEWr.New(stdout, margin));
        formatOptions.bodyFont := "";
        formatOptions.keywordFont := "";
        formatOptions.builtinIDFont := "";
        formatOptions.procNameFont := "";
        formatOptions.commentFont := "";
        formatOptions.fixedFont := "";
        formatOptions.offset := 2.0;
        formatOptions.commentColumn := 33.0;
    | BackEndType.PostScript =>
        output :=
          Formatter.New(
            FBEPostScript.New(stdout, infile, FileTimes(infile), landscape));
        formatOptions.bodyFont := "Times-Roman10";
        formatOptions.keywordFont := "Helvetica7";
        formatOptions.builtinIDFont := "Times-Roman8";
        formatOptions.procNameFont := "Times-Bold10";
        formatOptions.commentFont := "Times-Italic10";
        formatOptions.fixedCommentFont := "Courier-Oblique9";
        formatOptions.fixedFont := "Courier9";
        formatOptions.offset := 4.0;
        formatOptions.commentColumn := 80.0;
    END;
    IF offset # 0 THEN formatOptions.offset := FLOAT (offset); END;
    IF commentColumn # 0 THEN
      formatOptions.commentColumn := FLOAT (commentColumn);
    END;
    IF bodyFont # NIL THEN formatOptions.bodyFont := bodyFont; END;
    IF keywordFont # NIL THEN formatOptions.keywordFont := keywordFont; END;
    IF builtinIDFont # NIL THEN
      formatOptions.builtinIDFont := builtinIDFont;
    END;
    IF procNameFont # NIL THEN formatOptions.procNameFont := procNameFont; END;
    IF commentFont # NIL THEN formatOptions.commentFont := commentFont; END;
    IF fixedCommentFont # NIL THEN
      formatOptions.fixedCommentFont := fixedCommentFont;
    END;
    IF fixedFont # NIL THEN formatOptions.fixedFont := fixedFont; END;
    Parse.Init(inputFile := infile, output := output, options := formatOptions,
               calledFromEmacs := emacs);

    TRY
      LOOP
        Parse.yyparse();
        IF (NOT emacsLoop) THEN EXIT END;
        Formatter.PutChar(output, '\001');
        Formatter.PutChar(output, '\n');
        Formatter.Flush(output);
      END;
      Formatter.Flush(output);
    FINALLY
      Formatter.Close(output);
    END;
  EXCEPT
    FBE.Failed (e) =>
      Wr.PutText(stderr, "Formatting failed -- " & e.info & ".\n");
  END;

END Main
.
