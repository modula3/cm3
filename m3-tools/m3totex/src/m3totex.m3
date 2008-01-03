(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun 15 07:39:36 PDT 1994 by kalsow                   *)
(*      modified on Mon May  3 12:23:44 PDT 1993 by mcjones                  *)
(*      modified on Tue Feb  9 12:30:09 1993 by gnelson                      *)
(*      modified on Thu Jan  7 11:08:45 PST 1993 by muller                   *)

(* Implementation of the m3totex command; see its manpage for details. *)

MODULE m3totex EXPORTS Main;

IMPORT Stdio, Rd, Wr, Params, FileRd, FileWr, Text, Thread, B, Bundle, OSError;

<*FATAL Wr.Failure, Rd.Failure, Thread.Alerted, OSError.E*>

VAR bundle := B.Get ();

PROCEDURE Trans(rd: Rd.T; wr: Wr.T; raw: BOOLEAN) =
  BEGIN
    TRY
      AdvanceToBlankLine(rd);
      Wr.PutText(wr, "{");
      IF NOT raw THEN Wr.PutText(wr, Bundle.Get(bundle, "header")) END;
      LOOP Prog(rd, wr); Comment(rd, wr) END
    EXCEPT
      Rd.EndOfFile => Wr.PutText(wr, "\n}\n")
    END 
  END Trans;

PROCEDURE AdvanceToBlankLine(rd: Rd.T) =
  VAR blank: BOOLEAN; c: CHAR;
  BEGIN
    TRY
      REPEAT
        blank := TRUE;
        LOOP
          c := Rd.GetChar(rd);
          IF c = '\n' THEN EXIT END;
          IF c # ' ' THEN blank := FALSE END
        END
      UNTIL blank
    EXCEPT
      Rd.EndOfFile => (*skip*)
    END
  END AdvanceToBlankLine;

PROCEDURE Prog(rd: Rd.T; wr: Wr.T) RAISES {Rd.EndOfFile} =
  VAR c: CHAR;  vspace := 1; hspace := 0; empty := TRUE; startOfLine := TRUE;
  PROCEDURE Space(wr: Wr.T) = BEGIN
    IF empty THEN 
      Wr.PutText(wr, "\\par\\medskip "); 
      empty := FALSE
    END;
    startOfLine := FALSE;
    IF vspace = 1 THEN
      Wr.PutText(wr, "\\par\n\\tab ") 
    ELSIF vspace > 1 THEN
      Wr.PutText(wr, "\\par\\medskip\n\\tab ")
    END;
    vspace := 0;
    WHILE hspace > 0 DO Wr.PutText(wr, "~"); DEC(hspace) END
  END Space;
  BEGIN
    Wr.PutText(wr, "\\par{\\tt\\parskip=0pt\\parindent=0pt\\progmode\n");
    TRY
      LOOP
        c := Rd.GetChar(rd);
        CASE c OF
          '\n' => INC(vspace); hspace := 0; startOfLine := TRUE |
          ' '  => INC(hspace) |
          '%'  => Space(wr); Wr.PutText(wr, "\\char\'045{}") |
          '_'  => Space(wr); Wr.PutText(wr, "\\char\'137{}") |
          '&'  => Space(wr); Wr.PutText(wr, "\\char\'046{}") |
          '$'  => Space(wr); Wr.PutText(wr, "\\char\'044{}") |
          '^'  => Space(wr); Wr.PutText(wr, "\\char\'136{}") |
          '#'  => Space(wr); Wr.PutText(wr, "\\char\'043{}") |
          '{'  => Space(wr); Wr.PutText(wr, "\\char\'173{}") |
          '}'  => Space(wr); Wr.PutText(wr, "\\char\'175{}") |
          '~'  => Space(wr); Wr.PutText(wr, "\\char\'176{}") |
          '*'  => Space(wr); Wr.PutText(wr, "\\char\'052{}") |
          '\\' => Space(wr); Wr.PutText(wr, "\\char\'134{}") |
          '(' =>
            WITH d = Rd.GetChar(rd) DO
              IF d = '*' AND startOfLine AND hspace = 0 THEN
                RETURN
              ELSE
                Rd.UnGetChar(rd);
                Space(wr);
                Wr.PutChar(wr, c)
              END
            END 
        ELSE
          Space(wr);
          Wr.PutChar(wr, c)
        END
      END
    FINALLY
      IF (vspace < 2) AND NOT empty AND NOT Rd.EOF(rd) THEN
        Wr.PutText(wr, "}\\par\\smallskip\n");
        Wr.PutText(wr, "\\procspec ")
      ELSE
        Wr.PutText(wr, "}\\par\\medskip\\noindent\n")
      END 
    END
  END Prog;

PROCEDURE QuoteChar(wr: Wr.T; ch: CHAR) =
  BEGIN
    CASE ch OF <*NOWARN*>
      '%'  => Wr.PutText(wr, "\\char\'045{}") |
      '_'  => Wr.PutText(wr, "\\char\'137{}") |
      '&'  => Wr.PutText(wr, "\\char\'046{}") |
      '$'  => Wr.PutText(wr, "\\char\'044{}") |
      '^'  => Wr.PutText(wr, "\\char\'136{}") |
      '#'  => Wr.PutText(wr, "\\char\'043{}") |
      '{'  => Wr.PutText(wr, "\\char\'173{}") |
      '}'  => Wr.PutText(wr, "\\char\'175{}") |
      '*'  => Wr.PutText(wr, "\\char\'052{}") |
      '\\' => Wr.PutText(wr, "\\char\'134{}") 
    END 
  END QuoteChar;

PROCEDURE Comment(rd: Rd.T; wr: Wr.T) RAISES {Rd.EndOfFile} =
  VAR c: CHAR; startOfLine := TRUE; afterDisplay := FALSE; dblquoteparity := 0;
  BEGIN
    Wr.PutText(wr, "{\\rm ");
    LOOP
      c := Rd.GetChar(rd);
      CASE c OF
        '\"' =>
          IF dblquoteparity = 0 THEN
            Wr.PutText(wr, "{\\tt ")
          ELSE
            Wr.PutText(wr, "}")
          END;
          dblquoteparity := (dblquoteparity + 1) MOD 2 |
        '%', '_', '&', '$', '^', '#', '{', '}'  => 
          IF dblquoteparity = 1 THEN
            QuoteChar(wr, c)
          ELSE
            Wr.PutChar(wr, c) 
          END |
        '|' =>
          IF startOfLine THEN 
            IF NOT afterDisplay THEN
              Wr.PutText(wr, "\\par\\medskip ")
            END;
            c := Rd.GetChar(rd);
            IF c # ' ' THEN Rd.UnGetChar(rd) END;
            Display(rd, wr);
            c := '\n';
            afterDisplay := TRUE
          ELSE 
            Wr.PutText(wr, "|")
          END |
        '\n' => 
          IF afterDisplay THEN 
            Wr.PutText(wr, "\\medskip\\noindent%\n");
            afterDisplay := FALSE
          ELSIF startOfLine THEN
            Wr.PutText(wr, "\\par\n");
          ELSE
            Wr.PutText(wr, "\n")
          END |
        '*' =>
          WITH d = Rd.GetChar(rd) DO
            IF d = ')' THEN 
              Wr.PutText(wr, "\\par}");
              RETURN
            ELSE
              Rd.UnGetChar(rd);
              IF afterDisplay THEN 
                Wr.PutText(wr, "\\medskip\\noindent%\n");
                afterDisplay := FALSE
              END;
              Wr.PutChar(wr, c)
            END
          END 
      ELSE
        IF afterDisplay AND c # ' ' THEN 
          Wr.PutText(wr, "\\medskip\\noindent%\n");
          afterDisplay := FALSE
        END;
        Wr.PutChar(wr, c)
      END;
      startOfLine := (c = '\n') OR (startOfLine AND c = ' ')
    END
  END Comment;

PROCEDURE Display(rd: Rd.T; wr: Wr.T) RAISES {Rd.EndOfFile} =
  VAR c: CHAR; 
  BEGIN
    Wr.PutText(wr, "{\\display ");
    LOOP
      c := Rd.GetChar(rd);
      CASE c OF
        '%', '_', '&', '$', '^', '#', '{', '}', '*', '\\'  => 
          QuoteChar(wr, c) |
        '\n' => Wr.PutText(wr, "}\\noindent\\par\n"); RETURN |
        ' ' => Wr.PutText(wr, "~") |
        '`' => Undisplay(rd, wr)
      ELSE
        Wr.PutChar(wr, c)
      END
    END
  END Display;
  
PROCEDURE Undisplay(rd: Rd.T; wr: Wr.T) RAISES {Rd.EndOfFile} =
  VAR c: CHAR; dblquoteparity := 0;
  BEGIN
    Wr.PutText(wr, "{\\rm ");
    LOOP
      c := Rd.GetChar(rd);
      CASE c OF
        '\"' =>
          IF dblquoteparity = 0 THEN
            Wr.PutText(wr, "{\\tt ")
          ELSE
            Wr.PutText(wr, "}")
          END;
          dblquoteparity := (dblquoteparity + 1) MOD 2 |
        '`' => Wr.PutText(wr, "}"); RETURN
      ELSE
        Wr.PutChar(wr, c)
      END
    END
  END Undisplay;

EXCEPTION UsageError;

PROCEDURE Fix(t: TEXT): TEXT RAISES {UsageError} =
  VAR len := Text.Length(t);
     tail3 := Text.Sub(t, MAX(0, len-3), 3);
     tail5 := Text.Sub(t, MAX(0, len-5), 5);
  BEGIN
    IF Text.Equal(tail3, ".i3") THEN
      RETURN Text.Sub(t, 0, len-3) & ".i.tex"
    ELSIF Text.Equal(tail3, ".m3") THEN
      RETURN Text.Sub(t, 0, len-3) & ".m.tex"
    ELSIF Text.Equal(tail5, ".frag") THEN
      RETURN Text.Sub(t, 0, len-5) & ".f.tex"
    ELSE
      RAISE UsageError
    END
  END Fix;

PROCEDURE Open(intf: TEXT): Rd.T RAISES {Rd.Failure} =
  VAR 
    res := FileRd.Open(intf); 
  BEGIN
    IF res = NIL THEN RAISE Rd.Failure(NIL) END;
    RETURN res
  END Open;

(* "FileRd.Open" seems to raise "Rd.Failure" when there is no
   such file; however its spec says that it returns "NIL" in this case.
   So "Open" is coded to handle both possibilities.  *)

PROCEDURE PutFile(textName, fileName: TEXT) =
  VAR 
    wr := FileWr.Open(fileName);
  BEGIN
    Wr.PutText (wr, Bundle.Get(bundle, textName));
    Wr.Close(wr)
  END PutFile;

PROCEDURE GetParam (n: INTEGER): TEXT =
  BEGIN
    IF (n >= Params.Count) THEN RETURN NIL END;
    RETURN Params.Get (n);
  END GetParam;
  
VAR
  m3file, texfile: TEXT;
  raw := FALSE;
  rd: Rd.T;
  wr: Wr.T;
BEGIN
  TRY
    m3file := GetParam(1);
    IF m3file = NIL THEN RAISE UsageError END;
    IF Text.Equal(m3file, "-example") THEN
      PutFile("xmpl", "Example.i3");
      PutFile("make", "Makefile");
      PutFile("tex", "Example.tex")
    ELSIF Text.Equal(m3file, "-defs") THEN
      PutFile("header", "m3totex.tex")
    ELSE
      IF Text.Equal(m3file, "-raw") THEN
        raw := TRUE;
        m3file := GetParam(2);
        IF m3file = NIL THEN RAISE UsageError END;
        texfile := GetParam(3)
      ELSE
        texfile := GetParam(2)
      END;
      IF texfile = NIL THEN texfile := Fix(m3file) END;
      rd := Open(m3file);
      wr := FileWr.Open(texfile);
      Trans(rd, wr, raw);
      Wr.Close(wr)
    END
  EXCEPT
    Rd.Failure => Wr.PutText(Stdio.stderr, "? can't read file\n")
  | UsageError =>
      Wr.PutText(
        Stdio.stderr,
        "? usage: m3ToTex name.extension [name.extension]\n")
  | Wr.Failure =>
      Wr.PutText(Stdio.stderr, "? can't write file\n")
  END
END m3totex.
