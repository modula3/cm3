(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 30 16:08:13 PST 1995 by kalsow                   *)
(*      modified on Mon May  3 12:23:44 PDT 1993 by mcjones                  *)
(*      modified on Tue Feb  9 12:30:09 1993 by gnelson                      *)
(*      modified on Thu Jan  7 11:08:45 PST 1993 by muller                   *)

(* Implementation of the m3tohtml command; see its manpage for details. *)

MODULE MarkUp;

IMPORT Rd, Wr, Thread, M3MarkUp, Text, Word;

CONST
  EscapeSpecials = TRUE;
  EOF = '\000';

TYPE
  State = RECORD
    rd     : Rd.T := NIL;
    wr     : Wr.T := NIL;
    eof    : BOOLEAN := FALSE;
    ins    : M3MarkUp.Insertion := NIL;
    offset : INTEGER := 0;
    path   : TEXT;
    un_buf : ARRAY [0..15] OF CHAR;
    un_len : INTEGER := 0;
  END;

PROCEDURE Annotate (rd: Rd.T;  wr: Wr.T;  path: TEXT)
  RAISES {Thread.Alerted, Wr.Failure, Rd.Failure} =
  VAR s: State;  here: CARDINAL;
  BEGIN
    s.rd   := rd;
    s.wr   := wr;
    s.path := path;
    here   := Rd.Index (rd);
    s.ins  := M3MarkUp.Get (rd, path);
    Rd.Seek (rd, here);
    Trans (s);
  END Annotate;

(*------------------------------------------------------- file processing ---*)

PROCEDURE Trans(VAR s: State)
  RAISES {Thread.Alerted, Wr.Failure, Rd.Failure} =
  VAR spec: BOOLEAN;
  BEGIN
    CommitC (s);
    (*  AdvanceToBlankLine (s);  *)
    WHILE NOT s.eof DO
      spec := Prog (s);
      IF (spec) THEN OutT (s, "<BLOCKQUOTE><EM>"); END;
      Comment (s, spec);
      IF (spec) THEN OutT (s, "</EM></BLOCKQUOTE>"); END;
    END;
    OutT (s, "\n");
    s.offset := LAST (INTEGER);
    CommitC (s);
  END Trans;

(***************
   -- was used to skip the copyright & last modified lines

PROCEDURE AdvanceToBlankLine (VAR s: State)
  RAISES {Thread.Alerted, Rd.Failure, Wr.Failure} =
  VAR blank: BOOLEAN; c: CHAR;
  BEGIN
    REPEAT
      blank := TRUE;
      LOOP
        c := GetC (s);  CommitC (s);
        IF s.eof THEN EXIT END;
        IF c = '\n' THEN EXIT END;
        IF c # ' ' THEN blank := FALSE END
      END
    UNTIL blank OR s.eof;
  END AdvanceToBlankLine;
************)

PROCEDURE Prog (VAR s: State): BOOLEAN
  RAISES {Thread.Alerted, Wr.Failure, Rd.Failure} =
  VAR
    c, d: CHAR;
    vspace := 0;
    hspace := 0;
    empty := TRUE;
    startOfLine := TRUE;

  PROCEDURE Space () RAISES {Thread.Alerted, Wr.Failure} =
    BEGIN
      IF empty THEN (*! OutT (s, "\\par\\medskip ");  !*) END;
      empty := FALSE;
      startOfLine := FALSE;
      IF vspace = 1 THEN
        OutT (s, "\n");
      ELSIF vspace > 1 THEN
        OutT (s, "\n\n");
      END;
      vspace := 0;
      WHILE hspace > 0 DO OutT (s, " "); DEC (hspace); END;
    END Space;

  BEGIN
    OutT (s, "<PRE>");
    TRY
      WHILE NOT s.eof DO
        c := GetC (s);
        CASE c OF
        | '\n' =>
            CommitC (s); INC(vspace); hspace := 0; startOfLine := TRUE;
        | ' '  =>
            CommitC (s); INC(hspace);
        | '('  =>
            CommitC (s);
            d := GetC (s);
            IF (d = '*') AND startOfLine AND (hspace = 0) THEN
              CommitC(s);
              EXIT;
            END;
            UngetC (s, d);  Space ();  OutC (s, c);
        | EOF  =>
            CommitC (s);
            EXIT;
        | '<', '>', '&', '"' =>
            Space (); CommitC (s); OutX (s, c);
        ELSE
            Space (); CommitC (s); OutC (s, c);
        END;
      END;
      IF (vspace > 0) THEN OutT (s, "\n") END;
    FINALLY
      OutT (s, "</PRE>");
    END;
    RETURN (vspace < 2) AND (NOT empty) AND (NOT s.eof);
  END Prog;

PROCEDURE Comment (VAR s: State;  in_spec: BOOLEAN)
  RAISES {Thread.Alerted, Wr.Failure, Rd.Failure} =
  CONST CodeEdge = ARRAY BOOLEAN OF TEXT { "<CODE>", "</CODE>" };
  VAR c, d: CHAR; startOfLine := TRUE; afterDisplay := FALSE; in_code := FALSE;
  BEGIN
    WHILE (NOT s.eof) DO
      c := GetC (s);
      CASE c OF
      | '\"' =>
          CommitC (s);
          OutT (s, CodeEdge [in_code]);
          in_code := NOT in_code;

      | '<', '>', '&' =>
          CommitC (s);
          OutX (s, c);

      | '|' =>
          CommitC (s);
          IF startOfLine THEN 
            IF NOT afterDisplay THEN OutT (s, "<PRE>\n"); END;
            c := GetC (s);
            IF (c = ' ') THEN CommitC (s);  ELSE UngetC (s, c); END;
            Display (s);
            c := '\n';
            afterDisplay := TRUE;
          ELSE 
            OutT (s, "|");
          END;

      | '\n' => 
          CommitC (s);
          IF afterDisplay THEN 
            OutT (s, "</PRE>\n");
            afterDisplay := FALSE;
          ELSIF startOfLine THEN
            OutT (s, "<P>\n");
          ELSE
            OutT (s, "\n");
          END;

      | '*' =>
          CommitC (s);
          d := GetC (s);
          IF (d = ')') THEN
            CommitC (s);
            IF in_spec OR CommentGap (s) THEN RETURN; END;
          ELSE
            UngetC (s, d);
            IF afterDisplay THEN
              OutT (s, "</PRE>\n");
              afterDisplay := FALSE;
            END;
            OutC (s, c);
          END;

      | '\134' =>
          CommitC (s);
          IF afterDisplay THEN 
            OutT (s, "</PRE>\n");
            afterDisplay := FALSE;
          END;
          IF Match (s, "char'")
            THEN EatTeXChar (s);
            ELSE OutC (s, c);
          END;

      ELSE
          CommitC (s);
          IF afterDisplay AND c # ' ' THEN 
            OutT (s, "</PRE>\n");
            afterDisplay := FALSE;
          END;
          OutC (s, c);

      END; (*CASE*)

      startOfLine := (c = '\n') OR (startOfLine AND c = ' ')
    END; (*WHILE*)
  END Comment;

PROCEDURE Display (VAR s: State)
  RAISES {Thread.Alerted, Wr.Failure, Rd.Failure} =
  VAR c, d: CHAR; 
  BEGIN
    OutT (s, "      ");
    WHILE NOT s.eof DO
      c := GetC (s);
      CASE c OF
      | '<', '>', '&', '"' =>
          CommitC (s);  OutX (s, c);
      |'\n' =>
          CommitC (s);  OutC (s, '\n');  RETURN
      | ' ' =>
          CommitC (s);  OutC (s, ' ');
      | '`' =>
          CommitC (s);  Undisplay (s);
      | '*' =>
          d := GetC (s);
          UngetC (s, d);
          IF (d = ')') THEN
            UngetC (s, c);
            OutC (s, '\n');
            RETURN;
          END;
          CommitC (s);
          OutC (s, c);
      | '\134' =>
          CommitC (s);
          IF Match (s, "char'")
            THEN EatTeXChar (s);
            ELSE OutC (s, c);
          END;

      ELSE
          CommitC (s);  OutC (s, c);
      END;
    END;
  END Display;

PROCEDURE Undisplay (VAR s: State)
  RAISES {Thread.Alerted, Wr.Failure, Rd.Failure} =
  CONST CodeEdge = ARRAY BOOLEAN OF TEXT { "<KBD>", "</KBD>" };
  VAR c: CHAR;  in_code := TRUE;
  BEGIN
    OutT (s, "<KBD>");
    WHILE NOT s.eof DO
      c := GetC (s);  CommitC (s);
      CASE c OF
      | '<', '>', '&' => OutX (s, c);
      | '\"'          => OutT (s, CodeEdge [in_code]); in_code := NOT in_code;
      | '`'           => OutT (s, "</KBD>"); RETURN;
      | '\134' =>
          IF Match (s, "char'")
            THEN EatTeXChar (s);
            ELSE OutC (s, c);
          END;
      ELSE               OutC (s, c);
      END;
    END;
  END Undisplay;

PROCEDURE CommentGap (VAR s: State): BOOLEAN
  RAISES {Thread.Alerted, Wr.Failure, Rd.Failure} =
  VAR c, d: CHAR;  blankLine := FALSE;
  BEGIN
    WHILE NOT s.eof DO
      c := GetC (s);
      CASE c OF
      | '\n' =>
          CommitC (s);
          OutC (s, c);
          IF blankLine THEN OutT (s, "<P>"); END;
          blankLine := TRUE;
      | ' '  =>
          CommitC (s);
          OutC (s, c);
      | '('  =>
          CommitC (s);
          d := GetC (s);
          IF (d = '*') THEN  CommitC (s);  RETURN FALSE;  END;
          UngetC (s, d);
          UngetC (s, c);
          RETURN TRUE;
      | EOF  =>
          CommitC (s);
          RETURN TRUE;
      ELSE
          UngetC (s, c);
          RETURN TRUE;
      END;
    END;
    RETURN TRUE;
  END CommentGap;

PROCEDURE EatTeXChar (VAR s: State)
  RAISES {Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR c: CHAR;  val := 0;  cnt := 0;
  BEGIN
    (* scan the digits *)
    LOOP
      c := GetC (s);
      IF (c < '0') OR ('7' < c) THEN EXIT; END;
      val := 8 * val + ORD (c) - ORD ('0');
      INC (cnt);
    END;

    IF (cnt = 0) THEN
      (* we didn't find anything?? *)
      OutT (s, "\\char'");
      UngetC (s, c);
      RETURN;
    END;

    (* put out the real character *)
    CommitC (s);
    OutC (s, VAL (Word.And (val, 16_ff), CHAR));

    (* skip the white space following the TeXism *)
    WHILE (c = ' ') OR (c = '\n') DO
      c := GetC (s);
    END;
    UngetC (s, c);
    CommitC (s);
  END EatTeXChar;

(*--------------------------------------------------------- low-level I/O ---*)

PROCEDURE Match (VAR s: State;  word: TEXT): BOOLEAN
  RAISES {Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR len := Text.Length (word);  i: INTEGER;  ch: CHAR;
  BEGIN
    IF (len > NUMBER (s.un_buf)) THEN
      (* there's not enough room in the unget buffer => no match *)
      RETURN FALSE;
    END;

    i := 0;
    WHILE (i < len) DO
      ch := GetC (s);
      IF Text.GetChar (word, i) # ch THEN
        (* bail out *)
        UngetC (s, ch);
        WHILE (i > 0) DO
          DEC (i);
          UngetC (s, Text.GetChar (word, i));
        END;
        RETURN FALSE;
      END;
      INC (i);
    END;

    (* we found a match, commit it and return *)
    CommitC (s);
    RETURN TRUE;
  END Match;

PROCEDURE UngetC (VAR s: State;  ch: CHAR) =
  BEGIN
    DEC (s.offset);
    s.un_buf [s.un_len] := ch;  INC (s.un_len);
  END UngetC;

PROCEDURE CommitC (VAR s: State)
  RAISES {Thread.Alerted, Wr.Failure} =
  (* It's illegal to call UngetC after calling Commit. *)
  BEGIN
    WHILE (s.ins # NIL) AND (s.ins.offset < s.offset) DO
      OutT (s, s.ins.insert);
      s.ins := s.ins.next;
    END;
  END CommitC;

PROCEDURE GetC (VAR s: State): CHAR
  RAISES {Thread.Alerted, Rd.Failure} =
  <*FATAL Rd.EndOfFile*>
  VAR ch: CHAR;
  BEGIN
    IF (s.eof) OR Rd.EOF (s.rd) THEN
      s.eof := TRUE;
      ch := EOF;
    ELSE
      INC (s.offset);
      IF (s.un_len > 0)
        THEN DEC (s.un_len);  ch := s.un_buf [s.un_len];
        ELSE ch := Rd.GetChar (s.rd);
      END;
    END;
    RETURN ch;
  END GetC;

PROCEDURE OutT (VAR s: State;  a, b, c: TEXT := NIL)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    IF (a # NIL) THEN Wr.PutText (s.wr, a); END;
    IF (b # NIL) THEN Wr.PutText (s.wr, b); END;
    IF (c # NIL) THEN Wr.PutText (s.wr, c); END;
  END OutT;

PROCEDURE OutC (VAR s: State;  ch: CHAR)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    Wr.PutChar (s.wr, ch);
  END OutC;

PROCEDURE OutX (VAR s: State;  ch: CHAR)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    IF NOT EscapeSpecials THEN OutC (s, ch);
    ELSIF (ch = '<')      THEN OutT (s, "&lt;");
    ELSIF (ch = '>')      THEN OutT (s, "&gt;");
    ELSIF (ch = '&')      THEN OutT (s, "&amp;");
    ELSIF (ch = '"')      THEN OutT (s, "&quot;");
    ELSE                       OutC (s, ch);
    END;
  END OutX;

BEGIN
END MarkUp.
