(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 16 09:17:10 PDT 1995 by kalsow                   *)
(*      modified on Mon May  3 12:23:44 PDT 1993 by mcjones                  *)
(*      modified on Tue Feb  9 12:30:09 1993 by gnelson                      *)
(*      modified on Thu Jan  7 11:08:45 PST 1993 by muller                   *)

MODULE MarkUp;

IMPORT Buf, Wx, M3MarkUp, Text, Word;

CONST
  EscapeSpecials = TRUE;
  EOF = '\000';

TYPE
  State = RECORD
    buf    : Buf.T   := NIL;
    wx     : Wx.T    := NIL;
    eof    : BOOLEAN := FALSE;
    offset : INTEGER := 0;
    buf_len: INTEGER := 0;
    ins    : M3MarkUp.Insertion := NIL;
  END;

PROCEDURE Annotate (buf: Buf.T;  wx: Wx.T;  full := TRUE) =
  VAR s: State;
  BEGIN
    IF (buf = NIL) THEN RETURN END;
    s.buf     := buf;
    s.wx      := wx;
    s.eof     := FALSE;
    s.offset  := 0;
    s.buf_len := NUMBER (buf^);
    IF full THEN s.ins     := M3MarkUp.Get (buf); END;
    Trans (s, full);
  END Annotate;

(*------------------------------------------------------- file processing ---*)

PROCEDURE Trans(VAR s: State; full: BOOLEAN) =
  VAR spec: BOOLEAN;
  BEGIN
    CommitC (s);
    IF NOT full THEN AdvanceToBlankLine (s); END;
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

(* -- used to skip the copyright & last modified lines -- *)

PROCEDURE AdvanceToBlankLine (VAR s: State) =
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

PROCEDURE Prog (VAR s: State): BOOLEAN =
  VAR
    c, d: CHAR;
    vspace := 0;
    hspace := 0;
    empty := TRUE;
    startOfLine := TRUE;

  PROCEDURE Space () =
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
        | '<', '>', '&', '\"' =>
            Space ();  CommitC (s);  OutX (s, c);
        | EOF  =>
            CommitC (s);
            EXIT;
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

PROCEDURE Comment (VAR s: State;  in_spec: BOOLEAN) =
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

PROCEDURE Display (VAR s: State) =
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

PROCEDURE Undisplay (VAR s: State) =
  CONST CodeEdge = ARRAY BOOLEAN OF TEXT { "<KBD>", "</KBD>" };
  VAR c: CHAR;  in_code := TRUE;
  BEGIN
    OutT (s, "<KBD>");
    WHILE NOT s.eof DO
      c := GetC (s);  CommitC (s);
      CASE c OF
      | '<', '>', '&' => OutC (s, c);
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

PROCEDURE CommentGap (VAR s: State): BOOLEAN =
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

PROCEDURE EatTeXChar (VAR s: State) =
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

PROCEDURE Match (VAR s: State;  word: TEXT): BOOLEAN =
  VAR len := Text.Length (word);  i := 0;  ch: CHAR;
  BEGIN
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

PROCEDURE UngetC (VAR s: State;  <*UNUSED*> ch: CHAR) =
  BEGIN
    DEC (s.offset);
  END UngetC;

PROCEDURE CommitC (VAR s: State) =
  (* It's illegal to call UngetC after calling Commit. *)
  BEGIN
    WHILE (s.ins # NIL) AND (s.ins.offset < s.offset) DO
      OutT (s, s.ins.insert);
      s.ins := s.ins.next;
    END;
  END CommitC;

PROCEDURE GetC (VAR s: State): CHAR =
  VAR ch: CHAR;
  BEGIN
    IF (s.offset < s.buf_len)
      THEN ch := s.buf[s.offset];  INC (s.offset);
      ELSE ch := EOF;  s.eof := TRUE;
    END;
    RETURN ch;
  END GetC;

PROCEDURE OutT (VAR s: State;  a, b, c: TEXT := NIL) =
  BEGIN
    IF (a # NIL) THEN Wx.PutText (s.wx, a); END;
    IF (b # NIL) THEN Wx.PutText (s.wx, b); END;
    IF (c # NIL) THEN Wx.PutText (s.wx, c); END;
  END OutT;

PROCEDURE OutC (VAR s: State;  ch: CHAR) =
  BEGIN
    Wx.PutChar (s.wx, ch);
  END OutC;

PROCEDURE OutX (VAR s: State;  ch: CHAR) =
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
