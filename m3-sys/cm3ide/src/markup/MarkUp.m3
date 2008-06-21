(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Dec  8 10:57:49 PST 1994 by kalsow                   *)
(*      modified on Mon May  3 12:23:44 PDT 1993 by mcjones                  *)
(*      modified on Tue Feb  9 12:30:09 1993 by gnelson                      *)
(*      modified on Thu Jan  7 11:08:45 PST 1993 by muller                   *)

MODULE MarkUp;

IMPORT Buf, Wx, Marker, M3MarkUp, Text, Word, Wr, Thread;

CONST
  EscapeSpecials = TRUE;
  EOF = '\000';

TYPE
  State = RECORD
    buf      : Buf.T    := NIL;
    wx       : Wx.T     := NIL;
    eof      : BOOLEAN  := FALSE;
    offset   : INTEGER  := 0;
    buf_len  : INTEGER  := 0;
    cur_line : INTEGER  := 0;
    ins_cnt  : CARDINAL := 0;
    ins      : Marker.CharInsertion := NIL;
    xns      : Marker.LineInsertion := NIL;
  END;

VAR (* CONST *)
  WhiteSpace := ARRAY CHAR OF BOOLEAN { FALSE, .. };

PROCEDURE Annotate (buf: Buf.T;  wx: Wx.T;
                    ins: Marker.LineInsertion;
                    target: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR s: State;
  BEGIN
    IF (buf = NIL) THEN RETURN END;
    s.buf      := buf;
    s.wx       := wx;
    s.eof      := FALSE;
    s.offset   := 0;
    s.cur_line := 1;
    s.buf_len  := NUMBER (buf^);
    s.ins_cnt  := 0;
    s.ins      := M3MarkUp.Get (buf, target);
    s.xns      := ins;
    Trans (s);
  END Annotate;

(*------------------------------------------------------- file processing ---*)

PROCEDURE Trans(VAR s: State)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR spec: BOOLEAN;
  BEGIN
    CommitC (s);
(*    AdvanceToBlankLine (s);  *)
    SkipInitialComments (s);
    WHILE NOT s.eof DO
      spec := Prog (s);
      IF (spec) THEN OutT (s, "<BLOCKQUOTE><EM>"); END;
      Comment (s, spec);
      IF (spec) THEN OutT (s, "</EM></BLOCKQUOTE>"); END;
    END;
    OutT (s, Wr.EOL);
    s.offset   := LAST (INTEGER);
    s.cur_line := LAST (INTEGER);
    CommitC (s);
  END Trans;

(* SkipInitialComments should really be moved to M3Markup module. *)

PROCEDURE SkipInitialComments (VAR s: State)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR commentlevel: CARDINAL := 0; c: CHAR;

  PROCEDURE SkipWhiteSpace ()
    RAISES {Wr.Failure, Thread.Alerted} =
    VAR blank_line := FALSE;
    BEGIN
      (* Skip white space. *)
      CommitC(s); c := GetC (s);
      WHILE WhiteSpace [c] DO
        IF (c = '\n') THEN
          IF blank_line THEN EXIT; END;
          blank_line := TRUE;
        END;
        CommitC(s); c := GetC (s);
      END;
    END SkipWhiteSpace;

  BEGIN

    SkipWhiteSpace();
    IF c # '(' THEN UngetC(s,c); CommitC(s); RETURN; END;

    LOOP
      REPEAT
        IF s.eof THEN EXIT END;
        CASE c OF
        | '(' => CommitC(s); c := GetC(s);
	    IF c = '*' THEN INC(commentlevel) ELSE UngetC (s, c) END;
        |  '*' => CommitC(s); c := GetC(s); 
	    IF c = ')' THEN DEC(commentlevel) ELSE UngetC (s, c) END;
        ELSE (* do nothing *)
        END;
        CommitC(s); c := GetC(s);
      UNTIL commentlevel = 0; 

      SkipWhiteSpace();
    
      IF c # '(' THEN EXIT END;
    END;
    UngetC(s,c); CommitC(s);

  END SkipInitialComments;

<*UNUSED*>PROCEDURE AdvanceToBlankLine (VAR s: State)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR blank: BOOLEAN; c: CHAR;
  BEGIN
    REPEAT
      blank := TRUE;
      LOOP
        c := GetC (s);  CommitC (s);
        IF s.eof THEN EXIT END;
        IF c = '\n' THEN EXIT END;
        IF NOT WhiteSpace [c] THEN blank := FALSE END
      END
    UNTIL blank OR s.eof;
  END AdvanceToBlankLine;

PROCEDURE Prog (VAR s: State): BOOLEAN
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    c, d: CHAR;
    vspace := 0;
    hspace := 0;
    empty := TRUE;
    startOfLine := TRUE;

  PROCEDURE Space ()
    RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN
      IF empty THEN (*! OutT (s, "\\par\\medskip ");  !*) END;
      empty := FALSE;
      startOfLine := FALSE;
      IF vspace = 1 THEN
        OutT (s, Wr.EOL);
      ELSIF vspace > 1 THEN
        OutT (s, Wr.EOL);  OutT (s, Wr.EOL);
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
        | '\r' =>
            CommitC (s);
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
      IF (vspace > 0) THEN OutT (s, Wr.EOL) END;
    FINALLY
      OutT (s, "</PRE>");
    END;
    RETURN (vspace < 2) AND (NOT empty) AND (NOT s.eof);
  END Prog;

PROCEDURE Comment (VAR s: State;  in_spec: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted} =
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

      | '<', '>' =>
          CommitC (s);
          d := GetC (s);
          IF (d = c)
            THEN  CommitC (s);    OutC (s, c);
            ELSE  UngetC (s, d);  OutX (s, c);
          END;

      | '&' =>
          CommitC (s);
          OutX (s, c);

      | '|' =>
          CommitC (s);
          IF startOfLine THEN 
            IF NOT afterDisplay THEN OutT (s, "<PRE>"); OutT (s, Wr.EOL); END;
            c := GetC (s);
            IF (c = ' ') THEN CommitC (s);  ELSE UngetC (s, c); END;
            Display (s);
            c := '\n';
            afterDisplay := TRUE;
          ELSE 
            OutT (s, "|");
          END;

      | '\r' =>
          CommitC (s);  (* discard *)

      | '\n' => 
          CommitC (s);
          IF afterDisplay THEN 
            OutT (s, "</PRE>");  OutT (s, Wr.EOL);
            afterDisplay := FALSE;
          ELSIF startOfLine THEN
            OutT (s, "<P>");  OutT (s, Wr.EOL);
          ELSE
            OutT (s, Wr.EOL);
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
              OutT (s, "</PRE>");  OutT (s, Wr.EOL);
              afterDisplay := FALSE;
            END;
            OutC (s, c);
          END;

      | '\134' =>
          CommitC (s);
          IF afterDisplay THEN 
            OutT (s, "</PRE>");  OutT (s, Wr.EOL);
            afterDisplay := FALSE;
          END;
          IF Match (s, "char'")
            THEN EatTeXChar (s);
            ELSE OutC (s, c);
          END;

      ELSE
          CommitC (s);
          IF afterDisplay AND c # ' ' THEN 
            OutT (s, "</PRE>");  OutT (s, Wr.EOL);
            afterDisplay := FALSE;
          END;
          OutC (s, c);

      END; (*CASE*)

      IF (c = '\n') THEN
        startOfLine := TRUE;
      ELSIF NOT WhiteSpace [c] THEN
        startOfLine := FALSE;
      END;
    END; (*WHILE*)
  END Comment;

PROCEDURE Display (VAR s: State)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR c, d: CHAR; 
  BEGIN
    OutT (s, "      ");
    WHILE NOT s.eof DO
      c := GetC (s);
      CASE c OF
      | '<', '>', '&', '"' =>
          CommitC (s);  OutX (s, c);
      |'\r' =>
          CommitC (s);
      |'\n' =>
          CommitC (s);  OutT (s, Wr.EOL);  RETURN
      | ' ' =>
          CommitC (s);  OutC (s, ' ');
      | '`' =>
          CommitC (s);  Undisplay (s);
      | '*' =>
          d := GetC (s);
          UngetC (s, d);
          IF (d = ')') THEN
            UngetC (s, c);
            OutT (s, Wr.EOL);
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
  RAISES {Wr.Failure, Thread.Alerted} =
  CONST CodeEdge = ARRAY BOOLEAN OF TEXT { "<KBD>", "</KBD>" };
  VAR c, d: CHAR;  in_code := TRUE;
  BEGIN
    OutT (s, "<KBD>");
    WHILE NOT s.eof DO
      c := GetC (s);  CommitC (s);
      CASE c OF
      | '&'    => OutC (s, c);
      | '\"'   => OutT (s, CodeEdge [in_code]); in_code := NOT in_code;
      | '`'    => OutT (s, "</KBD>"); RETURN;
      | '\134' =>
          IF Match (s, "char'")
            THEN EatTeXChar (s);
            ELSE OutC (s, c);
          END;
      | '<', '>' =>
          d := GetC (s);
          IF (d = c)
            THEN  CommitC (s);    OutC (s, c);
            ELSE  UngetC (s, d);  OutX (s, c);
          END;
      ELSE
          OutC (s, c);
      END;
    END;
  END Undisplay;

PROCEDURE CommentGap (VAR s: State): BOOLEAN
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR c, d: CHAR;  blankLine := FALSE;
  BEGIN
    WHILE NOT s.eof DO
      c := GetC (s);
      CASE c OF
      | '\r' =>
          CommitC (s);
      | '\n' =>
          CommitC (s);
          OutT (s, "<BR>");
          OutT (s, Wr.EOL);
          (***
          IF blankLine THEN OutT (s, "<P>"); END;
          **)
          blankLine := TRUE;
      | ' ', '\t'  =>
          CommitC (s);
          OutC (s, c);
      | '('  =>
          CommitC (s);
          d := GetC (s);
          IF (d = '*') THEN
            CommitC (s);
            RETURN FALSE;
          ELSE
            UngetC (s, d);
            UngetC (s, c);
            RETURN TRUE;
          END;
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
  RAISES {Wr.Failure, Thread.Alerted} =
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
    WHILE WhiteSpace[c] DO
      c := GetC (s);
    END;
    UngetC (s, c);
    CommitC (s);
  END EatTeXChar;

(*--------------------------------------------------------- low-level I/O ---*)

PROCEDURE Match (VAR s: State;  word: TEXT): BOOLEAN
  RAISES {Wr.Failure, Thread.Alerted} =
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
    IF (s.offset >= 0) AND (s.buf[s.offset] = '\n') THEN
      DEC (s.cur_line);
    END;
  END UngetC;

PROCEDURE CommitC (VAR s: State)
  RAISES {Wr.Failure, Thread.Alerted} =
  (* It's illegal to call UngetC after calling Commit. *)
  BEGIN
    WHILE (s.ins # NIL)
      AND (s.ins_cnt < s.ins.count)
      AND (s.ins.insert [s.ins_cnt].offset < s.offset) DO
      WITH z = s.ins.insert [s.ins_cnt] DO
        IF (z.txt # NIL) THEN  OutT (s, z.txt);  END;
        IF (z.length > 0) THEN
          s.wx.putStr (SUBARRAY (s.buf^, z.start, z.length));
        END;
      END;
      INC (s.ins_cnt);
      IF (s.ins_cnt >= s.ins.count) THEN
        s.ins_cnt := 0;
        s.ins := s.ins.next;
      END;
    END;
    WHILE (s.xns # NIL) AND (s.xns.line < s.cur_line) DO
      OutT (s, s.xns.insert);
      s.xns := s.xns.next;
    END;
  END CommitC;

PROCEDURE GetC (VAR s: State): CHAR =
  VAR ch: CHAR;
  BEGIN
    IF (s.offset < s.buf_len)
      THEN ch := s.buf[s.offset];  INC (s.offset);
      ELSE ch := EOF;  s.eof := TRUE;
    END;
    IF (ch = '\n') THEN INC (s.cur_line); END;
    RETURN ch;
  END GetC;

PROCEDURE OutT (VAR s: State;  a, b, c: TEXT := NIL)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    s.wx.put (a, b, c);
  END OutT;

PROCEDURE OutC (VAR s: State;  ch: CHAR)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    s.wx.putChar (ch);
  END OutC;

PROCEDURE OutX (VAR s: State;  ch: CHAR)
  RAISES {Wr.Failure, Thread.Alerted} =
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
  WhiteSpace [' ']  := TRUE;
  WhiteSpace ['\f'] := TRUE;
  WhiteSpace ['\n'] := TRUE;
  WhiteSpace ['\r'] := TRUE;
  WhiteSpace ['\t'] := TRUE;
END MarkUp.
