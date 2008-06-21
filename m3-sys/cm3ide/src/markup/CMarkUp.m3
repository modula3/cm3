(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Dec  8 10:46:37 PST 1994 by kalsow                   *)

MODULE CMarkUp;

IMPORT Buf, Marker, Wx, Wr, Thread;

VAR special := ARRAY CHAR OF BOOLEAN { FALSE, .. };

PROCEDURE Annotate (buf: Buf.T;  wx: Wx.T;  ins: Marker.LineInsertion)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR c: CHAR;  line := 1;
  BEGIN
    wx.put ("<PRE>\n");
    GenInserts (wx, ins, line);
    FOR i := 0 TO LAST (buf^) DO
      c := buf[i];
      IF NOT special[c] THEN wx.putChar (c);
      ELSIF  (c = '<')  THEN wx.put ("&lt;");
      ELSIF  (c = '>')  THEN wx.put ("&gt;");
      ELSIF  (c = '&')  THEN wx.put ("&amp;");
      ELSIF  (c = '"')  THEN wx.put ("&quot;");
      ELSIF  (c = '\n') THEN
        wx.putChar ('\n'); INC (line);
        GenInserts (wx, ins, line);
      ELSE
        (*skip*)
      END;
    END;
    wx.put ("\n");
    GenInserts (wx, ins, LAST (INTEGER));
    wx.put ("</PRE>\n");
  END Annotate;

PROCEDURE GenInserts (wx: Wx.T;  ins: Marker.LineInsertion;  line: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    WHILE (ins # NIL) AND (ins.line < line) DO
      wx.put (ins.insert);
      ins := ins.next;
    END;
  END GenInserts;

BEGIN
  special ['\n'] := TRUE;
  special ['<'] := TRUE;
  special ['>'] := TRUE;
  special ['&'] := TRUE;
  special ['"'] := TRUE;
  special ['\000'] := TRUE;
END CMarkUp.
