(*
   FBEWr.m3
   Traditional, fixed-width, ASCII backend for Formatter.
   David Nichols, Xerox PARC
   July, 1991

   $Id$
*)
(* Copyright (c) 1991 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works based
   upon this software are permitted.  Any distribution of this software or
   derivative works must comply with all applicable United States export
   control laws.  This software is made available AS IS, and Xerox Corporation
   makes no warranty about the software, its performance or its conformity to
   any specification. *)

MODULE FBEWr;

IMPORT FBE, Text, Wr, Thread;

<* FATAL Thread.Alerted *>

TYPE
  T = FBE.T OBJECT
        wr    : Wr.T;            (* the writer *)
        width : INTEGER;         (* logical width *)
        curPos            := 0;  (* where we are on the line *)
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

(* Fonts are ignored, so we always return NIL. *)
PROCEDURE GetFont (<*UNUSED*> o: T; <*UNUSED*> fontName: TEXT): FBE.Font =
  BEGIN
    RETURN NIL;
  END GetFont;

PROCEDURE PageWidth (o: T): REAL =
  BEGIN
    RETURN FLOAT(o.width);
  END PageWidth;

PROCEDURE TextWidth (<*UNUSED*> o: T; t: TEXT; <*UNUSED*> font: FBE.Font):
  REAL =
  BEGIN
    RETURN FLOAT(Text.Length(t));
  END TextWidth;

PROCEDURE CharWidth (<*UNUSED*> o   : T;
                     <*UNUSED*> c   : CHAR;
                     <*UNUSED*> font: FBE.Font): REAL =
  BEGIN
    RETURN 1.0;
  END CharWidth;

PROCEDURE NewLine (o: T) RAISES {FBE.Failed} =
  BEGIN
    TRY
      Wr.PutChar(o.wr, '\n');
      o.curPos := 0;
    EXCEPT
    | Wr.Failure (r) =>
        RAISE
          FBE.Failed(NEW(FBE.WrFailure, info := "writer failure", wrRef := r));
    END;
  END NewLine;

PROCEDURE Goto (o: T; pos: REAL) RAISES {FBE.Failed} =
  VAR iPos := TRUNC(pos);
  BEGIN
    TRY
      WHILE o.curPos < iPos DO Wr.PutChar(o.wr, ' '); INC(o.curPos); END;
    EXCEPT
    | Wr.Failure (r) =>
        RAISE
          FBE.Failed(NEW(FBE.WrFailure, info := "writer failure", wrRef := r));
    END;
  END Goto;

PROCEDURE GetPos (o: T): REAL =
  BEGIN
    RETURN FLOAT(o.curPos);
  END GetPos;

PROCEDURE PutText (o: T; t: TEXT; <*UNUSED*> font: FBE.Font)
  RAISES {FBE.Failed} =
  BEGIN
    TRY
      Wr.PutText(o.wr, t);
      FOR i := 0 TO Text.Length(t) - 1 DO
        IF Text.GetChar(t, i) = '\n' THEN
          o.curPos := 0;
        ELSE
          INC(o.curPos);
        END;
      END;
    EXCEPT
    | Wr.Failure (r) =>
        RAISE
          FBE.Failed(NEW(FBE.WrFailure, info := "writer failure", wrRef := r));
    END;
  END PutText;

PROCEDURE PutChar (o: T; c: CHAR; <*UNUSED*> font: FBE.Font)
  RAISES {FBE.Failed} =
  BEGIN
    TRY
      Wr.PutChar(o.wr, c);
      IF c = '\n' THEN o.curPos := 0; ELSE INC(o.curPos); END;
    EXCEPT
    | Wr.Failure (r) =>
        RAISE
          FBE.Failed(NEW(FBE.WrFailure, info := "writer failure", wrRef := r));
    END;
  END PutChar;

PROCEDURE Flush (o: T) RAISES {FBE.Failed} =
  BEGIN
    TRY
      Wr.Flush(o.wr);
    EXCEPT
    | Wr.Failure (r) =>
        RAISE
          FBE.Failed(NEW(FBE.WrFailure, info := "writer failure", wrRef := r));
    END;
  END Flush;

PROCEDURE Close (o: T) RAISES {FBE.Failed} =
  BEGIN
    o.Flush();
  END Close;

(****************************************************************)

(* Returns a fixed-width FBE.T that writes to the underlying Wr.T. *)
PROCEDURE New (wr: Wr.T; width: INTEGER): FBE.T =
  VAR o := NEW(T);
  BEGIN
    o.wr := wr;
    o.width := width;
    RETURN o;
  END New;

BEGIN
END FBEWr.
