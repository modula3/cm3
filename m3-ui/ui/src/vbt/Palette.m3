(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Tue Jan 31 09:50:42 PST 1995 by kalsow   *)
(*      modified on Mon May 17 19:00:31 PDT 1993 by msm      *)
(*      modified on Tue Mar 10 19:05:26 1992 by steveg   *)
(*      modified on Mon Feb 24 13:57:29 PST 1992 by muller   *)
(*      modified on Tue Oct 22 22:45:40 PDT 1991 by gnelson  *)

<*PRAGMA LL*>

MODULE Palette;

IMPORT VBT, VBTRep, PaintOp, Font, Cursor, Pixmap, 
  ScrnPixmap, ScrnCursor, ScrnFont, ScrnPaintOp, Thread;

FROM PlttFrnds IMPORT con, noOp, noFont, noCursor, noPixmap;

VAR
  c := NEW(Thread.Condition);

PROCEDURE FromOpClosure (cl: OpClosure): PaintOp.T =
  VAR
    res: PaintOp.T;
  BEGIN
    LOCK con DO
      res.op := con.nextOp;
      INC(con.nextOp);
      IF con.ops = NIL OR LAST(con.ops^) < res.op THEN
        VAR new := NEW(REF ARRAY OF OpClosure, MAX(2 * res.op, 5));
        BEGIN
          IF con.ops # NIL THEN
            SUBARRAY(new^, 0, NUMBER(con.ops^)) := con.ops^
          END;
          con.ops := new
        END
      END;
      con.ops[res.op] := cl
    END;
    RETURN res
  END FromOpClosure;

PROCEDURE ExtendOps (st: VBT.ScreenType) =
  VAR osz, sz: CARDINAL;
  BEGIN
    IF con.ops # NIL THEN sz := NUMBER(con.ops^) ELSE sz := con.nextOp END;
    IF st.ops = NIL OR sz > NUMBER(st.ops^) THEN
      VAR new := NEW(REF ARRAY OF ScrnPaintOp.T, RoundUp(sz));
      BEGIN
        IF st.ops # NIL THEN
          osz := NUMBER(st.ops^);
          SUBARRAY(new^, 0, osz) := st.ops^
        ELSE
          osz := 0
        END;
        FOR i := osz TO LAST(new^) DO new[i] := NIL END;
        st.ops := new
      END
    END
  END ExtendOps;
    
PROCEDURE ResolveOp(st: VBT.ScreenType; pop: PaintOp.T): ScrnPaintOp.T =
  VAR res: ScrnPaintOp.T; cl: OpClosure := NIL; op := pop.op;
  BEGIN
    IF op < 0 THEN Crash() END;
    IF st.ops # NIL AND op < NUMBER(st.ops^) THEN
      res := st.ops[op];
      IF res # NIL AND res # noOp THEN RETURN res END
    END;
    LOCK con DO
      IF op >= con.nextOp THEN Crash() END;
      IF st.ops = NIL OR op > LAST(st.ops^) THEN ExtendOps(st) END;
      WHILE st.ops[op] = noOp DO Thread.Wait(con, c) END;
      res := st.ops[op];
      IF res # NIL THEN RETURN res END;
      st.ops[op] := noOp;
      IF op > LAST(PaintOp.Predefined) THEN cl := con.ops[op] END
    END;
    res := st.opApply(cl, pop);
    IF res = NIL THEN res := ResolveOp(st, PaintOp.Transparent) END;
    LOCK con DO st.ops[op] := res END;
    Thread.Broadcast(c);
    RETURN res
  END ResolveOp;
   
PROCEDURE FromFontClosure(cl: FontClosure): Font.T =
  VAR res: Font.T; BEGIN
    LOCK con DO
      res.fnt := con.nextFont;
      INC(con.nextFont);
      IF con.fonts = NIL OR LAST(con.fonts^) < res.fnt THEN
        VAR new := NEW(REF ARRAY OF FontClosure, MAX(2*res.fnt,5)); BEGIN
          IF con.fonts # NIL THEN
            SUBARRAY(new^, 0, NUMBER(con.fonts^)) := con.fonts^
          END;
          con.fonts := new
        END
      END;
      con.fonts[res.fnt] := cl
    END;
    RETURN res
  END FromFontClosure;

PROCEDURE ExtendFonts (st: VBT.ScreenType) =
  VAR osz, sz: CARDINAL;
  BEGIN
    IF con.fonts # NIL THEN
      sz := NUMBER(con.fonts^)
    ELSE
      sz := con.nextFont
    END;
    IF st.fonts = NIL OR sz > NUMBER(st.fonts^) THEN
      VAR new := NEW(REF ARRAY OF ScrnFont.T, RoundUp(sz));
      BEGIN
        IF st.fonts # NIL THEN
          osz := NUMBER(st.fonts^);
          SUBARRAY(new^, 0, osz) := st.fonts^
        ELSE
          osz := 0
        END;
        FOR i := osz TO LAST(new^) DO new[i] := NIL END;
        st.fonts := new
      END
    END
  END ExtendFonts;

PROCEDURE ResolveFont (st: VBT.ScreenType; pfont: Font.T): ScrnFont.T =
  VAR
    res : ScrnFont.T;
    cl  : FontClosure := NIL;
    font              := pfont.fnt;
  BEGIN
    IF font < 0 THEN Crash() END;
    IF st.fonts # NIL AND font < NUMBER(st.fonts^) THEN
      res := st.fonts[font];
      IF res # NIL AND res # noFont THEN RETURN res END
    END;
    LOCK con DO
      IF font >= con.nextFont THEN Crash() END;
      IF st.fonts = NIL OR font > LAST(st.fonts^) THEN ExtendFonts(st) END;
      WHILE st.fonts[font] = noFont DO Thread.Wait(con, c) END;
      res := st.fonts[font];
      IF res # NIL THEN RETURN res END;
      st.fonts[font] := noFont;
      IF font > LAST(Font.Predefined) THEN cl := con.fonts[font] END
    END;
    res := st.fontApply(cl, pfont);
    IF res = NIL THEN res := ResolveFont(st, Font.BuiltIn) END;
    LOCK con DO st.fonts[font] := res END;
    Thread.Broadcast(c);
    RETURN res
  END ResolveFont;

PROCEDURE FromPixmapClosure(cl: PixmapClosure): Pixmap.T =
  VAR res: Pixmap.T; BEGIN
    LOCK con DO
      res.pm := con.nextPixmap;
      INC(con.nextPixmap);
      IF con.pixmaps = NIL OR LAST(con.pixmaps^) < res.pm THEN
        VAR new := NEW(REF ARRAY OF PixmapClosure, MAX(2*res.pm,5)); BEGIN
          IF con.pixmaps # NIL THEN
            SUBARRAY(new^, 0, NUMBER(con.pixmaps^)) := con.pixmaps^
          END;
          con.pixmaps := new
        END
      END;
      con.pixmaps[res.pm] := cl
    END;
    RETURN res
  END FromPixmapClosure;

PROCEDURE ExtendPixmaps (st: VBT.ScreenType) =
  VAR osz, sz: CARDINAL;
  BEGIN
    IF con.pixmaps # NIL THEN
      sz := NUMBER(con.pixmaps^)
    ELSE
      sz := con.nextPixmap
    END;
    IF st.pixmaps = NIL OR sz > NUMBER(st.pixmaps^) THEN
      VAR new := NEW(REF ARRAY OF ScrnPixmap.T, RoundUp(sz));
      BEGIN
        IF st.pixmaps # NIL THEN
          osz := NUMBER(st.pixmaps^);
          SUBARRAY(new^, 0, osz) := st.pixmaps^
        ELSE
          osz := 0
        END;
        FOR i := osz TO LAST(new^) DO new[i] := NIL END;
        st.pixmaps := new
      END
    END
  END ExtendPixmaps;

PROCEDURE ResolvePixmap (st: VBT.ScreenType; pix: Pixmap.T): ScrnPixmap.T =
  VAR
    res   : ScrnPixmap.T;
    cl    : PixmapClosure := NIL;
    pixmap                := pix.pm;
  BEGIN
    IF pixmap < 0 THEN Crash() END;
    IF st.pixmaps # NIL AND pixmap < NUMBER(st.pixmaps^) THEN
      res := st.pixmaps[pixmap];
      IF res # NIL AND res # noPixmap THEN RETURN res END
    END;
    LOCK con DO
      IF pixmap >= con.nextPixmap THEN Crash() END;
      IF st.pixmaps = NIL OR pixmap > LAST(st.pixmaps^) THEN
        ExtendPixmaps(st)
      END;
      WHILE st.pixmaps[pixmap] = noPixmap DO Thread.Wait(con, c) END;
      res := st.pixmaps[pixmap];
      IF res # NIL THEN RETURN res END;
      st.pixmaps[pixmap] := noPixmap;
      IF pixmap > LAST(Pixmap.Predefined) THEN
        cl := con.pixmaps[pixmap]
      END
    END;
    res := st.pixmapApply(cl, pix);
    IF res = NIL THEN res := ResolvePixmap(st, Pixmap.Empty) END;
    LOCK con DO st.pixmaps[pixmap] := res END;
    Thread.Broadcast(c);
    RETURN res
  END ResolvePixmap;

PROCEDURE FromCursorClosure(cl: CursorClosure): Cursor.T =
  VAR res: Cursor.T; BEGIN
    LOCK con DO
      res.cs := con.nextCursor;
      INC(con.nextCursor);
      IF con.cursors = NIL OR LAST(con.cursors^) < res.cs THEN
        VAR new := NEW(REF ARRAY OF CursorClosure, MAX(2*res.cs,5)); BEGIN
          IF con.cursors # NIL THEN
            SUBARRAY(new^, 0, NUMBER(con.cursors^)) := con.cursors^
          END;
          con.cursors := new
        END
      END;
      con.cursors[res.cs] := cl
    END;
    RETURN res
  END FromCursorClosure;

PROCEDURE ExtendCursors (st: VBT.ScreenType) =
  VAR osz, sz: CARDINAL;
  BEGIN
    IF con.cursors # NIL THEN
      sz := NUMBER(con.cursors^)
    ELSE
      sz := con.nextCursor
    END;
    IF st.cursors = NIL OR sz > NUMBER(st.cursors^) THEN
      VAR new := NEW(REF ARRAY OF ScrnCursor.T, RoundUp(sz));
      BEGIN
        IF st.cursors # NIL THEN
          osz := NUMBER(st.cursors^);
          SUBARRAY(new^, 0, osz) := st.cursors^
        ELSE
          osz := 0
        END;
        FOR i := osz TO LAST(new^) DO new[i] := NIL END;
        st.cursors := new
      END
    END
  END ExtendCursors;

PROCEDURE ResolveCursor (st: VBT.ScreenType; curs: Cursor.T):
  ScrnCursor.T =
  VAR
    res   : ScrnCursor.T;
    cl    : CursorClosure := NIL;
    cursor                := curs.cs;
  BEGIN
    IF cursor < 0 THEN Crash() END;
    IF st.cursors # NIL AND cursor < NUMBER(st.cursors^) THEN
      res := st.cursors[cursor];
      IF res # NIL AND res # noCursor THEN RETURN res END
    END;
    LOCK con DO
      IF cursor >= con.nextCursor THEN Crash() END;
      IF st.cursors = NIL OR cursor > LAST(st.cursors^) THEN
        ExtendCursors(st)
      END;
      WHILE st.cursors[cursor] = noCursor DO Thread.Wait(con, c) END;
      res := st.cursors[cursor];
      IF res # NIL THEN RETURN res END;
      st.cursors[cursor] := noCursor;
      IF cursor > LAST(Cursor.Predefined) THEN
        cl := con.cursors[cursor]
      END
    END;
    res := st.cursorApply(cl, curs);
    IF res = NIL THEN res := ResolveCursor(st, Cursor.DontCare) END;
    LOCK con DO st.cursors[cursor] := res END;
    Thread.Broadcast(c);
    RETURN res
  END ResolveCursor;

PROCEDURE RoundUp(sz: CARDINAL): CARDINAL =
  VAR i := 1; BEGIN
    WHILE i < sz DO INC(i, i) END;
    RETURN i
  END RoundUp;

PROCEDURE Init(st: VBT.ScreenType) = 
  BEGIN
    IF st = NIL THEN RETURN END;
    FOR i := FIRST(Font.Predefined) TO LAST(Font.Predefined) DO
      EVAL ResolveFont(st, Font.T{i})
    END;
    FOR i := FIRST(Cursor.Predefined) TO LAST(Cursor.Predefined) DO
      EVAL ResolveCursor(st, Cursor.T{i})
    END;
    FOR i := FIRST(Pixmap.Predefined) TO LAST(Pixmap.Predefined) DO
      EVAL ResolvePixmap(st, Pixmap.T{i})
    END;
    FOR i := FIRST(PaintOp.Predefined) TO LAST(PaintOp.Predefined) DO
      EVAL ResolveOp(st, PaintOp.T{i})
    END;
  END Init;

EXCEPTION FatalError;

PROCEDURE Crash() =
  <*FATAL FatalError*>
  BEGIN
    RAISE FatalError
  END Crash;

BEGIN END Palette.
