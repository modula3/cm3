(* --------------------------------------------------------------------- *)
(* 07-JUN-96  JK  Modified ParseAlg to replace @number tags with spaces. *)
(* --------------------------------------------------------------------- *)
(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Fri Jun 28 00:44:29 PDT 1996 by mhb    *)
(*      modified on Wed Feb 23 08:08:08 PST 1994 by kalsow *)
(*      modified on Wed Jun 23 14:24:07 PDT 1993 by steveg *)
(*      modified on Tue Jan  5 20:04:15 PST 1993 by johnh *)
(*      modified on Wed Aug 19 16:34:43 PDT 1992 by sclafani*)
<*PRAGMA LL*>

MODULE CodeView;

IMPORT Axis, BorderedVBT, ASCII, ColorName, Fmt, FloatMode, Font, IntRef,
       IntRefSort, IntRefTbl, Lex, RefList, PaintOp, Pixmap, Point, Rd,
       RefListUtils, Rect, Split, Stdio, TextPort, TextRefTbl, Text,
       TextRef, TextRefSort, TextRd, TextWr, TextureVBT, Thread, VBT,
       VText, VTDef, Wr, ZSplit;

<* FATAL Rd.Failure, Wr.Failure, Thread.Alerted, Rd.EndOfFile *>
<* FATAL Lex.Error, FloatMode.Trap *>
<* FATAL VTDef.Error, Split.NotAChild *>

TYPE
  ProcInfo = REF RECORD
                   source : TEXT;
                   offsets: IntRefTbl.T;
                 END;
  Position = REF RECORD start, end: CARDINAL;  END;

REVEAL
  T = Public BRANDED OBJECT
        procTable   : TextRefTbl.T;
        font        : Font.T;
        delta       : CARDINAL;
      OVERRIDES
        shape       := ZShape;
        enter       := Enter;
        exit        := Exit;
        at          := At;
        event       := Event;
        exitAll     := ExitAll;
        listNames   := ListNames;
        listRegions := ListRegions;
        init        := Init;
      END;

TYPE
  AlgVBT = TextPort.T OBJECT
             interval: VText.Interval;
             proc    : ProcInfo;
           OVERRIDES
             shape := Shape;
           END;

<* FATAL ColorName.NotFound *>
VAR
  replaceTags: BOOLEAN := FALSE;       (* JK, 07-JUN-96 *)
	
  highlightStyle := VText.MakeIntervalOptions (
                      VText.IntervalStyle.InverseStyle,
                      PaintOp.MakeColorScheme (
                        PaintOp.Fg, PaintOp.FromRGB (
                                        ColorName.ToRGB ("LightGreen").r,
                                        ColorName.ToRGB ("LightGreen").g,
                                        ColorName.ToRGB ("LightGreen").b)),
                      PaintOp.bgFg, PaintOp.Bg);



PROCEDURE ZShape (v: VBT.T; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  VAR res := ZSplit.T.shape (v, ax, n);
  BEGIN
    IF res.pref < 100 THEN
      IF ax = Axis.T.Ver THEN res.pref := 200 ELSE res.pref := 400 END;
      IF res.pref >= res.hi THEN res.hi := res.pref + 1; END;
    END;
    RETURN res;
  END ZShape;

PROCEDURE Shape (<*UNUSED*> v : VBT.T;
                 <*UNUSED*> ax: Axis.T;
                 <*UNUSED*> n : CARDINAL): VBT.SizeRange =
  VAR res: VBT.SizeRange;
  BEGIN
    res.pref := 2000;
    res.lo := res.pref;
    res.hi := res.lo + 1;
    RETURN res;
  END Shape;

PROCEDURE Enter (t: T; procedureName: TEXT; pauseTime := -1) =
  VAR
    algVBT: AlgVBT;
    point : Point.T;
    depth : INTEGER;
    pos   : Position;
    refany: REFANY;
  BEGIN
    IF NOT t.procTable.get (procedureName, refany) THEN RETURN; END;
    algVBT := NewAlgVBT (t, refany);
    depth := Split.NumChildren (t) - 1;
    point := Point.Add (
               Rect.NorthWest (ZSplit.GetParentDomain (t)),
               Point.FromCoords (t.delta * depth, t.delta * depth));
    ZSplit.InsertAt (t, BorderedVBT.New (algVBT, 0.5), point);
    IF algVBT.proc.offsets.get (0, refany) THEN
      pos := refany;
      VText.MoveInterval (algVBT.interval, pos.start, pos.end);
      VBT.Mark (algVBT);
      IF pauseTime < 0 THEN pauseTime := t.pauseTime; END;
      Thread.Pause (FLOAT(pauseTime, LONGREAL));
    END;
  END Enter;

PROCEDURE Exit (t: T; pauseTime := -1) =
  BEGIN
    IF Split.NumChildren (t) < 2 THEN RETURN; END;
    Split.Delete (t, Split.Succ (t, NIL));
    IF pauseTime < 0 THEN pauseTime := t.pauseTime; END;
    Thread.Pause (FLOAT(pauseTime, LONGREAL));
  END Exit;

PROCEDURE At (t: T; highlight: CARDINAL; pauseTime := -1) =
  VAR
    algVBT: AlgVBT;
    pos   : Position;
    refany: REFANY;
  BEGIN
    IF Split.NumChildren (t) < 2 THEN RETURN; END;
    algVBT := Split.Succ (Split.Succ (t, NIL), NIL);
    IF algVBT.proc.offsets.get (highlight, refany) THEN
      pos := refany;
      VText.MoveInterval (algVBT.interval, pos.start, pos.end);
      VBT.Mark (algVBT);
      IF pauseTime < 0 THEN pauseTime := t.pauseTime; END;
      Thread.Pause (FLOAT(pauseTime, LONGREAL));
    END;
  END At;

PROCEDURE Event (t            : T;
                 highlight           := 0;
                 pauseTime           := -1;
                 procedureName: TEXT := NIL ) =
  BEGIN
    IF procedureName # NIL THEN
      t.enter (procedureName, pauseTime);
    ELSIF highlight < 0 THEN
      t.exit (pauseTime);
    ELSE
      t.at (highlight, pauseTime);
    END;
  END Event;

PROCEDURE ExitAll (t: T) =
  VAR
    bg := Split.Pred (t, NIL);
    ch := Split.Pred (t, bg);
  BEGIN
    WHILE ch # NIL DO Split.Delete (t, ch); ch := Split.Pred (t, bg); END;
  END ExitAll;

PROCEDURE NewAlgVBT (t: T; proc: ProcInfo): AlgVBT =
  VAR
    vbt: AlgVBT;
    vt : VText.T;
  BEGIN
    vbt := NEW (AlgVBT).init (wrap := FALSE, font := t.font);
    TextPort.SetText (vbt, proc.source);
    vbt.setReadOnly(TRUE);  (* replaces TextPort.SetReadOnly (vbt, TRUE);*)
    vt := TextPort.GetVText (vbt);
    vbt.interval := VText.CreateInterval (vt, 0, 0, highlightStyle);
    VText.SwitchInterval (vbt.interval, VText.OnOffState.On);
    vbt.proc := proc;
    RETURN vbt;
  END NewAlgVBT;

PROCEDURE Dump (source: Rd.T; wr: Wr.T; errorWr: Wr.T := NIL) =
  VAR
    procList: RefList.T;
    assoc   : RefList.T;
    name    : TEXT;
    proc    : ProcInfo;
    posList : RefList.T;
    pos     : Position;
    line    : REF INTEGER;
  BEGIN
    procList := SortTextRefTbl(ParseAlg (source, errorWr));
    WHILE procList # NIL DO
      assoc := RefListUtils.Pop (procList);
      name := RefListUtils.Pop (assoc);
      proc := RefListUtils.Pop (assoc);
      Wr.PutText (wr, name & "\n");
      posList := SortIntRefTbl(proc.offsets);
      WHILE posList # NIL DO
        assoc := RefListUtils.Pop (posList);
        line := RefListUtils.Pop (assoc);
        pos := RefListUtils.Pop (assoc);
        Wr.PutText (wr, Fmt.F ("%5s  %s\n", Fmt.Int (line^),
                               Text.Sub (proc.source, pos.start,
                                         pos.end - pos.start)));
      END;
      Wr.PutChar (wr, '\n');
    END;
  END Dump;

PROCEDURE ParseAlg (rd: Rd.T; errorWr: Wr.T): TextRefTbl.T =
  TYPE
    State = {Top, TopAt, TopTag, InProc, ProcAt, ProcTag, StatTag, InStat,
             StatAt};
  VAR
    procTable           := NEW(TextRefTbl.Default).init();
    procWr              := TextWr.New ();
    tagWr               := TextWr.New ();
    state               := State.Top;
    c        : CHAR;
    name     : TEXT;
    tag      : TEXT;
    id       : CARDINAL;
    any      : REFANY;
    proc     : ProcInfo;
    pos      : Position;
  BEGIN
    IF errorWr = NIL THEN errorWr := Stdio.stderr; END;
    WHILE NOT Rd.EOF (rd) DO
      c := Rd.GetChar (rd);
      CASE state OF
      | State.Top => IF c = '@' THEN state := State.TopAt; END;
      | State.TopAt =>
          IF c IN ASCII.AlphaNumerics THEN
            Wr.PutChar (tagWr, c);
            state := State.TopTag;
          ELSE
            state := State.Top;
          END;
      | State.TopTag =>
          IF c IN ASCII.Punctuation + ASCII.Spaces THEN
            name := TextWr.ToText (tagWr);
            proc := NEW (ProcInfo);
            proc.offsets := NEW(IntRefTbl.Default).init(4);
            pos := NEW (Position);
            tag := "0";
            id := 0;
            pos.start := Wr.Index (procWr);
            state := State.InStat;
          ELSE
            Wr.PutChar (tagWr, c);
          END;
      | State.InProc =>
          IF c = '@' THEN
            IF replaceTags THEN
              pos := NEW (Position);            (* JK, 11-JUN-96 *)
              pos.start := Wr.Index (procWr);   (* JK, 11-JUN-96 *)
              Wr.PutChar (procWr, ' ');	        (* JK, 07-JUN-96 *)
            END;
            state := State.ProcAt;
          ELSE
            Wr.PutChar (procWr, c);
          END;
      | State.ProcAt =>
          IF c IN ASCII.Letters THEN
            Wr.PutChar (tagWr, c);
            state := State.ProcTag;
          ELSIF c IN ASCII.Digits THEN
            Wr.PutChar (tagWr, c);
            IF replaceTags THEN
              Wr.PutChar (procWr, ' ');	        (* JK, 07-JUN-96 *)
            END;
            state := State.StatTag;
          ELSE
            state := State.InProc;
          END;
      | State.ProcTag =>
          IF c IN ASCII.Punctuation + ASCII.Spaces THEN
            tag := TextWr.ToText (tagWr);
            IF NOT Text.Equal (tag, name) THEN
              Wr.PutText (
                errorWr,
                Fmt.F (
                  "procedure trailer for '%s' does not match header\n",
                  name));
            END;
            proc.source := TextWr.ToText (procWr);
            EVAL procTable.put (name, proc);
            state := State.Top;
          ELSE
            Wr.PutChar (tagWr, c);
          END;
      | State.StatTag =>
          IF c IN ASCII.Digits THEN
            IF replaceTags THEN
              Wr.PutChar (procWr, ' ');         (* JK, 07-JUN-96 *)
            END;
            Wr.PutChar (tagWr, c);
          ELSE
            IF replaceTags THEN
              Wr.PutChar (procWr, ' ');         (* JK, 07-JUN-96 *)
            END;
            tag := TextWr.ToText (tagWr);
            id := Lex.Int (TextRd.New (tag));
            IF proc.offsets.get (id, any) THEN
              Wr.PutText (
                errorWr,
                Fmt.F (
                  "duplicate statement tag '@%s' at offsets %s and %s\n",
                  tag, Fmt.Int (pos.start), Fmt.Int (Rd.Index (rd))));
            END;
            IF NOT replaceTags THEN
              pos := NEW (Position);             (* JK, 11-JUN-96 *)
              pos.start := Wr.Index (procWr);    (* JK, 11-JUN-96 *)
            END;
            state := State.InStat;
          END;
      | State.InStat =>
          IF c = '@' THEN
            state := State.StatAt;
          ELSE
            Wr.PutChar (procWr, c);
          END;
      | State.StatAt =>
          IF c = '@' THEN
            Wr.PutChar (procWr, c);
            state := State.InStat;
          ELSE
            pos.end := Wr.Index (procWr);
            EVAL proc.offsets.put (id, pos);
            Wr.PutChar (procWr, c);
            state := State.InProc;
          END;
      END;
    END;

    CASE state OF
    | State.TopTag =>
        Wr.PutText (
          errorWr, "unterminated procedure header (@name) at end-of-file\n");
    | State.InProc, State.ProcAt =>
        Wr.PutText (errorWr,
                    Fmt.F (
                      "unmatched procedure header (@%s) at end-of-file\n",
                      name));
    | State.ProcTag =>
        Wr.PutText (
          errorWr,
          Fmt.F (
            "unterminated procedure trailer for '%s' at end-of-file\n",
            name));
    | State.StatTag =>
        Wr.PutText (
          errorWr,
          Fmt.F ("unterminated statement tag for '%s' at end-of-file\n",
                 name));
    | State.InStat =>
        Wr.PutText (
          errorWr,
          Fmt.F ("unterminated statement marker ('@%s') at end-of-file\n",
                 tag));
        Wr.PutText (errorWr,
                    Fmt.F (
                      "unmatched procedure header (@%s) at end-of-file\n",
                      name));
    | State.StatAt =>
        pos.end := Wr.Index (procWr);
        EVAL proc.offsets.put (Lex.Int (TextRd.New (tag)), pos);
        Wr.PutText (errorWr,
                    Fmt.F (
                      "unmatched procedure header (@%s) at end-of-file\n",
                      name));
    ELSE
    END;
    Wr.Flush (errorWr);
    RETURN procTable;
  END ParseAlg;

PROCEDURE ListNames (t: T): RefList.T =
  VAR
    iter            := t.procTable.iterate();
    k   : TEXT;
    val : REFANY;
    res : RefList.T := NIL;
  BEGIN
    WHILE iter.next(k, val) DO res := RefList.Cons(k, res); END;
    RETURN res
  END ListNames;

PROCEDURE ListRegions (t: T; procedureName: TEXT): RefList.T =
  VAR
    refany: REFANY;
    proc  : ProcInfo;
    k     : INTEGER;
    ri    : REF INTEGER;
    val   : REFANY;
    res   : RefList.T   := NIL;
  BEGIN
    IF t.procTable.get(procedureName, refany) THEN
      proc := refany;
      WITH iter = proc.offsets.iterate() DO
        WHILE iter.next(k, val) DO
          ri := NEW(REF INTEGER);
          ri^ := k;
          res := RefList.Cons(ri, res);
        END;
      END;
      RETURN res
    ELSE
      RETURN NIL;
    END;
  END ListRegions;

PROCEDURE Init (         t          : T;
                         source     : Rd.T;
                         errorWr    : Wr.T     := NIL;
                READONLY fontName              := DefaultFont;
                         paneOffset : CARDINAL := 20;
                         background : VBT.T    := NIL ): T =
  BEGIN
    IF background = NIL THEN
      background :=
        BorderedVBT.New(TextureVBT.New(txt := Pixmap.Gray), 0.5);
    END;
    EVAL ZSplit.T.init(t, background);
    t.procTable := ParseAlg(source, errorWr);
    t.font := Font.FromName(fontName);
    t.delta := paneOffset;
    RETURN t;
  END Init;

PROCEDURE New (         source     : Rd.T;
                        errorWr    : Wr.T     := NIL;
               READONLY fontName              := DefaultFont;
                        paneOffset : CARDINAL := 20;
                        background : VBT.T    := NIL ): T =
  BEGIN
    RETURN Init(NEW(T), source, errorWr, fontName, paneOffset,
                background);
  END New;

PROCEDURE SortTextRefTbl(tbl: TextRefTbl.T): RefList.T =
  VAR arr  := NEW(REF ARRAY OF TextRef.T, tbl.size());
      iter := tbl.iterate();
      k: TEXT;
      val: REFANY;
      res: RefList.T := NIL;
  BEGIN
    FOR i := 0 TO LAST(arr^) DO
      EVAL iter.next(k, val);
      arr[i] := TextRef.T{k, val};
    END;
    TextRefSort.Sort(arr^);
    FOR i := 0 TO LAST(arr^) DO
      res := RefList.Cons(RefList.List2(arr[i].key, arr[i].value), res);
    END;
    res := RefList.ReverseD(res);
    RETURN res;
  END SortTextRefTbl;

PROCEDURE SortIntRefTbl (tbl: IntRefTbl.T): RefList.T =
  VAR
    arr               := NEW(REF ARRAY OF IntRef.T, tbl.size());
    iter              := tbl.iterate();
    k   : INTEGER;
    ri  : REF INTEGER;
    val : REFANY;
    res : RefList.T   := NIL;
  BEGIN
    FOR i := 0 TO LAST(arr^) DO
      EVAL iter.next(k, val);
      arr[i] := IntRef.T{k, val};
    END;
    IntRefSort.Sort(arr^);
    FOR i := 0 TO LAST(arr^) DO
      ri := NEW(REF INTEGER);
      ri^ := arr[i].key;
      res := RefList.Cons(RefList.List2(ri, arr[i].value), res);
    END;
    res := RefList.ReverseD(res);
    RETURN res;
  END SortIntRefTbl;


PROCEDURE DoReplaceTags( replace: BOOLEAN := TRUE ) =
  BEGIN
    replaceTags := replace;
  END DoReplaceTags;


BEGIN
END CodeView.
