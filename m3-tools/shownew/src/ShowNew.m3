(* Copyright (C) 1992, Digital Equipment Corporation       *)
(* All rights reserved.                                    *)
(* See the file COPYRIGHT for a full description.          *)
(*                                                         *)
(* Last modified on Tue Sep 13 15:15:21 PDT 1994 by kalsow *)
(*      modified on Fri Jan 29 19:07:08 PST 1993 by jdd    *)
(*      modified on Fri Jan 15 12:20:22 PST 1993 by mhb    *)
(*      modified on Thu Apr 23 18:57:36 PDT 1992 by muller *)

UNSAFE MODULE ShowNew EXPORTS Main;

IMPORT Stdio, Rd, Wr, Thread, Word, Text, (*ViewportVBT,*) Trestle;
IMPORT VBT, PaintOp, TrestleComm, Color, Region, Rect, Point;
IMPORT AnchorBtnVBT, TextVBT, BorderedVBT, MenuBtnVBT, Split, HVSplit;
IMPORT ButtonVBT, Axis, Font, VBTClass, Fmt, Process, HVBar;
IMPORT ZIO;

TYPE
  Vector = REF ARRAY OF INTEGER;

CONST
  ControlNames = ARRAY [0..9] OF TEXT {
    "start",
    NIL,
    "new object count",
    "new byte size",
    "total object count",
    "total byte size",
    "lap object count",
    "lap byte size",
    NIL,
    "quit"
  };

TYPE
  CompareProc = PROCEDURE (a, b: INTEGER): INTEGER;

CONST
  CompareProcs = ARRAY [0..5] OF CompareProc {
     CompareCnts,
     CompareBytes,
     CompareTotalCnts,
     CompareTotalBytes,
     CompareLapCnts,
     CompareLapBytes
  };

CONST
  Titles = ARRAY [0..5] OF TEXT {
    " new object counts ",
    " new byte sizes ",
    " total object counts ",
    " total byte sizes ",
    " lap object counts ",
    " lap byte sizes "
  };

(*-------------------------------------------------------------- raw data ---*)

VAR
  n_types     : INTEGER := 0;
  type_names  : REF ARRAY OF TEXT;
  type_sizes  : Vector;
  cur_cnts    : Vector;
  cur_bytes   : Vector;
  total_cnts  : Vector;
  total_bytes : Vector;
  lap_cnts    : Vector;
  lap_bytes   : Vector;
  sort_map    : Vector;
  root        : VBT.T;
  title       : TextVBT.T;
  lap_title   : TextVBT.T;
  display     : BarGraphVBT;
  max_value   : INTEGER := 10;
  disp_stat   : INTEGER := 0;
  compare     : CompareProc := CompareCnts;
  colors      : REF ARRAY OF PaintOp.T;
  started     : BOOLEAN := FALSE;
  lap_running : BOOLEAN := FALSE;

(*--------------------------------------------- initial type descriptions ---*)

PROCEDURE GetTypes () =
  <*FATAL Rd.EndOfFile*>
  VAR n: INTEGER;
  BEGIN
    n := ZIO.GetInt ();
    type_names  := NEW (REF ARRAY OF TEXT, n);
    type_sizes  := NewVec (n);
    cur_cnts    := NewVec (n);
    cur_bytes   := NewVec (n);
    total_cnts  := NewVec (n);
    total_bytes := NewVec (n);
    lap_cnts    := NewVec (n);
    lap_bytes   := NewVec (n);
    sort_map    := NewVec (n);

    FOR i := 0 TO n-1 DO
      type_sizes[i] := ZIO.GetInt ();
      type_names[i] := FixName (ZIO.GetText ());
    END;

    FOR i := 0 TO n-1 DO sort_map[i] := i; END;

    n_types := n;
    CreateColors ();
  END GetTypes;

PROCEDURE FixName (t: TEXT): TEXT =
  VAR i: INTEGER;  len := Text.Length (t);
  BEGIN
    IF len > 3
      AND Text.GetChar (t, 0) = 'T'
      AND Text.GetChar (t, 1) = 'C'
      AND Text.GetChar (t, 2) = '=' THEN
      i := 3;
      WHILE (i < len) AND (Text.GetChar (t, i) # ' ') DO INC (i) END;
      IF (i < len) THEN t := Text.Sub (t, i+1); END;
    END;
    RETURN t;
  END FixName;

(*-------------------------------------------------------- build the VBTs ---*)

PROCEDURE SetupVBT () =
  <*FATAL TrestleComm.Failure*>
  VAR control, start, menu_bar, viewport: VBT.T;
  BEGIN
    control   := BuildMenu ("control...", ControlNames, SetControl);
    lap_title := TextVBT.New ("lap start");
    start     := BorderedVBT.New (MenuBtnVBT.New (lap_title, StartPress, NIL));
    title     := TextVBT.New (Titles[disp_stat]);
    menu_bar  := ButtonVBT.MenuBar (control, start, title);

    display := NEW (BarGraphVBT);

(*
    viewport := NEW (ViewportVBT.T).init (display,
                     scrollStyle := ViewportVBT.ScrollStyle.Auto);
    viewport := BorderedVBT.New (display);
*)
    viewport := display;

    root := HVSplit.Cons (Axis.T.Ver, menu_bar, viewport);

    (* VBT.NewShape (display); *)
    Trestle.Install (root);
  END SetupVBT;

(*----------------------------------------------------------------- menus ---*)

TYPE
  MenuProc = PROCEDURE (i: INTEGER);
  MenuClosure = REF RECORD
    id   : INTEGER;
    tag  : TEXT;
    proc : MenuProc;
  END;

PROCEDURE BuildMenu (name: TEXT;  READONLY tags: ARRAY OF TEXT;  p: MenuProc): VBT.T =
  VAR menu: VBT.T;  m: MenuClosure;  id := 0;
  BEGIN
    menu := HVSplit.New (Axis.T.Ver);
    FOR i := 0 TO LAST (tags) DO
      IF tags[i] # NIL THEN
        m := NEW (MenuClosure, id := id,  tag := tags[i], proc := p); INC (id);
        Split.AddChild (menu, MenuBtnVBT.TextItem (tags[i], MenuPress, m));
      ELSE
        Split.AddChild (menu, HVBar.New (size := 1.0));
      END;
    END;
    RETURN BorderedVBT.New (
              AnchorBtnVBT.New (
                TextVBT.New (name),
                BorderedVBT.New (menu),
                99999));
  END BuildMenu;

PROCEDURE MenuPress (v: ButtonVBT.T;  <*UNUSED*>READONLY cd: VBT.MouseRec) =
  VAR m: MenuClosure := VBT.GetProp (v, TYPECODE (MenuClosure));
  BEGIN
    m.proc (m.id);
  END MenuPress;

PROCEDURE StartPress (<*UNUSED*> v: ButtonVBT.T;
                      <*UNUSED*>READONLY cd: VBT.MouseRec) =
  BEGIN
    IF NOT started THEN RETURN END;
    LOCK display DO
      IF (lap_running) THEN
        (* capture the final lap values *)
        FOR i := 0 TO n_types-1 DO
          lap_cnts[i]  := total_cnts[i]  - lap_cnts[i];
          lap_bytes[i] := total_bytes[i] - lap_bytes[i];
        END;
        lap_running := FALSE;
        TextVBT.Put (lap_title, "lap start");
      ELSE
        (* capture the initial lap values *)
        lap_cnts^  := total_cnts^;
        lap_bytes^ := total_bytes^;
        lap_running := TRUE;
        TextVBT.Put (lap_title, "lap stop");
      END;
    END;
    lap_title.redisplay ();
    IF (disp_stat = 4) OR (disp_stat = 5) THEN display.redisplay (); END;
  END StartPress;

PROCEDURE SetControl (i: INTEGER) =
  BEGIN
    IF (i = 0) THEN
      Go ();
    ELSIF (i <= 6) THEN
      disp_stat := i - 1;
      compare := CompareProcs [disp_stat];
      TextVBT.Put (title, Titles[disp_stat]);
      UpdateMax ();
      Sort ();
      display.redisplay ();
    ELSE
      Process.Exit (0);
    END;
  END SetControl;

(*---------------------------------------------------------------- colors ---*)

PROCEDURE CreateColors () =
  VAR j: INTEGER := 0;
  BEGIN
    colors := NEW (REF ARRAY OF PaintOp.T, n_types);
    FOR i := 0 TO n_types-1 BY 2 DO
      colors[i] := NewColor (j);  INC (j);
    END;
    FOR i := 1 TO n_types-1 BY 2 DO
      colors[i] := NewColor (j);  INC (j);
    END;
  END CreateColors;

PROCEDURE NewColor (i: INTEGER): PaintOp.T =
  VAR rgb: Color.T;  hsv: Color.HSV;
  BEGIN
    hsv.h := FLOAT (i) / FLOAT (n_types);
    hsv.s := 1.0;
    hsv.v := 1.0;
    rgb := Color.FromHSV (hsv);
    RETURN PaintOp.FromRGB (rgb.r, rgb.g, rgb.b);
  END NewColor;

(*------------------------------------------------------------- bar graph ---*)

CONST
  Vertical_gap   = 15;  (* pixel height of the bars *)
  Vertical_base  = 17;  (* veritcal offset of the first row *)
  Tag_base       = 5;   (* horizontal offset of the labels *)
  Bar_base       = 150; (* horizontal offset of the bottom of the bars *)
  Num_width      = 25;  (* pixels to leave for the numbers *)
  Min_bar_length = 150;
VAR
  bar_length   := 250; (* length in pixels of a full bar *)

TYPE
  BarGraphVBT = VBT.Leaf OBJECT
    rect: Rect.T;
  OVERRIDES
    repaint := RepaintBarGraph;
    reshape := ReshapeBarGraph;
    shape   := ShapeBarGraph;
  END;

PROCEDURE ShapeBarGraph (<*UNUSED*> self: BarGraphVBT;
                                   ax  : Axis.T;
                        <*UNUSED*> n   : CARDINAL    ): VBT.SizeRange =
  VAR sz: INTEGER;
  BEGIN
    IF (ax = Axis.T.Hor) THEN
      sz := 2 * Tag_base + Bar_base + Min_bar_length + Num_width;
    ELSE
      sz := Vertical_base + 4 * Vertical_gap;
    END;
    RETURN VBT.SizeRange {lo := sz, pref := sz, hi := 100000};
  END ShapeBarGraph;

PROCEDURE ReshapeBarGraph (self: BarGraphVBT; READONLY cd: VBT.ReshapeRec) =
  CONST Used = Bar_base + 2 * Tag_base + Num_width;
  BEGIN
    self.rect := cd.new;
    bar_length := (cd.new.east - cd.new.west) - Used;
    RepaintBarGraph (self, Region.T{r := cd.new});
  END ReshapeBarGraph;

PROCEDURE RepaintBarGraph (self: BarGraphVBT;
       <*UNUSED*> READONLY rgn : Region.T    ) =
  VAR
    x, y: INTEGER;
    name: TEXT;
    value: INTEGER;
  BEGIN
    (** VBT.PaintTint (self, self.rect, PaintOp.Bg); **)
    EraseBar (-2);  y := -2;

    (* repaint the ruler *)
    EraseBar (-1);  y := -1;
    PaintRuler ();

    (* repaint each of the bars *)
    FOR i := 0 TO n_types-1 DO
      EraseBar (i); y := i;
      IF (VPos (i) >= self.rect.south) THEN EXIT; END;
      x := sort_map [i];
      name := type_names[x];
      value := GetStat (x);
      IF (value > 0) THEN
        VBT.PaintTint (self, GetBar (i, value), colors[x]);
        VBT.PaintText (self, Rect.Full, TagBase (i), Font.BuiltIn, name);
        VBT.PaintText (self, Rect.Full, NumBase (name, value, i),
                       Font.BuiltIn, Fmt.Int (value));
      END;
    END;

    (* finish erasing *)
    WHILE (VPos (y) <= self.rect.south) DO
      EraseBar (y);  INC (y);
    END;
  END RepaintBarGraph;

PROCEDURE EraseBar (ver: INTEGER) =
  VAR r: Rect.T;
  BEGIN
    r.west  := display.rect.west;
    r.east  := display.rect.east;
    r.north := VPos (ver);
    r.south := r.north + Vertical_gap;
    VBT.PaintTint (display, r, PaintOp.Bg);
  END EraseBar;

PROCEDURE GetBar (ver, value: INTEGER): Rect.T =
  VAR r: Rect.T;
  BEGIN
    r.north := VPos (ver);
    r.south := r.north + Vertical_gap;
    r.west  := display.rect.west  + Bar_base; (*== HPos(0) *)
    r.east  := HPos (value);
    RETURN r;
  END GetBar;

PROCEDURE TagBase (ver: INTEGER): Point.T =
  VAR p: Point.T;
  BEGIN
    p.h := display.rect.west + Tag_base;
    p.v := VPos (ver+1) - 1;
    RETURN p;
  END TagBase;

PROCEDURE NumBase (name: TEXT;  value, ver: INTEGER): Point.T =
  VAR p := TagBase (ver);
  BEGIN
    p.h := MAX (p.h + TWidth (name) + 6, HPos (value) + 2);
    RETURN p;
  END NumBase;

PROCEDURE VPos (row: INTEGER): INTEGER =
  BEGIN
    RETURN display.rect.north + Vertical_base + Vertical_gap * row;
  END VPos;

PROCEDURE HPos (value: INTEGER): INTEGER =
  VAR len := FLOAT(value) / FLOAT (max_value) * FLOAT(bar_length);
  BEGIN
    RETURN display.rect.west + Bar_base + ROUND (len);
  END HPos;

PROCEDURE PaintRuler () =
  VAR pt: Point.T;  n, wid: INTEGER;  txt: TEXT;
  BEGIN
    pt.v := VPos (0) - 1;
    pt.h := HPos (0);
    VBT.PaintText (display, Rect.Full, pt, Font.BuiltIn, "0");

    n    := max_value DIV 2;
    txt  := Fmt.Int (n);
    wid  := TWidth (txt);
    pt.h := HPos (n) - wid DIV 2;
    VBT.PaintText (display, Rect.Full, pt, Font.BuiltIn, txt);

    n    := max_value;
    txt  := Fmt.Int (n);
    wid  := TWidth (txt);
    pt.h := HPos (n) - wid;
    VBT.PaintText (display, Rect.Full, pt, Font.BuiltIn, Fmt.Int (max_value));
  END PaintRuler;

PROCEDURE TWidth (txt: TEXT): INTEGER =
  BEGIN
    RETURN VBT.TextWidth (display, txt, Font.BuiltIn);
  END TWidth;

(*---------------------------------------------------------------------------*)

PROCEDURE Run () =
  VAR n_elts, n, cnt, size: INTEGER;  cnts, bytes: Vector;
  BEGIN
    TRY
      LOOP
        cnts := NewVec (n_types);
        bytes := NewVec (n_types);

        (* read the sample *)
        n_elts := ZIO.GetInt ();
        FOR i := 0 TO n_elts-1 DO
          n    := ZIO.GetInt ();
          cnt  := ZIO.GetInt ();
          size := type_sizes [n];
          IF size >= 0
            THEN size := size * cnt;
            ELSE size := ZIO.GetInt ();
          END;
          cnts [n]  := cnt;
          bytes [n] := size;
        END;

        (* update the globals *)
        LOCK display DO
          cur_cnts  := cnts;
          cur_bytes := bytes;
          FOR i := 0 TO n_types-1 DO
            total_cnts[i]  := Word.Plus (total_cnts[i], cnts[i]);
            total_bytes[i] := Word.Plus (total_bytes[i], bytes[i]);
          END;
          UpdateMax ();
          Sort ();
        END;

        display.redisplay ();
      END;
    EXCEPT
    | Rd.EndOfFile =>
    END;
  END Run;

PROCEDURE GetStat (i: INTEGER): INTEGER =
  VAR v: INTEGER;
  BEGIN
    IF    (disp_stat = 0) THEN v := cur_cnts[i];
    ELSIF (disp_stat = 1) THEN v := cur_bytes[i];
    ELSIF (disp_stat = 2) THEN v := total_cnts[i];
    ELSIF (disp_stat = 3) THEN v := total_bytes[i];
    ELSIF (disp_stat = 4) THEN
      IF lap_running
        THEN v := total_cnts[i] - lap_cnts[i];
        ELSE v := lap_cnts[i];
      END;
    ELSE (*disp_stat = 5*)
      IF lap_running
        THEN v := total_bytes[i] - lap_bytes[i];
        ELSE v := lap_bytes[i];
      END;
    END;
    RETURN v;
  END GetStat;

PROCEDURE UpdateMax () =
  VAR max := 0;
  BEGIN
    max := 0;
    FOR i := 0 TO n_types-1 DO max := MAX (max, GetStat (i)); END;

    IF (max > max_value) THEN
      (* make it bigger *)
      max_value := (3 * max + 1) DIV 2;
    ELSIF (3 * max  < 2 * max_value) AND (max > 5) THEN
      (* make is smaller *)
      max_value := MAX (max, 5);
    END;
  END UpdateMax;

(*--------------------------------------------------------------- sorting ---*)

PROCEDURE Sort () =
  BEGIN
    DoSort (compare);
  END Sort;

(*********
PROCEDURE CompareTypecodes (a, b: INTEGER): INTEGER =
  BEGIN
    RETURN a - b;
  END CompareTypecodes;

PROCEDURE CompareName (a, b: INTEGER): INTEGER =
  BEGIN
    RETURN Text.Compare (type_names[a], type_names[b]);
  END CompareName;
**********)

PROCEDURE CompareCnts (a, b: INTEGER): INTEGER =
  VAR aa := cur_cnts [a];
  VAR bb := cur_cnts [b];
  VAR x  := bb - aa;
  BEGIN
    IF (x = 0) AND (aa > 0) THEN
      x := Text.Compare (type_names[a], type_names[b]);
    END;
    RETURN x;
  END CompareCnts;

PROCEDURE CompareBytes (a, b: INTEGER): INTEGER =
  VAR aa := cur_bytes [a];
  VAR bb := cur_bytes [b];
  VAR x  := bb - aa;
  BEGIN
    IF (x = 0) AND (aa > 0) THEN
      x := Text.Compare (type_names[a], type_names[b]);
    END;
    RETURN x;
  END CompareBytes;

PROCEDURE CompareTotalCnts (a, b: INTEGER): INTEGER =
  VAR aa := total_cnts [a];
  VAR bb := total_cnts [b];
  VAR x  := bb - aa;
  BEGIN
    IF (x = 0) AND (aa > 0) THEN
      x := Text.Compare (type_names[a], type_names[b]);
    END;
    RETURN x;
  END CompareTotalCnts;

PROCEDURE CompareTotalBytes (a, b: INTEGER): INTEGER =
  VAR aa := total_bytes [a];
  VAR bb := total_bytes [b];
  VAR x  := bb - aa;
  BEGIN
    IF (x = 0) AND (aa > 0) THEN
      x := Text.Compare (type_names[a], type_names[b]);
    END;
    RETURN x;
  END CompareTotalBytes;

PROCEDURE CompareLapCnts (a, b: INTEGER): INTEGER =
  VAR aa := lap_cnts [a];
  VAR bb := lap_cnts [b];
  VAR x  := bb - aa;
  BEGIN
    IF lap_running THEN
      aa := total_cnts[a] - aa;
      bb := total_cnts[b] - bb;
      x  := bb - aa;
    END;
    IF (x = 0) AND (aa > 0) THEN
      x := Text.Compare (type_names[a], type_names[b]);
    END;
    RETURN x;
  END CompareLapCnts;

PROCEDURE CompareLapBytes (a, b: INTEGER): INTEGER =
  VAR aa := lap_bytes [a];
  VAR bb := lap_bytes [b];
  VAR x  := bb - aa;
  BEGIN
    IF lap_running THEN
      aa := total_bytes[a] - aa;
      bb := total_bytes[b] - bb;
      x  := bb - aa;
    END;
    IF (x = 0) AND (aa > 0) THEN
      x := Text.Compare (type_names[a], type_names[b]);
    END;
    RETURN x;
  END CompareLapBytes;

PROCEDURE DoSort (cmp: CompareProc) =
  BEGIN
    IF (sort_map = NIL) THEN RETURN END;
    QuickSort (sort_map^, 0, NUMBER (sort_map^), cmp);
    InsertionSort (sort_map^, 0, NUMBER (sort_map^), cmp);
  END DoSort;

PROCEDURE QuickSort (VAR a: ARRAY OF INTEGER;  lo, hi: INTEGER;
                     cmp: CompareProc) =
  CONST CutOff = 9;
  VAR i, j: INTEGER;  key, tmp: INTEGER;
  BEGIN
    WHILE (hi - lo > CutOff) DO (* sort a[lo..hi) *)

      (* use median-of-3 to select a key *)
      i := (hi + lo) DIV 2;
      IF cmp (a[lo], a[i]) < 0 THEN
        IF cmp (a[i], a[hi-1]) < 0 THEN
          key := a[i];
        ELSIF cmp (a[lo], a[hi-1]) < 0 THEN
          key := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        ELSE
          key := a[lo];  a[lo] := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        END;
      ELSE (* a[lo] >= a[i] *)
        IF cmp (a[hi-1], a[i]) < 0 THEN
          key := a[i];  tmp := a[hi-1];  a[hi-1] := a[lo];  a[lo] := tmp;
        ELSIF cmp (a[lo], a[hi-1]) < 0 THEN
          key := a[lo];  a[lo] := a[i];  a[i] := key;
        ELSE
          key := a[hi-1];  a[hi-1] := a[lo];  a[lo] := a[i];  a[i] := key;
        END;
      END;

      (* partition the array *)
      i := lo+1;  j := hi-2;

      (* find the first hole *)
      WHILE cmp (a[j], key) > 0 DO DEC (j) END;
      tmp := a[j];
      DEC (j);

      LOOP
        IF (i > j) THEN EXIT END;

        WHILE cmp (a[i], key) < 0 DO INC (i) END;
        IF (i > j) THEN EXIT END;
        a[j+1] := a[i];
        INC (i);

        WHILE cmp (a[j], key) > 0 DO DEC (j) END;
        IF (i > j) THEN  IF (j = i-1) THEN  DEC (j)  END;  EXIT  END;
        a[i-1] := a[j];
        DEC (j);
      END;

      (* fill in the last hole *)
      a[j+1] := tmp;
      i := j+2;

      (* then, recursively sort the smaller subfile *)
      IF (i - lo < hi - i)
        THEN  QuickSort (a, lo, i-1, cmp);   lo := i;
        ELSE  QuickSort (a, i, hi, cmp);     hi := i-1;
      END;

    END; (* WHILE (hi-lo > CutOff) *)
  END QuickSort;


PROCEDURE InsertionSort (VAR a: ARRAY OF INTEGER;  lo, hi: INTEGER;
                         cmp: CompareProc) =
  VAR j: INTEGER;  key: INTEGER;
  BEGIN
    FOR i := lo+1 TO hi-1 DO
      key := a[i];
      j := i-1;
      WHILE (j >= lo) AND cmp (key, a[j]) < 0 DO
        a[j+1] := a[j];
        DEC (j);
      END;
      a[j+1] := key;
    END;
  END InsertionSort;

(*--------------------------------------------------- low-level utilities ---*)

PROCEDURE Go () =
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    IF NOT started THEN
      (* let the application begin *)
      started := TRUE;
      Wr.PutChar (Stdio.stdout, 'g');
      Wr.Flush (Stdio.stdout);
    END;
  END Go;

PROCEDURE NewVec (n: INTEGER): Vector =
  BEGIN
    RETURN NEW (Vector, n);
  END NewVec;

BEGIN
  SetupVBT ();
  GetTypes ();
  Run ();
  Trestle.AwaitDelete (root);
END ShowNew.


