MODULE Main;

IMPORT ASCII, FloatMode, Fmt, FmtTime, IO, Lex, Params, Process;
IMPORT Random, Scan, Stdio, Text, Thread, Time, Wr;

CONST
  DumpCellStates = FALSE;
  PathSaturation = 0.30;  (* fraction of maze cells that are on the solution path *)

TYPE
  CellState = [ 0..4 ];

CONST
  DeltaX = ARRAY CellState OF [-1 .. +1] { 0,  0, +1,  0, -1 };
  DeltaY = ARRAY CellState OF [-1 .. +1] { 0, +1,  0, -1,  0 };

TYPE
  Quad = RECORD nw, ne, sw, se: CellState; END;

  QMap = RECORD before, after: Quad;  delta: [0..4];  END;

CONST
  Updates = ARRAY [0..31] OF QMap {
  QMap { Quad { 1, 1, 0, 0 },  Quad { 2, 1, 1, 4 }, 2 },
  QMap { Quad { 1, 4, 0, 0 },  Quad { 2, 4, 1, 4 }, 2 },
  QMap { Quad { 2, 0, 2, 0 },  Quad { 1, 2, 2, 3 }, 2 },
  QMap { Quad { 2, 0, 3, 0 },  Quad { 1, 2, 3, 3 }, 2 },

  QMap { Quad { 0, 0, 1, 1 },  Quad { 1, 2, 4, 1 }, 2 },
  QMap { Quad { 0, 0, 1, 2 },  Quad { 1, 2, 4, 2 }, 2 },
  QMap { Quad { 4, 0, 4, 0 },  Quad { 4, 3, 1, 4 }, 2 },
  QMap { Quad { 3, 0, 4, 0 },  Quad { 3, 3, 1, 4 }, 2 },

  QMap { Quad { 0, 2, 0, 2 },  Quad { 2, 3, 1, 2 }, 2 },
  QMap { Quad { 0, 2, 0, 1 },  Quad { 2, 3, 1, 1 }, 2 },
  QMap { Quad { 3, 3, 0, 0 },  Quad { 3, 2, 4, 3 }, 2 },
  QMap { Quad { 4, 3, 0, 0 },  Quad { 4, 2, 4, 3 }, 2 },

  QMap { Quad { 0, 0, 3, 3 },  Quad { 2, 3, 3, 4 }, 2 },
  QMap { Quad { 0, 0, 2, 3 },  Quad { 2, 3, 2, 4 }, 2 },
  QMap { Quad { 0, 4, 0, 4 },  Quad { 1, 4, 4, 3 }, 2 },
  QMap { Quad { 0, 1, 0, 4 },  Quad { 1, 1, 4, 3 }, 2 },

  QMap { Quad { 1, 2, 0, 2 },  Quad { 2, 0, 1, 2 }, 0 },
  QMap { Quad { 1, 2, 0, 1 },  Quad { 2, 0, 1, 1 }, 0 },
  QMap { Quad { 3, 3, 0, 4 },  Quad { 3, 0, 4, 3 }, 0 },
  QMap { Quad { 4, 3, 0, 4 },  Quad { 4, 0, 4, 3 }, 0 },

  QMap { Quad { 1, 1, 4, 0 },  Quad { 0, 1, 1, 4 }, 0 },
  QMap { Quad { 1, 4, 4, 0 },  Quad { 0, 4, 1, 4 }, 0 },
  QMap { Quad { 2, 3, 2, 0 },  Quad { 0, 2, 2, 3 }, 0 },
  QMap { Quad { 2, 3, 3, 0 },  Quad { 0, 2, 3, 3 }, 0 },

  QMap { Quad { 2, 0, 1, 1 },  Quad { 1, 2, 0, 1 }, 0 },
  QMap { Quad { 2, 0, 1, 2 },  Quad { 1, 2, 0, 2 }, 0 },
  QMap { Quad { 4, 0, 4, 3 },  Quad { 4, 3, 0, 4 }, 0 },
  QMap { Quad { 3, 0, 4, 3 },  Quad { 3, 3, 0, 4 }, 0 },

  QMap { Quad { 0, 2, 3, 3 },  Quad { 2, 3, 3, 0 }, 0 },
  QMap { Quad { 0, 2, 2, 3 },  Quad { 2, 3, 2, 0 }, 0 },
  QMap { Quad { 0, 4, 1, 4 },  Quad { 1, 4, 4, 0 }, 0 },
  QMap { Quad { 0, 1, 1, 4 },  Quad { 1, 1, 4, 0 }, 0 }
  };

TYPE
  CellGrid = REF ARRAY OF ARRAY OF CellState;
  EdgeGrid = REF ARRAY OF ARRAY OF BOOLEAN;
  LetterDesc = RECORD ch: CHAR;  loc: INTEGER; END;

VAR
  n_rows : INTEGER;
  n_cols : INTEGER;
  rnd    : Random.T;
  cells  : CellGrid;
  rows   : EdgeGrid;
  cols   : EdgeGrid;
  path   : REF ARRAY OF INTEGER;
  letters: REF ARRAY OF LetterDesc;

PROCEDURE DoIt () =
  CONST NO_VALUE = -39494;
  VAR i, extras: INTEGER;  arg, puzzle: TEXT;  solved: BOOLEAN;
  BEGIN
    IF Params.Count < 2 OR Params.Count > 8 THEN Usage (); END;

    n_rows := NO_VALUE;
    n_cols := NO_VALUE;
    puzzle := NIL;
    extras := 0;
    solved := FALSE;

    i := 1;
    WHILE (i < Params.Count) DO
      arg := Params.Get (i);  INC (i);
      IF Text.Equal (arg, "-solved") THEN
        solved := TRUE;
      ELSIF Text.Equal (arg, "-word") THEN
        IF (i >= Params.Count) THEN Usage (); END;
        puzzle := Params.Get (i);  INC (i);
      ELSIF Text.Equal (arg, "-extras") THEN
        IF (i >= Params.Count) THEN Usage (); END;
        extras := GetInt (Params.Get (i), "extras");  INC (i);        
      ELSIF n_rows = NO_VALUE THEN
        n_rows := GetInt (Params.Get (1), "rows");
      ELSIF n_cols = NO_VALUE THEN
        n_cols := GetInt (Params.Get (2), "columns");
      ELSE
        Usage ();
      END;
    END;

    IF n_rows = NO_VALUE THEN Usage (); END;
    IF n_cols = NO_VALUE THEN n_cols := n_rows; END;
    IF (n_rows < n_cols) THEN
      (* swap'em *)
      VAR tmp := n_rows; BEGIN n_rows := n_cols;  n_cols := tmp; END;
    END;

    rnd := NEW (Random.Default).init (fixed := FALSE);

    InitCells ();
    SetPath ();
    FillCells ();
    MarkEdges ();
    PlotLetters (puzzle, extras);
    DumpMaze (solved);
  END DoIt;

PROCEDURE InitCells () =
  BEGIN
    cells := NEW (CellGrid, n_rows, n_cols);
    FOR x := 0 TO n_rows - 1 DO
      FOR y := 0 TO n_cols - 1 DO
        cells [x, y] := 0;  (* closed cell *)
      END;
    END;
  END InitCells;

PROCEDURE SetPath () =
  VAR
    len  := n_rows + n_cols - 1;
    goal := ROUND (FLOAT (n_rows * n_cols) * PathSaturation);
    x, y, z: INTEGER;
    n_mutations := 0;
    big_extension : INTEGER;
  BEGIN
    (* build the initial path *)

    (* first the diagonal *)
    FOR i := 0 TO MIN (n_rows, n_cols)-2 DO
      cells [i, i] := 1;
      cells [i, i+1] := 2;
    END;

    (* then, finish with either a horizontal or vertical strip *)
    IF (n_rows < n_cols) THEN
      cells [n_rows-1, n_rows-1] := 1;
      FOR i := n_rows TO n_cols-1 DO cells [n_rows-1, i] := 1; END;
    ELSE
      FOR i := n_cols-1 TO n_rows-2 DO cells [i, n_cols-1] := 2; END;
      cells [n_rows-1, n_cols-1] := 1;
    END;

    (* now, extend the path until it's long enough to be interesting *)
    big_extension := 0;
    WHILE (len < goal) DO
      x := rnd.integer (0, n_rows-2);
      y := rnd.integer (0, n_cols-2);
      IF (big_extension <= 0) AND FindExtension (x, y, z) THEN
        INC (len, z);
        big_extension := 5; (* don't make too many big ones in a row... *)
      ELSIF FindUpdate (x, y, z, TRUE) THEN
        WITH u = Updates [z] DO
          cells [x, y]     := u.after.nw;
          cells [x, y+1]   := u.after.ne;
          cells [x+1, y]   := u.after.sw;
          cells [x+1, y+1] := u.after.se;
          INC (len, u.delta);
        END;
        DEC (big_extension);
      END;
    END;

    (* now, mutate the path so that it runs around a bit... *)
    n_mutations := 0;
    goal := 2 * n_rows * n_cols;
    WHILE (n_mutations < goal) DO
      x := rnd.integer (0, n_rows-2);
      y := rnd.integer (0, n_cols-2);
      IF FindUpdate (x, y, z, FALSE) THEN
        WITH u = Updates [z] DO
          cells [x, y]     := u.after.nw;
          cells [x, y+1]   := u.after.ne;
          cells [x+1, y]   := u.after.sw;
          cells [x+1, y+1] := u.after.se;
          INC (n_mutations);
        END;
      END;
    END;

    (* finally, record the path *)
    path := NEW (REF ARRAY OF INTEGER, len);
    len := 0;
    x := 0;  y := 0;
    WHILE (len < NUMBER (path^)) DO
      path [len] := x * n_cols + y;  INC (len);
      z := cells [x, y];   <* ASSERT z # 0 *>
      INC (x, DeltaX [z]);
      INC (y, DeltaY [z]);
    END;
(*******
    FOR r := 0 TO n_rows-1 DO
      FOR c := 0 TO n_cols-1 DO
        IF cells [r, c] # 0 THEN
          path [len] := r * n_cols + c;
          INC (len);
        END;
      END;
    END;
    <* ASSERT len = NUMBER (path^) *>
******)
  END SetPath;

PROCEDURE FindExtension (x, y: INTEGER;  VAR(*OUT*) z: INTEGER): BOOLEAN =
  VAR x0, y0, h, w: INTEGER;
  BEGIN
    IF cells [x, y] # 0 THEN RETURN FALSE; END;
    FindOpenRectangle (x, y, x0, y0, h, w);
    IF (h < 2) OR (w < 2) OR (h * w < 8) THEN RETURN FALSE; END;
    h := MIN (h, rnd.integer (2, n_rows-1));
    w := MIN (w, rnd.integer (2, n_cols-1));
    IF (h < 2) OR (w < 2) OR (h * w < 8) THEN RETURN FALSE; END;
    IF NOT FindAttachment (x0, y0, h, w, x, y) THEN RETURN FALSE; END;
    CASE cells [x, y] OF
    | 0 => RETURN FALSE;
    | 1 =>
        IF x < x0 THEN
          (* attach on the north *)
          BuildCounterClockwisePath (x0, y0, h, w);
          cells [x, y] := 2;  cells [x+1, y+1] := 4;
        ELSE
          (* attach on the south *)
          BuildClockwisePath (x0, y0, h, w);
          cells [x, y] := 4;  cells [x-1, y+1] := 2;
        END;
    | 2 =>
        IF y < y0 THEN
          (* attach on the west *)
          BuildClockwisePath (x0, y0, h, w);
          cells [x, y] := 1;  cells [x+1, y+1] := 3;
        ELSE
          (* attach on the east *)
          BuildCounterClockwisePath (x0, y0, h, w);
          cells [x, y] := 3;  cells [x+1, y-1] := 1;
        END;
    | 3 =>
        IF x < x0 THEN
          (* attach on the north *)
          BuildClockwisePath (x0, y0, h, w);
          cells [x, y] := 2;  cells [x+1, y-1] := 4;
        ELSE
          (* attach on the south *)
          BuildCounterClockwisePath (x0, y0, h, w);
          cells [x, y] := 4;  cells [x-1, y-1] := 2;
        END;
    | 4 =>
        IF y < y0 THEN
          (* attach on the west *)
          BuildCounterClockwisePath (x0, y0, h, w);
          cells [x, y] := 1;  cells [x-1, y+1] := 3;
        ELSE
          (* attach on the east *)
          BuildClockwisePath (x0, y0, h, w);
          cells [x, y] := 3;  cells [x-1, y-1] := 1;
        END;
    END;

    (* set the new path length *)
    z := h + h + w + w - 4;
    RETURN TRUE;
  END FindExtension;

VAR
  limit_a, limit_b : REF ARRAY OF INTEGER;

PROCEDURE FindOpenRectangle (x, y: INTEGER;
                  VAR(*OUT*) x0, y0, h, w: INTEGER) =
  VAR j, k, x1, x2: INTEGER;
      best_x, best_y, best_h, best_w: INTEGER;
  BEGIN
    IF (limit_a = NIL) THEN
      limit_a := NEW (REF ARRAY OF INTEGER, n_cols);
      limit_b := NEW (REF ARRAY OF INTEGER, n_cols);
    END;

    (* find the empty vertical stripes around row "x" *)
    FOR i := 0 TO n_cols-1 DO
      j := x; WHILE (j >= 0) AND (cells[j, i] = 0) DO DEC (j); END;
      limit_a[i] := j;
      j := x; WHILE (j < n_rows) AND (cells[j, i] = 0) DO INC (j); END;
      limit_b[i] := j;
    END;

    (* clip the vertical stripes so they can always reach colum "y" *)
    j := limit_a[y];   k := limit_b[y];
    FOR i := y-1 TO 0 BY -1 DO
      j := MAX (j, limit_a[i]);  limit_a[i] := j;
      k := MIN (k, limit_b[i]);  limit_b[i] := k;
    END;
    j := limit_a[y];   k := limit_b[y];
    FOR i := y+1 TO n_cols-1 DO
      j := MAX (j, limit_a[i]);  limit_a[i] := j;
      k := MIN (k, limit_b[i]);  limit_b[i] := k;
    END;

    best_x := 0; best_y := 0;  best_h := 0; best_w := 0;

    FOR i := 0 TO y DO
      FOR j := y TO n_cols-1 DO
        x1 := MAX (limit_a[i], limit_a[j]) + 1;
        x2 := MIN (limit_b[i], limit_b[j]);
        IF (x2 - x1) * (j - i + 1) > best_h * best_w THEN
          best_x := x1;
          best_y := i;
          best_h := x2 - x1;
          best_w := j - i + 1;
        END;
      END;
    END;

    x0 := best_x;
    y0 := best_y;
    h  := best_h;
    w  := best_w;
  END FindOpenRectangle;

PROCEDURE FindAttachment (x, y, h, w: INTEGER;
                          VAR(*OUT*) x0, y0: INTEGER): BOOLEAN =
  VAR
    skip := rnd.integer (0, 3); (* # of edges tests to skip *)
    cnt  := 4;  (* # of edges to scan *)
  BEGIN
    LOOP
      IF cnt <= 0 THEN RETURN FALSE; END;
      IF (skip <= 0) THEN
        DEC (cnt);
        IF x > 0 THEN
          (* search the north edge *)
          FOR c := y TO y + w -1 DO
            CASE cells [x-1, c] OF
            | 0, 2, 4 => (* nope *)
            | 1 => IF (c < y + w - 1) THEN x0 := x-1;  y0 := c;  RETURN TRUE; END;
            | 3 => IF (c > y)         THEN x0 := x-1;  y0 := c;  RETURN TRUE; END;
            END;
          END;
        END;
      END;
      DEC (skip);


      IF cnt <= 0 THEN RETURN FALSE; END;
      IF (skip <= 0) THEN
        DEC (cnt);
        IF y > 0 THEN
          (* search the west edge *)
          FOR r := x TO x + h -1 DO
            CASE cells [r, y-1] OF
            | 0, 1, 3 => (* nope *)
            | 2 => IF (r < x + h - 1) THEN x0 := r;  y0 := y-1;  RETURN TRUE; END;
            | 4 => IF (r > x)         THEN x0 := r;  y0 := y-1;  RETURN TRUE; END;
            END;
          END;
        END;
      END;
      DEC (skip);

      IF cnt <= 0 THEN RETURN FALSE; END;
      IF (skip <= 0) THEN
        DEC (cnt);
        IF x+h < n_rows THEN
          (* search the south edge *)
          FOR c := y TO y + w -1 DO
            CASE cells [x+h, c] OF
            | 0, 2, 4 => (* nope *)
            | 1 => IF (c < y + w - 1) THEN x0 := x+h;  y0 := c;  RETURN TRUE; END;
            | 3 => IF (c > y)         THEN x0 := x+h;  y0 := c;  RETURN TRUE; END;
            END;
          END;
        END;
      END;
      DEC (skip);

      IF cnt <= 0 THEN RETURN FALSE; END;
      IF (skip <= 0) THEN
        DEC (cnt);
        IF y+w < n_cols THEN
          (* search the east edge *)
          FOR r := x TO x + h -1 DO
            CASE cells [r, y+w] OF
            | 0, 1, 3 => (* nope *)
            | 2 => IF (r < x + h - 1) THEN x0 := r;  y0 := y+w;  RETURN TRUE; END;
            | 4 => IF (r > x)         THEN x0 := r;  y0 := y+w;  RETURN TRUE; END;
            END;
          END;
        END;
      END;
      DEC (skip);
    END;
  END FindAttachment;

PROCEDURE BuildClockwisePath (x, y, h, w: INTEGER) =
  VAR x1 := x + h - 1;   y1 := y + w - 1;
  BEGIN
    FOR i := x+1 TO x1-1 DO
      cells [i, y] := 4;
      cells [i, y1] := 2;
    END;
    FOR i := y+1 TO y1-1 DO
      cells [x, i] := 1;
      cells [x1, i] := 3;
    END;
    cells [x, y]  := 1;   cells [x, y1]  := 2;
    cells [x1, y] := 4;   cells [x1, y1] := 3;
  END BuildClockwisePath;

PROCEDURE BuildCounterClockwisePath (x, y, h, w: INTEGER) =
  VAR x1 := x + h - 1;   y1 := y + w - 1;
  BEGIN
    FOR i := x+1 TO x1-1 DO
      cells [i, y] := 2;
      cells [i, y1] := 4;
    END;
    FOR i := y+1 TO y1-1 DO
      cells [x, i] := 3;
      cells [x1, i] := 1;
    END;
    cells [x, y]  := 2;   cells [x, y1]  := 3;
    cells [x1, y] := 1;   cells [x1, y1] := 4;
  END BuildCounterClockwisePath;

(****
PROCEDURE DumpCells () =
  BEGIN
    FOR r := 0 TO n_rows-1 DO
      FOR c := 0 TO n_cols -1 DO
        OutX (" ", Fmt.Int (cells[r, c]));
      END;
      Out ();
    END;
  END DumpCells;
****)

PROCEDURE FindUpdate (x, y: INTEGER;  VAR(*OUT*) z: INTEGER;
                      extend: BOOLEAN): BOOLEAN =
  VAR q: Quad;
  BEGIN
    q.nw := cells [x, y];
    q.ne := cells [x, y+1];
    q.sw := cells [x+1, y];
    q.se := cells [x+1, y+1];
    FOR i := FIRST (Updates) TO LAST (Updates) DO
      IF Updates[i].before = q THEN
        z := i;
        RETURN (extend OR Updates[i].delta = 0);
      END;
    END;
    RETURN FALSE;
  END FindUpdate;

PROCEDURE FillCells () =
  VAR
    n_free := 0;
    free := NEW (REF ARRAY OF INTEGER, n_rows * n_cols);
    x, y, z: INTEGER;
    s: CellState;
  BEGIN
    (* gather the free cells *)
    FOR r := 0 TO n_rows-1 DO
      FOR c := 0 TO n_cols-1 DO
        IF cells[r,c] = 0 THEN
          free [n_free] := r * n_cols + c;
          INC (n_free);
        END;
      END;
    END;

    WHILE n_free > 0 DO
      z := rnd.integer (0, n_free - 1);
      x := free[z] DIV n_cols;
      y := free[z] - x * n_cols;
      IF cells [x, y] # 0 THEN
        DEC (n_free); free [z] := free [n_free];
      ELSIF FindNeighbor (x, y, FALSE, s) THEN
        cells [x, y] := s;
        DEC (n_free);  free [z] := free [n_free];
        (* continue to extend the path for a bit *)
        WHILE (rnd.integer (0, 10) > 0) DO
          IF FindNeighbor (x, y, TRUE, s) THEN
            CASE s OF
            | 0 => (* nope *)
            | 1 => INC (y);  cells [x, y] := 3;
            | 2 => INC (x);  cells [x, y] := 4;
            | 3 => DEC (y);  cells [x, y] := 1;
            | 4 => DEC (x);  cells [x, y] := 2;
            END;
          END;
        END;
      END;
    END;
  END FillCells;

PROCEDURE FindNeighbor (x, y: INTEGER;  free: BOOLEAN;
                        VAR(*OUT*) s: CellState): BOOLEAN =
  TYPE
   Offset = RECORD dx,dy: [-1..+1];  state: CellState  END;
  CONST
    Neighbor = ARRAY [0..6] OF Offset {
      Offset { -1, 0,  4 },  Offset { 0, +1,  1 },
      Offset { +1, 0,  2 },  Offset { 0, -1,  3 },
      Offset { -1, 0,  4 },  Offset { 0, +1,  1 },
      Offset { +1, 0,  2 }
    };
  VAR
    z0 := rnd.integer (0, 3);
    x1, y1: INTEGER;
  BEGIN
    FOR z := z0 TO z0 + 3 DO
      WITH n = Neighbor[z] DO
        x1 := x + n.dx;
        y1 := y + n.dy;
        IF (0 <= x1) AND (x1 < n_rows)
        AND (0 <= y1) AND (y1 < n_cols)
        AND (cells [x1, y1] = 0) = free THEN
          s := n.state;
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END FindNeighbor;

PROCEDURE MarkEdges () =
  BEGIN
    rows := NEW (EdgeGrid, n_rows+1, n_cols+1);
    cols := NEW (EdgeGrid, n_rows+1, n_cols+1);

    (* fill in all the edges *)
    FOR i := 0 TO n_rows DO
      FOR j := 0 TO n_cols DO
        rows [i, j] := TRUE;
        cols [i, j] := TRUE;
      END;
    END;

    (* remove the extra outer edges *)
    FOR i := 0 TO n_rows DO rows[i, n_cols] := FALSE; END;
    FOR i := 0 TO n_cols DO cols[n_rows, i] := FALSE; END;

    (* open the entry door *)
    cols [0, 0] := FALSE;

    FOR r := 0 TO n_rows-1 DO
      FOR c := 0 TO n_cols-1 DO
        CASE cells [r, c] OF
        | 0 => (* free  *)
        | 1 => (* east  *) cols [r, c+1] := FALSE;
        | 2 => (* south *) rows [r+1, c] := FALSE;
        | 3 => (* west  *) cols [r, c]   := FALSE;
        | 4 => (* north *) rows [r, c]   := FALSE;
        END;
      END;
    END;
  END MarkEdges;

(*--------------------------------------------------- character plotting ---*)

PROCEDURE PlotLetters (puzzle: TEXT;  n_extras: INTEGER) =
  VAR cnt := n_extras;  len: CARDINAL := 0;
  BEGIN
    IF puzzle # NIL THEN len := Text.Length (puzzle); INC (cnt, len); END;
    letters := NEW (REF ARRAY OF LetterDesc, cnt);

    IF puzzle # NIL THEN
      FOR i := 0 TO len-1 DO
        WITH ll = letters[i] DO
          ll.ch := ASCII.Upper [Text.GetChar (puzzle, i)];
          ll.loc := FindNthCell (i, len);
        END;
      END;
    END;

    FOR i := 0 TO n_extras-1 DO
      WITH ll = letters [i + len] DO
        ll.ch := VAL (ORD ('A') + rnd.integer (0, 25), CHAR);
        ll.loc := FindEmptyCell (len + i);
      END;
    END;
  END PlotLetters;

PROCEDURE FindNthCell (n, m: INTEGER): INTEGER =
  BEGIN
    RETURN path [ ROUND (FLOAT (n+1) / FLOAT (m+1) * FLOAT (NUMBER (path^))) ];
  END FindNthCell;

PROCEDURE FindEmptyCell (n_done: INTEGER): INTEGER =
  (* find an off-path cell with no letter *)
  VAR loc: INTEGER;  ok: BOOLEAN;
  BEGIN
    LOOP
      loc := rnd.integer (0, n_rows * n_cols - 1);
      ok  := TRUE;
      FOR i := 0 TO n_done-1 DO
        IF letters [i].loc = loc THEN ok := FALSE;  EXIT; END;
      END;
      IF ok THEN
        FOR i := 0 TO LAST (path^) DO
          IF path [i] = loc THEN ok := FALSE; EXIT; END;
        END;
      END;
      IF ok THEN RETURN loc; END;
    END;
  END FindEmptyCell;

(*---------------------------------------------------- PostScript output ---*)

PROCEDURE DumpMaze (solved: BOOLEAN) =
  CONST
    MaxWidth  = 468; (* 6.5 inches x 72pts/inch *)
    MaxHeight = 648; (* 9 inches x 72pts/inch *)
  VAR
    row_pts  := MaxHeight DIV n_rows;
    col_pts  := MaxWidth DIV n_cols;
    unit_pts := MAX (2, MIN (row_pts, col_pts));
    x, y, cnt : INTEGER;
  BEGIN
    (* print the result *)
    Out ("%!PS-Adobe-1.0");
    OutX ("%%Title: (", Fmt.Int (n_rows), " x ");
    Out  (Fmt.Int (n_cols), " Maze)");
    Out ("%%Creator: ", Params.Get (0));
    Out ("%%CreationDate: ", FmtTime.Long (Time.Now ()));
    Out ("%%Pages: 1");
    Out ("%%EndComments:");
    Out ();
    Out ("/baseX 72  def");
    Out ("/baseY 720 def");
    Out ("/zz ", Fmt.Int (unit_pts), " def");
    Out ("/goto { newpath");
    Out ("   zz mul baseX add exch");
    Out ("   zz mul baseY exch sub moveto");
    Out (" } def");
    Out ("/row { goto zz 0 rlineto stroke } def");
    Out ("/col { goto 0 zz neg rlineto stroke } def");
    IF DumpCellStates OR letters # NIL THEN
      Out ("/Helvetica findfont ", Fmt.Real (FLOAT (unit_pts) * 0.9),
           " scalefont setfont");
      Out ("/cshow { dup gsave 0 0 moveto true charpath flattenpath pathbbox ");
      Out ("   2 div neg exch 2 div neg exch grestore rmoveto pop pop show } def");
      Out ("/tag { goto zz 2 div zz 2 div neg rmoveto cshow } def");
    END;
    Out ("/cell { goto zz 0 rlineto 0 zz neg rlineto zz neg 0 rlineto");
    Out ("        closepath fill } def");

    Out ();
    Out ("%%EndProlog");
    Out ("%%BeginPage: 1 1");

    Out ("%--- the solution ----");
    Out ("/showsolution ", ARRAY BOOLEAN OF TEXT {"false", "true"}[solved], " def");
    Out ("showsolution {");
    OutX ("  gsave 0.8 setgray");
    cnt := 0;
    FOR i := 0 TO LAST (path^) DO
      IF (i > 0) AND (i MOD 100 = 0) THEN
        Out (); Out ("} if"); OutX ("showsolution {"); cnt := 0;
      END;
      IF cnt <= 0 THEN Out ();  OutX ("  "); cnt := 5; END;
      x := path [i] DIV n_cols;
      y := path [i] - x * n_cols;
      OutX (Fmt.Int (x), " ", Fmt.Int (y), " cell ");
      DEC (cnt);
    END;
    Out ();
    Out ("  grestore");
    Out ("} if");
    Out ();

    IF letters # NIL THEN
      Out ("%--- the puzzle ----");
      FOR i := 0 TO LAST (letters^) DO
        WITH ll = letters [i] DO
          x := ll.loc DIV n_cols;
          y := ll.loc - x * n_cols;
          OutX ("(", Text.FromChar (ll.ch), ") ");
          Out  (Fmt.Int (x), " ", Fmt.Int (y), " tag ");
        END;
      END;
    END;

    Out ("%--- the maze walls ---");
    cnt := 5;
    FOR r := 0 TO n_rows DO
      FOR c := 0 TO n_cols DO
        IF DumpCellStates THEN
          IF (r < n_rows) AND (c < n_cols) THEN
            IF cnt <= 0 THEN Out ();  cnt := 5; END;
            OutX ("(", Fmt.Int (cells[r,c]), ")" );
            OutX (Fmt.Int (r), " ", Fmt.Int (c), " tag ");
            DEC (cnt);
          END;
        END;
        IF rows [r, c] THEN
          IF cnt <= 0 THEN Out ();  cnt := 5; END;
          OutX (Fmt.Int (r), " ", Fmt.Int (c), " row ");
          DEC (cnt);
        END;
        IF cols [r, c] THEN
          IF cnt <= 0 THEN Out ();  cnt := 5; END;
          OutX (Fmt.Int (r), " ", Fmt.Int (c), " col ");
          DEC (cnt);
        END;
      END;
      Out ();  cnt := 5;
    END;
    Out ();

    Out ("showpage");
    Out ("%%EndPage");
    Out ("%%Trailer");
    Out ("%%Pages: 1");
  END DumpMaze;

PROCEDURE Out (a, b, c, d: TEXT := NIL) =
  BEGIN
    OutX (a, b, c, d, Wr.EOL);
  END Out;

PROCEDURE OutX (a, b, c, d, e: TEXT := NIL) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR wr := Stdio.stdout;
  BEGIN
    IF (a # NIL) THEN Wr.PutText (wr, a); END;
    IF (b # NIL) THEN Wr.PutText (wr, b); END;
    IF (c # NIL) THEN Wr.PutText (wr, c); END;
    IF (d # NIL) THEN Wr.PutText (wr, d); END;
    IF (e # NIL) THEN Wr.PutText (wr, e); END;
  END OutX;

(*------------------------------------------------------------- misc I/O ---*)

PROCEDURE GetInt (arg: TEXT;  name: TEXT): INTEGER =
  VAR n := 0;
  BEGIN
    TRY
      n := Scan.Int (arg);
    EXCEPT Lex.Error, FloatMode.Trap =>
      n := -1;
    END;
    IF n <= 0 THEN
      Die ("Illegal number of " & name & " \"" & arg
           & "\", must be a positive integer.");
    END;
    RETURN n;
  END GetInt;

PROCEDURE Usage () =
  BEGIN
    Die ("usage: maze n_rows [n_cols] [-solved] [-word <whatever>] [-extras <n>]");
  END Usage;

PROCEDURE Die (msg: TEXT) =
  BEGIN
    IO.Put (msg & Wr.EOL);
    Process.Exit (1);
  END Die;

BEGIN
  DoIt ();
END Main.
