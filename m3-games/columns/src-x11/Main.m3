(* Copyright (C) 1992, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Fri Nov 12 16:53:54 PST 1993 by kalsow  *)
(*      modified on Tue Mar 17 14:53:11 PST 1992 by muller  *)

UNSAFE MODULE Main;

IMPORT Stdio, Wr, Fmt, Word, Text, Random, Cstdlib, Convert;
IMPORT X, Xaw, Xt, XtN, Xmu, RTLinker, M3toC;
IMPORT Columns, Rows, Bars, Squares, Threes;

FROM M3toC IMPORT TtoS;
FROM Config IMPORT Point, Piece, Game;

<*FATAL ANY*>

CONST
  Aspect_ratio = 3    (* hUnit / vUnit *);
  Score_basis  = 65.645;
  Height_pts   = 10;
  Speedup      = 0.95;
  Speedup_step = 5;
  Max_rows     = (30 * Aspect_ratio);
  Max_cols     = 30;
  Max_pieces   = 70;
  Max_tints    = 36;
  Max_tiles    = 30;
  Max_level    = 5;

TYPE
  Event = {Noop, Tick, Drop, Move_left, Rotate_left, Rotate_right, Move_right};

TYPE
  State = {Falling, Resting, Blinking, Pausing, Done};

TYPE
  Color = [0 .. Max_tints-1];

CONST
  WHITE    = FIRST (Color);
  BLACK    = LAST (Color) - 1;
  NO_COLOR = LAST (Color);

TYPE
  Rect = RECORD x, y, width, height: INTEGER END;

(* misc. flags & global parameters *)
VAR  speed_up := TRUE;
VAR  one_hand := TRUE;
VAR  running  := FALSE;
VAR  paused   := FALSE;
VAR  level    := 1;
VAR  keymap   : ARRAY CHAR OF Event;
VAR  rand     : Random.T;

(* clock *)
VAR delay : INTEGER := 0;
VAR tid   : Xt.IntervalId;

(* game state *)
VAR  all_games : ARRAY [0 .. 4] OF Game;
VAR  gameID    : INTEGER;
VAR  game      : Game;
VAR  state     := State.Done;
VAR  counter   := 0;
VAR  curPiece  := 0; (* piece *)
VAR  cur       : Piece;
VAR  loc       : Point;
VAR  curRot    := 0;
VAR  dropped   := FALSE;
VAR  nWipeouts := 0;
VAR  board     : ARRAY [0 .. Max_cols-1], [0 .. Max_rows-1] OF Color;
VAR  wipeout   : ARRAY [0 .. Max_cols * Max_rows -1] OF Point;
VAR  curTint   : ARRAY [0 .. Max_tiles] OF Color;

(* scoring *)
VAR  score     := 0;
VAR  nWiped    := 0;

(* graphics state *)
VAR  domain      := Rect { 0,0,0,0 };
VAR  vUnit       := 0;
VAR  hUnit       := 0;
VAR  tints       : ARRAY Color OF INTEGER;
VAR  topWindow   : Xt.Widget;
VAR  box         : Xt.Widget;
VAR  form        : Xt.Widget;
VAR  lastChild   : Xt.Widget := NIL;
VAR  goButton    : Xt.Widget;
VAR  pauseButton : Xt.Widget;
VAR  stopButton  : Xt.Widget;
VAR  levelButton : Xt.Widget;
VAR  gameButton  : Xt.Widget;
VAR  handButton  : Xt.Widget;
VAR  speedButton : Xt.Widget;
VAR  quitButton  : Xt.Widget;
VAR  keyLabel    : ARRAY [0..4] OF Xt.Widget;
VAR  wipeLabel   : Xt.Widget;
VAR  scoreLabel  : Xt.Widget;
VAR  stateLabel  : Xt.Widget;
VAR  playingArea : Xt.Widget;
VAR  win         : X.Window;
VAR  playWin     : X.Window;
VAR  gc          : X.GC;
VAR  dpy         : X.DisplayStar;
VAR  kbstate     : X.XKeyboardState;


(**************************************************************)
(*               Window system primitives                     *)
(**************************************************************)

TYPE ArgList = RECORD
       n: INTEGER;
       a: Xt.ArgList;
       v: ARRAY [0..19] OF Xt.Arg;
     END;

PROCEDURE AddArgI (VAR x: ArgList;  label: Xt.String;  val: INTEGER) =
  BEGIN
    x.a := ADR (x.v[0]);
    WITH n = x.v[x.n] DO
      n.name := label;
      n.value := LOOPHOLE(val, ADDRESS);
    END;
    INC (x.n);
  END AddArgI;

PROCEDURE AddArgT (VAR x: ArgList;  label: Xt.String;  text: TEXT) =
  BEGIN
    x.a := ADR (x.v[0]);
    WITH n = x.v[x.n] DO  n.name := label;  n.value := TtoS (text)  END;
    INC (x.n);
  END AddArgT;

PROCEDURE AddArgA (VAR x: ArgList;  label: Xt.String;  addr: ADDRESS) =
  BEGIN
    x.a := ADR (x.v[0]);
    WITH n = x.v[x.n] DO  n.name := label;  n.value := addr END;
    INC (x.n);
  END AddArgA;

PROCEDURE CreateButton (label: TEXT;  handler: Xt.CallbackProc): Xt.Widget =
  VAR w: Xt.Widget;  args: ArgList;
  BEGIN
    args.n := 0;
    AddArgT (args, XtN.label,       label);
    AddArgI (args, XtN.borderWidth, 2);
    AddArgI (args, XtN.width,       140);

    w := Xt.CreateManagedWidget (TtoS (label), Xaw.commandWidgetClass,
                                 form, args.a, args.n);
    Xt.AddCallback (w, XtN.callback, handler);
    lastChild := w;
    RETURN w;
  END CreateButton;


PROCEDURE CreateLabel (text: TEXT): Xt.Widget =
  VAR w: Xt.Widget;  args: ArgList;
  BEGIN
    args.n := 0;
    text := Fmt.Pad (text, 17, ' ', Fmt.Align.Left);
    AddArgT (args, XtN.label,       text);
    AddArgI (args, XtN.borderWidth, 0);
    AddArgI (args, XtN.justify,     Xmu.JustifyLeft);

    w := Xt.CreateManagedWidget (TtoS (text), Xaw.labelWidgetClass,
                                 form, args.a, args.n);
    lastChild := w;
    RETURN w;
  END CreateLabel;


PROCEDURE SetLabel (label: Xt.Widget;  text: TEXT) =
  VAR args: ArgList;
  BEGIN
    args.n := 0;
    AddArgT (args, XtN.label, text);
    Xt.SetValues (label, args.a, args.n);
  END SetLabel;


PROCEDURE Enable (w: Xt.Widget) =
  VAR args: ArgList;
  BEGIN
    args.n := 0;
    AddArgI (args, XtN.sensitive, ORD (TRUE));
    Xt.SetValues (w, args.a, args.n);
  END Enable;


PROCEDURE Disable (w: Xt.Widget) =
  VAR args: ArgList;
  BEGIN
    args.n := 0;
    AddArgI (args, XtN.sensitive, ORD (FALSE));
    Xt.SetValues (w, args.a, args.n);
  END Disable;


PROCEDURE MaskButtons (mask: TEXT) =
  BEGIN
    FOR i := 0 TO Text.Length (mask)-1 DO
      CASE (Text.GetChar (mask, i)) OF
      | 'g' =>  Disable (goButton);
      | 'G' =>  Enable  (goButton);
      | 'p' =>  Disable (pauseButton);
      | 'P' =>  Enable  (pauseButton);
      | 's' =>  Disable (stopButton);
      | 'S' =>  Enable  (stopButton);
      | 'x' =>  Disable (gameButton);
      | 'X' =>  Enable  (gameButton);
      | 'l' =>  Disable (levelButton);
      | 'L' =>  Enable  (levelButton);
      | 'h' =>  Disable (handButton);
      | 'H' =>  Enable  (handButton);
      | 'Z' =>  Enable  (speedButton);
      | 'z' =>  Disable (speedButton);
      ELSE      (* ignore *)
      END;
    END;
  END MaskButtons;


PROCEDURE GrabFocus () =
  BEGIN
    EVAL X.XGrabKeyboard (dpy, win, ORD (FALSE), X.GrabModeAsync,
                              X.GrabModeAsync, X.CurrentTime);
(*IF (kbstate.global_auto_repeat = ORD(TRUE)) THEN X.XAutoRepeatOff(dpy) END;*)
  END GrabFocus;


PROCEDURE DropFocus () =
  BEGIN
    X.XUngrabKeyboard (dpy, X.CurrentTime);
(*IF (kbstate.global_auto_repeat = ORD(TRUE)) THEN X.XAutoRepeatOn (dpy) END;*)
  END DropFocus;


PROCEDURE StopClock () =
  BEGIN
    IF (tid # 0) THEN Xt.RemoveTimeOut (tid);  tid := 0; END;
  END StopClock;


PROCEDURE StartClock () =
  BEGIN
    IF (tid # 0) THEN Xt.RemoveTimeOut (tid) END;
    tid := Xt.AddTimeOut (delay, ClockTick);
  END StartClock;


PROCEDURE MakeColor (r, g, b: INTEGER): INTEGER =
  VAR color: X.XColor;  scr: INTEGER;
  BEGIN
    scr := X.XDefaultScreen (dpy);
    IF (r = 255) AND (g = 255) AND (b = 255) THEN
      color.pixel := X.XWhitePixel (dpy, scr);
    ELSIF (r = 0) AND (g = 0) AND (b = 0) THEN
      color.pixel := X.XBlackPixel (dpy, scr);
    ELSE
      (* do a real color lookup *)
      color.red   := Word.And (r, 255) * 256 + 255;
      color.green := Word.And (g, 255) * 256 + 255;
      color.blue  := Word.And (b, 255) * 256 + 255;
      color.flags := Word.Or (X.DoRed, Word.Or (X.DoGreen, X.DoBlue));
      EVAL X.XAllocColor (dpy,
               X.XDefaultColormap (dpy, scr),
               LOOPHOLE (ADR (color), X.XColorStar));
    END;
    RETURN color.pixel;
  END MakeColor;


PROCEDURE Paint (READONLY r: Rect;  color: Color) =
  BEGIN
    X.XSetForeground (dpy, gc, tints [color]);
    X.XFillRectangle (dpy, playWin, gc, r.x, r.y, r.width, r.height);
  END Paint;


(**************************************************************)
(*                  Game primitives                           *)
(**************************************************************)


PROCEDURE ScaleGame (g: Game) =
  (* scale the game to match the aspect ratio *)
  VAR n: INTEGER;  p, q: Piece;
  BEGIN
    IF (g = NIL) THEN RETURN END;

    IF (Aspect_ratio > 1) THEN
      g.speed := g.speed DIV Aspect_ratio;
      g.nRows := g.nRows * Aspect_ratio;
      n := g.nTiles;
      g.nTiles := n * Aspect_ratio;
      FOR i := 0 TO g.nPieces-1 DO
        p := g.pieces[i];
	q := NEW (Piece, n * Aspect_ratio);
        FOR j := 0 TO n-1 DO
	  WITH jj = p[j] DO
            FOR k := 0 TO  Aspect_ratio-1 DO
              WITH qq = q [j * Aspect_ratio + k] DO
	        qq.x := jj.x;
	        qq.y := jj.y * Aspect_ratio - (Aspect_ratio DIV 2) + k;
              END;
	    END;
	  END;
        END;
        g.pieces[i] := q;
      END;
    END;

    (* make sure the game still fits within the constant bounds *)
    IF (g.nRows > Max_rows)        THEN Die (g, "too many rows")    END;
    IF (g.nCols > Max_cols)        THEN Die (g, "too many columns") END;
    IF (g.nPieces > Max_pieces)    THEN Die (g, "too many pieces")  END;
    IF (g.nTiles > Max_tiles)      THEN Die (g, "too many tiles")   END;
    IF (g.nColors >= Max_tints)    THEN Die (g, "too many colors")  END;
  END ScaleGame;

PROCEDURE Die (g: Game;  msg: TEXT) =
  BEGIN
    Wr.PutText (Stdio.stdout, g.name);
    Wr.PutText (Stdio.stdout, ": ");
    Wr.PutText (Stdio.stdout, msg);
    Wr.PutChar (Stdio.stdout, '\n');
    Wr.Close   (Stdio.stdout);
    Cstdlib.exit (-1);
  END Die;


PROCEDURE ResetGame () =
  VAR speedup: REAL;
  BEGIN
    (* init the scalars *)
    speedup  := 1.0 - 0.75 * (FLOAT (level - 1)) / FLOAT (Max_level);
    state    := State.Done;
    counter  := 2;
    delay    := ROUND (FLOAT (game.speed) * speedup);
    running  := FALSE;
    paused   := FALSE;
    score    := 0;
    nWiped   := 0;

    (* start with an empty board *)
    FOR x := 0 TO Max_cols-1 DO
      FOR y := 0 TO Max_rows-1 DO
        board [x][y] := WHITE;
      END;
    END;

    (* and clear the board *)
    MaskButtons ("GpsXLHZ");
    SetLabel (scoreLabel, "Score:  0        ");
    SetLabel (wipeLabel,  "Erased: 0        ");
    Resize ();
    Repaint ();
  END ResetGame;


PROCEDURE SetKeyBindings (on: BOOLEAN) =
  BEGIN
    (* record the global state *)
    one_hand := on;

    (* setup the key mapping *)
    FOR i := FIRST (keymap) TO LAST (keymap) DO keymap[i] := Event.Noop END;
    keymap [' '] := Event.Drop;

    IF (one_hand) THEN
      keymap ['S'] := Event.Move_left;      keymap ['s'] := Event.Move_left;
      keymap ['D'] := Event.Rotate_right;   keymap ['d'] := Event.Rotate_right;
      keymap ['F'] := Event.Move_right;     keymap ['f'] := Event.Move_right; 
      keymap ['J'] := Event.Move_left;      keymap ['j'] := Event.Move_left;
      keymap ['K'] := Event.Rotate_right;   keymap ['k'] := Event.Rotate_right;
      keymap ['L'] := Event.Move_right;     keymap ['l'] := Event.Move_right; 
    ELSE
      keymap ['D'] := Event.Move_left;      keymap ['d'] := Event.Move_left;
      keymap ['F'] := Event.Rotate_left;    keymap ['f'] := Event.Rotate_left;
      keymap ['J'] := Event.Rotate_right;   keymap ['j'] := Event.Rotate_right;
      keymap ['K'] := Event.Move_right;     keymap ['k'] := Event.Move_right; 
    END;

    (* set the window labels *)
    IF (one_hand) THEN
      SetLabel (keyLabel[0], "s, j - move left ");
      SetLabel (keyLabel[1], "d, k - rotate    ");
      SetLabel (keyLabel[2], "f, l - move right");
      SetLabel (keyLabel[3], "<space> - drop   ");
      SetLabel (keyLabel[4], "                 ");
    ELSE
      SetLabel (keyLabel[0], "d - move left    ");
      SetLabel (keyLabel[1], "f - rotate down  ");
      SetLabel (keyLabel[2], "j - rotate up    ");
      SetLabel (keyLabel[3], "k - move right   ");
      SetLabel (keyLabel[4], "<space> - drop   ");
    END;

    (* reset the button *)
    IF (one_hand)
      THEN SetLabel (handButton, " Hands: one      ");
      ELSE SetLabel (handButton, " Hands: two      ");
    END;
  END SetKeyBindings;


PROCEDURE Resize () =
  VAR s: INTEGER;  x, y, width, height: Xt.Position;  args: ArgList;
  BEGIN
    (* get the dimensions of the playing area *)
    args.n := 0;
    AddArgA (args, XtN.x,      ADR (x));
    AddArgA (args, XtN.y,      ADR (y));
    AddArgA (args, XtN.width,  ADR (width));
    AddArgA (args, XtN.height, ADR (height));
    Xt.GetValues (playingArea, args.a, args.n);

    (* find the critical dimension *)
    hUnit := width DIV (game.nCols * Aspect_ratio);
    vUnit := height DIV game.nRows;
    s := MAX (1, MIN (hUnit, vUnit));

    (* set the scaling units *)
    vUnit := s;
    hUnit := s * Aspect_ratio;

    (* and save the domain of the playing area *)
    domain.height := game.nRows * vUnit;
    domain.width  := game.nCols * hUnit;
    domain.x := (width - domain.width) DIV 2;
    domain.y := (height - domain.height) DIV 2;

    (* reset the background of the window *)
    X.XClearWindow (dpy, playWin);
  END Resize;


PROCEDURE PaintTile (x, y: INTEGER;  color: Color) =
  VAR r: Rect;
  BEGIN
    IF (paused) OR (x < 0) OR (y < 0) THEN RETURN END;
    r.x      := domain.x + x * hUnit;
    r.y      := domain.y + y * vUnit;
    r.width  := hUnit;
    r.height := vUnit;
    Paint (r, color);
  END PaintTile;


PROCEDURE RepaintPiece () =
  BEGIN
    FOR i := 0 TO  game.nTiles-1 DO
      PaintTile (loc.x + cur[i].x, loc.y + cur[i].y,
                   curTint [(i+curRot) MOD game.nTiles] );
    END;
  END RepaintPiece;


PROCEDURE Repaint () =
  (* repaints the entire playing area *)
  BEGIN
    (* paint an empty board *)
    Paint (domain, WHITE);
    IF (paused) THEN RETURN END;

    (* paint the fixed pieces *)
    FOR x := 0 TO game.nCols-1 DO
      FOR y := 0 TO game.nRows-1 DO
	IF (board[x][y] # WHITE) THEN PaintTile (x, y, board[x][y]) END;
      END;
    END;

    (* paint the current piece *)
    IF (running) AND (cur # NIL) THEN  RepaintPiece ()  END;
  END Repaint;


PROCEDURE PlacePiece (p, x, y: INTEGER): BOOLEAN =
  VAR
    done: BOOLEAN;
    min, max, x1, y1, z1, nTiles: INTEGER;
    old, new: ARRAY [0..Max_tiles-1] OF Point;
    newPiece: Piece;
  BEGIN
    nTiles := game.nTiles;
    newPiece := game.pieces[p];

    (* map the existing and the new pieces *)
    FOR i := 0 TO nTiles-1 DO
      old[i].x := loc.x + cur[i].x;
      old[i].y := loc.y + cur[i].y;
      new[i].x := x + newPiece[i].x;
      new[i].y := y + newPiece[i].y;
    END;

    (* slide the new piece horizontally until it's on the board *)
    max := 0;  min := game.nCols;
    FOR i := 0 TO nTiles-1 DO
      min := MIN (min, new[i].x);
      max := MAX (max, new[i].x);
    END;
    IF (min < 0) THEN (* slide left *)
      DEC (x, min);  FOR i := 0 TO nTiles-1 DO  DEC (new[i].x, min)  END;
    END;
    max := max - game.nCols + 1;
    IF (max > 0) THEN (* slide right *)
      DEC (x, max);  FOR i := 0 TO nTiles-1 DO  DEC (new[i].x, max) END;
    END;

    (* test for a fit *)
    FOR i := 0 TO nTiles-1 DO
      x1 := new[i].x;  y1 := new[i].y;
      IF (y1 >= game.nRows) THEN RETURN FALSE END;
      IF (y1 >= 0) AND (board[x1][y1] # WHITE) THEN RETURN FALSE END;
    END;

    (* IF we got here, it fits! *)
    curPiece := p;
    cur := game.pieces[p];
    loc.x := x;
    loc.y := y;

    (* erase the old piece *)
    FOR i := 0 TO  nTiles-1 DO
      done := FALSE;  x1 := old[i].x;  y1 := old[i].y;
      FOR j := 0 TO  nTiles-1 DO
        done := (new[j].x = x1) AND (new[j].y = y1);
	IF (done) THEN EXIT END;
      END;
      IF (NOT done) THEN PaintTile (x1, y1, WHITE) END;
    END;

    (* paint the new piece *)
    FOR i := 0 TO  nTiles-1 DO
      done := FALSE;  x1 := new[i].x;  y1 := new[i].y;
      z1 := curTint [(i+curRot) MOD nTiles];
      FOR j := 0 TO  nTiles-1 DO
        done := (old[j].x = x1) AND (old[j].y = y1)
                AND (curTint [(j+curRot) MOD nTiles] = z1);
        IF (done) THEN EXIT END;
      END;
      IF (NOT done) THEN PaintTile (x1, y1, z1) END;
    END;

    RETURN TRUE;
  END PlacePiece;


PROCEDURE MoveDown (): BOOLEAN =
  BEGIN
    RETURN PlacePiece (curPiece, loc.x, loc.y + 1)
  END MoveDown;

PROCEDURE MoveLeft () =
  BEGIN
    EVAL PlacePiece (curPiece, loc.x - 1, loc.y);
  END MoveLeft;

PROCEDURE MoveRight () =
  BEGIN
    EVAL PlacePiece (curPiece, loc.x + 1, loc.y);
  END MoveRight;

PROCEDURE RotateLeft () =
  BEGIN
    curRot := (curRot + Aspect_ratio) MOD game.nTiles;
    RepaintPiece ();
  END RotateLeft;

PROCEDURE RotateRight () =
  BEGIN
    curRot := (curRot + game.nTiles - Aspect_ratio) MOD game.nTiles;
    RepaintPiece ();
  END RotateRight;


PROCEDURE EndOfGame (): BOOLEAN =
  BEGIN
    FOR x := 0 TO game.nCols-1 DO
      IF (board [x][0] # WHITE) THEN
        (* the top row has a non-white cell *)
	StopClock ();
	state    := State.Done;
	counter  := 9999999;
	running  := FALSE;
	paused   := FALSE;
	SetLabel (stateLabel, "State:  DONE     ");
	MaskButtons ("GpsXLHZ");
	RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END EndOfGame;


PROCEDURE ScorePoints (pts: INTEGER) =
  VAR buf: ARRAY [0..29] OF CHAR;  arg: Xt.Arg;
  BEGIN
    INC (score, ROUND (FLOAT (pts) * Score_basis / 
                        FLOAT (delay * Aspect_ratio)));
    Text.SetChars (buf, "Score:  ");
    WITH i = Convert.FromInt (SUBARRAY (buf, 8, 15), score) DO
      buf[i+8] := ' ';  buf[i+9] := '\000';
    END;
    arg.name := XtN.label;
    arg.value := ADR (buf[0]);
    Xt.SetValues (scoreLabel, ADR (arg), 1);
  END ScorePoints;


PROCEDURE AddWipeout (x, y: INTEGER) =
  VAR i: INTEGER;
  BEGIN
    wipeout [nWipeouts].x := x;
    wipeout [nWipeouts].y := y;

    i := 0;
    WHILE (wipeout[i].x # x) OR (wipeout[i].y # y) DO INC (i) END;
    IF (i = nWipeouts) THEN INC (nWipeouts) END;
  END AddWipeout;


PROCEDURE FindHWipeout (x, y: INTEGER) =
  VAR x1: INTEGER;  color: Color;
  BEGIN
    (* remember the color of a match *)
    color := board [x][y];

    (* count the number of matches in a horizontal row *)
    x1 := x;
    WHILE (x1 < game.nCols) AND (board [x1][y] = color) DO INC (x1) END;

    IF ((x1 - x) >= game.nMatches) THEN
      (* we found a wipeout! *)
      x1 := x;
      WHILE (x1 < game.nCols) AND (board [x1][y] = color) DO
        AddWipeout (x1, y);
        INC (x1);
      END;
    END;
  END FindHWipeout;


PROCEDURE FindVWipeout (x, y: INTEGER) =
  VAR y1: INTEGER;  color: Color;
  BEGIN
    (* remember the color of a match *)
    color := board [x][y];

    (* count the number of matches in a vertical row *)
    y1 := y;
    WHILE (y1 < game.nRows) AND (board [x][y1] = color) DO INC (y1) END;

    IF ((y1 - y) >= (Aspect_ratio * game.nMatches)) THEN
      (* we found a wipeout! *)
      y1 := y;
      WHILE (y1 < game.nRows) AND (board [x][y1] = color) DO
        AddWipeout (x, y1);
        INC (y1);
      END;
    END;
  END FindVWipeout;


PROCEDURE FindNEWipeout (x, y: INTEGER) =
  VAR x1, y1: INTEGER;  color: Color;
  BEGIN
    (* remember the color of a match *)
    color := board [x][y];

    (* count the number of matches in a NE-SW diagonal row *)
    x1 := x;
    y1 := y;
    WHILE (x1 < game.nCols) AND (y1 < game.nRows)
      AND (board [x1][y1] = color) DO
      INC (x1);
      INC (y1, Aspect_ratio);
    END;

    IF ((x1 - x) >= game.nMatches) THEN
      (* we found a wipeout! *)
      x1 := x;
      y1 := y;
      WHILE (x1 < game.nCols) AND (y1 < game.nRows)
        AND (board [x1][y1] = color) DO
        AddWipeout (x1, y1);
        INC (x1);
        INC (y1, Aspect_ratio);
      END;
    END;
  END FindNEWipeout;


PROCEDURE FindNWWipeout (x, y: INTEGER) =
  VAR x1, y1: INTEGER;  color: Color;
  BEGIN
    (* remember the color of a match *)
    color := board [x][y];

    (* count the number of matches in a NW-SE diagonal row *)
    x1 := x;
    y1 := y;
    WHILE (x1 < game.nCols) AND (y1 >= 0) AND (board [x1][y1] = color) DO
      INC (x1);
      DEC (y1, Aspect_ratio);
    END;

    IF ((x1 - x) >= game.nMatches) THEN
      (* we found a wipeout! *)
      x1 := x;
      y1 := y;
      WHILE (x1 < game.nCols) AND (y1 >= 0) AND (board [x1][y1] = color) DO
        AddWipeout (x1, y1);
        INC (x1);
        DEC (y1, Aspect_ratio);
      END;
    END;
  END FindNWWipeout;


PROCEDURE FindWipeouts () =
  VAR new, step: INTEGER;  buf: ARRAY [0..29] OF CHAR;  arg: Xt.Arg;
  BEGIN
    (* find the tiles that can be erased *)
    nWipeouts := 0;
    FOR x := 0 TO  game.nCols-1 DO
      FOR y := 0 TO game.nRows-1 DO
        IF (board [x][y] # WHITE) THEN
          FindHWipeout (x, y);
          FindVWipeout (x, y);
          FindNEWipeout (x, y);
          FindNWWipeout (x, y);
        END;
      END;
    END;

    (* score points for the erased tiles *)
    FOR x := 0 TO  nWipeouts-1 DO
      ScorePoints ((game.nRows - wipeout[x].y) * Height_pts);
    END;

    new  := nWiped + (nWipeouts DIV Aspect_ratio);
    step := Speedup_step * game.nMatches;
    IF ((new DIV step) # (nWiped DIV step)) AND (speed_up) THEN
      delay := ROUND (FLOAT (delay) * Speedup);
    END;
    nWiped := new;

    Text.SetChars (buf, "Erased: ");
    WITH i = Convert.FromInt (SUBARRAY (buf, 8, 15), nWiped) DO
      buf[i+8] := ' ';  buf[i+9] := '\000';
    END;
    arg.name := XtN.label;
    arg.value := ADR (buf[0]);
    Xt.SetValues (wipeLabel, ADR (arg), 1);
  END FindWipeouts;


PROCEDURE EndFall () =
  BEGIN
    FindWipeouts ();
    IF (nWipeouts > 0) THEN
      state := State.Blinking;
      counter := 5 * Aspect_ratio;
      BlinkWipeouts (TRUE);
    ELSIF NOT EndOfGame () THEN
      state := State.Pausing;
      counter := 2 * Aspect_ratio;
    END;
  END EndFall;


PROCEDURE StartNewPiece () =
  VAR i, j, k, minx, maxx, maxy: INTEGER;
  BEGIN
    (* check for end of game *)
    IF EndOfGame () THEN RETURN END;

    (* pick a piece *)
    i := Word.And (rand.integer (), 65535);  j := 0;  k := 0;
    WHILE (j < game.nPieces-1) DO
      INC (k, TRUNC (game.freq[j] * 65536.0));
      IF (k > i) THEN EXIT END;
      INC (j);
    END;
    curPiece := j;
    cur := game.pieces[j];

    (* find an initial position *)
    maxx := 0;  maxy := 0;  minx := game.nCols;
    FOR i := 0 TO game.nTiles-1 DO
      minx := MIN (minx, cur[i].x);
      maxx := MAX (maxx, cur[i].x);
      maxy := MAX (maxy, cur[i].y);
    END;
    loc.x := rand.integer ();  IF (loc.x < 0) THEN loc.x := - loc.x END;
    loc.x := (loc.x MOD game.nCols);
    IF (loc.x + maxx >= game.nCols) THEN loc.x := game.nCols - maxx END;
    IF (loc.x + minx < 0) THEN loc.x := -minx END;
    loc.y := -maxy;

    (* assign its colors *)
    FOR i := 0 TO game.nTiles-1 BY Aspect_ratio DO
      j := (rand.integer () MOD game.nColors) + 1;
      FOR k := 0 TO Aspect_ratio-1 DO
        curTint [i+k] := j;
      END;
    END;

    dropped := FALSE;
  END StartNewPiece;


PROCEDURE FixPiece () =
  VAR x, y: INTEGER;
  BEGIN
    FOR i := 0 TO game.nTiles-1 DO
      x := loc.x + cur[i].x;
      y := loc.y + cur[i].y;
      IF (0 <= x) AND (x < game.nCols) AND (0 <= y) AND (y < game.nRows) THEN
        board [x][y] := curTint [(i+curRot) MOD game.nTiles];
      END;
    END;
  END FixPiece;


PROCEDURE BlinkWipeouts (on: BOOLEAN) =
  VAR x, y: INTEGER;
  BEGIN
    IF (on) THEN (* erase wipeouts *)
      FOR i := 0 TO nWipeouts-1 DO
        x := wipeout[i].x;
	y := wipeout[i].y;
        PaintTile (x, y, WHITE);
      END;
    ELSE (* repaint wipeouts *)
      FOR i := 0 TO nWipeouts-1 DO
        x := wipeout[i].x;
	y := wipeout[i].y;
        PaintTile (x, y, board[x][y]);
      END;
    END;
  END BlinkWipeouts;


PROCEDURE CollapseWipeouts () =
  VAR x1, y1, y2, delta: INTEGER;
  BEGIN
    (* mark the wipeouts *)
    FOR i := 0 TO nWipeouts-1 DO
      x1 := wipeout[i].x;
      y1 := wipeout[i].y;
      board [x1][y1] := NO_COLOR;
    END;

    (* repaint the board *)
    FOR x := 0 TO game.nCols-1 DO
      delta := 0;
      FOR y := game.nRows-1 TO 0 BY -1 DO
        IF (board[x][y] = NO_COLOR) THEN
	  PaintTile (x, y, WHITE);
	  board[x][y] := WHITE;
          INC (delta);
        ELSIF (board[x][y] = WHITE) THEN
          INC (delta);
	ELSIF (delta > 0) THEN
	  y2 := y + delta;
	  board [x][y2] := board[x][y];
	  board [x][y] := WHITE;
          PaintTile (x, y2, board[x][y2]);
	  PaintTile (x, y, WHITE);
	ELSE
          (* no change *)
	END;
      END;
    END;
  END CollapseWipeouts;


PROCEDURE Advance (ev: Event) =
  BEGIN
    CASE (ev) OF
    | Event.Noop =>
        (* do nothing *)

    | Event.Tick =>
        CASE (state) OF
        | State.Falling =>
            IF NOT MoveDown () THEN
              state := State.Resting;
              counter := 2 * Aspect_ratio;
	    END

        | State.Resting =>
            IF NOT MoveDown () THEN
              DEC (counter);
              IF (counter <= 0) THEN FixPiece ();  EndFall (); END;
            ELSE
              state := State.Falling;
            END;

        | State.Blinking =>
            DEC (counter);
            IF (counter <= 0) THEN
              CollapseWipeouts ();
	      EndFall ();
            ELSIF ((counter MOD Aspect_ratio) = 0) THEN
              BlinkWipeouts ((counter MOD 2) = 1);
            END;

        | State.Pausing =>
            DEC (counter);
            IF (counter <= 0) THEN
              state := State.Falling;
              StartNewPiece ();
            END;

	| State.Done =>
        END;

    | Event.Drop =>
        IF (NOT dropped) THEN
          ScorePoints ((game.nRows - loc.y) * Height_pts);
	  dropped := TRUE;
        END;
        IF (state = State.Falling) THEN
          WHILE MoveDown () DO (* plummet *) END;
          counter := 1;
        END;

    | Event.Move_left =>
        IF (state = State.Falling) OR (state = State.Resting) THEN
          MoveLeft ();
        END;

    | Event.Rotate_left =>
        IF (state = State.Falling) OR (state = State.Resting) THEN
          RotateLeft ();
        END;

    | Event.Rotate_right =>
        IF (state = State.Falling) OR (state = State.Resting) THEN
	  RotateRight ();
        END;

    | Event.Move_right =>
        IF (state = State.Falling) OR (state = State.Resting) THEN
	  MoveRight ();
        END;

    ELSE (* ignore event *)
    END;
  END Advance;


(**************************************************************)
(*                  Event handlers                            *)
(**************************************************************)

PROCEDURE ClockTick (<*UNUSED*> arg: Xt.Pointer;
                     <*UNUSED*> id: Xt.IntervalIdStar) =
  BEGIN
    IF NOT (paused) THEN
      IF (running) THEN
        Advance (Event.Tick);
        StartClock ();
      END;
    END;
  END ClockTick;

PROCEDURE GoPressed (<*UNUSED*> w: Xt.Widget;
                     <*UNUSED*> data, env: Xt.Pointer) =
  BEGIN
    IF (state = State.Done) AND (NOT running) THEN
      ResetGame ();
      state   := State.Pausing;
      counter := 2;
      running := TRUE;
      SetLabel (stateLabel, "State:  RUNNING  ");
      MaskButtons ("gPSxlhz");
    ELSIF (paused) THEN
      paused := FALSE;
      SetLabel (stateLabel, "State:  RUNNING  ");
      MaskButtons ("gPSxlhz");
      Repaint ();
      Advance (Event.Drop);
    END;
    GrabFocus ();
    StartClock ();
  END GoPressed;

PROCEDURE PausePressed (<*UNUSED*> w: Xt.Widget; 
                        <*UNUSED*> data, env: Xt.Pointer) =
  BEGIN
    IF (paused) OR (NOT running) THEN RETURN END;
    StopClock ();
    paused := TRUE;
    SetLabel (stateLabel, "State:  PAUSED   ");
    MaskButtons ("GpSxlhz");
    Repaint ();
  END PausePressed;

PROCEDURE StopPressed (<*UNUSED*> w: Xt.Widget; 
                       <*UNUSED*> data, env: Xt.Pointer) =
  BEGIN
     StopClock ();
     state    := State.Done;
     counter  := 9999999;
     running  := FALSE;
     paused   := FALSE;
     SetLabel (stateLabel, "State:  DONE     ");
     MaskButtons ("GpsXLHZ");
  END StopPressed;

PROCEDURE LevelPressed (<*UNUSED*> w: Xt.Widget; 
                        <*UNUSED*> data, env: Xt.Pointer) =
  VAR label: Text.T;
  BEGIN
    IF (running) OR (paused) THEN RETURN END;
    level := (level MOD Max_level) + 1;
    label := Text.Cat (" Level: ", Fmt.Int (level));
    SetLabel (levelButton, Fmt.Pad (label, 17, ' ', Fmt.Align.Left));
  END LevelPressed;

PROCEDURE GamePressed (<*UNUSED*> w: Xt.Widget; 
                       <*UNUSED*> data, env: Xt.Pointer) =
  VAR label: Text.T;
  BEGIN
    IF (running) OR (paused) THEN RETURN END;
    gameID := (gameID + 1) MOD NUMBER (all_games);
    game := all_games[gameID];
    label :=  Text.Cat (" Game: ", game.name);
    SetLabel (gameButton, Fmt.Pad (label, 17, ' ', Fmt.Align.Left));
    ResetGame ();
  END GamePressed;

PROCEDURE HandPressed (<*UNUSED*> w: Xt.Widget; 
                       <*UNUSED*> data, env: Xt.Pointer) =
  BEGIN
    IF (running) OR (paused) THEN RETURN END;
    one_hand := NOT one_hand;
    SetKeyBindings (one_hand);
  END HandPressed;

PROCEDURE SpeedPressed (<*UNUSED*> w: Xt.Widget;
		        <*UNUSED*> data, env: Xt.Pointer) =
  BEGIN
    IF (running) OR (paused) THEN RETURN END;
    speed_up := NOT speed_up;
    IF speed_up
      THEN SetLabel (speedButton, " Speedup: on     ");
      ELSE SetLabel (speedButton, " Speedup: off    ");
    END;
  END SpeedPressed;

PROCEDURE QuitPressed (<*UNUSED*> w: Xt.Widget;
		       <*UNUSED*> data, env: Xt.Pointer) =
  BEGIN
(*IF (kbstate.global_auto_repeat = ORD(TRUE)) THEN X.XAutoRepeatOn (dpy) END;*)
    Cstdlib.exit (0);
  END QuitPressed;

PROCEDURE KeyClick (<*UNUSED*> w     : Xt.Widget;
                    <*UNUSED*> tag   : Xt.Pointer;
                               event : X.XAnyEventStar;
                    <*UNUSED*> cont  : Xt.BooleanStar) =
  VAR i: INTEGER;  buf: ARRAY [0..31] OF CHAR;
  BEGIN
   CASE (event^.type) OF
   | X.ButtonPress =>
      WITH z = LOOPHOLE (event, X.XButtonEventStar)^ DO
        CASE (z.button) OF
        | X.Button1 =>  Advance (Event.Move_left);
        | X.Button2 =>  Advance (Event.Rotate_left);
        | X.Button3 =>  Advance (Event.Move_right);
        ELSE            (* do nothing *)
        END;
      END;

   | X.KeyPress =>
      WITH z = LOOPHOLE (event, X.XKeyEventStar) DO
        i := X.XLookupString (z, ADR (buf[0]), BYTESIZE (buf), NIL, NIL);
      END;
      (*** IF (i <= 0) THEN RETURN END; ***)
      Advance (keymap[buf[0]]);

   ELSE (* ignore event *)
   END;
  END KeyClick;

PROCEDURE DoExpose (<*UNUSED*> w: Xt.Widget;  <*UNUSED*> tag: Xt.Pointer;
                    <*UNUSED*> event: X.XAnyEventStar; 
                    <*UNUSED*>cont: Xt.BooleanStar) =
  BEGIN
    Resize ();
    Repaint ();
  END DoExpose;

PROCEDURE MoveFocus (<*UNUSED*> w: Xt.Widget;  <*UNUSED*> tag: Xt.Pointer;
		     event: X.XAnyEventStar;
                     <*UNUSED*> cont: Xt.BooleanStar) =
  BEGIN
    CASE (event^.type) OF
    | X.EnterNotify =>
        (**** IF (running) THEN StartClock () END; ****)
        GrabFocus ();

    | X.LeaveNotify =>
        DropFocus ();
        (*** IF (running) THEN StopClock () END; ***)

    ELSE (* ignore *)
    END;
  END MoveFocus;

(**************************************************************)
(*                  main program                              *)
(**************************************************************)


PROCEDURE Init () =
  VAR args: ArgList;
  BEGIN
    (* build the game descriptions & scale them to the current aspect ratio *)
    all_games[0] := Columns.Build ();
    all_games[1] := Rows.Build ();
    all_games[2] := Bars.Build ();
    all_games[3] := Squares.Build ();
    all_games[4] := Threes.Build ();
    FOR i := 0 TO LAST (all_games) DO ScaleGame (all_games[i]) END;
    game := all_games[0];
    gameID := 0;

    (* seed the random number generator *)
    rand := NEW (Random.Default).init (fixed := FALSE);

    (* top-level window *)
    VAR
      name := TtoS ("Columns");
      argc : Xt.Cardinal := RTLinker.info.argc;
      argv : X.Argv := RTLinker.info.argv;
    BEGIN
      topWindow := Xt.Initialize (name, name, NIL, 0, argc, argv);
    END;

    args.n := 0;
    AddArgI (args, XtN.allowShellResize, ORD (TRUE));
    AddArgI (args, XtN.input,            ORD (TRUE));
    Xt.SetValues (topWindow, args.a, args.n);

    args.n := 0;
    AddArgI (args, XtN.borderWidth, 0);
    AddArgI (args, XtN.sensitive,   ORD (TRUE));
    AddArgI (args, XtN.orientation, Xmu.orientHorizontal);
    AddArgI (args, XtN.input,       ORD (TRUE));
    box := Xt.CreateManagedWidget (TtoS ("outerbox"), Xaw.panedWidgetClass,
                                   topWindow, args.a, args.n);

    (* create the column of labels & buttons *)
    args.n := 0;
    AddArgI (args, XtN.borderWidth, 0);
    AddArgI (args, XtN.input,       ORD (TRUE));
    AddArgI (args, XtN.orientation, Xmu.orientVertical);
    AddArgI (args, XtN.sensitive,   ORD (TRUE));
    AddArgI (args, XtN.showGrip,    ORD (FALSE));
    form := Xt.CreateManagedWidget (TtoS ("form"), Xaw.boxWidgetClass, box,
                                      args.a, args.n);

    scoreLabel  := CreateLabel  ("Score:  0        ");
    wipeLabel   := CreateLabel  ("Erased: 0        ");
    stateLabel  := CreateLabel  ("State:  READY    ");
    EVAL           CreateLabel  ("                 ");
    keyLabel[0] := CreateLabel  ("                 ");
    keyLabel[1] := CreateLabel  ("                 ");
    keyLabel[2] := CreateLabel  ("                 ");
    keyLabel[3] := CreateLabel  ("                 ");
    keyLabel[4] := CreateLabel  ("                 ");
    EVAL           CreateLabel  ("                 ");
    goButton    := CreateButton (" Go              ", GoPressed);
    pauseButton := CreateButton (" Pause           ", PausePressed);
    stopButton  := CreateButton (" Stop            ", StopPressed);
    gameButton  := CreateButton (" Game: Columns   ", GamePressed);
    levelButton := CreateButton (" Level: 1        ", LevelPressed);
    handButton  := CreateButton (" Hands: one      ", HandPressed);
    speedButton := CreateButton (" Speedup: on     ", SpeedPressed);
    EVAL           CreateLabel  ("                 ");
    quitButton  := CreateButton (" Quit            ", QuitPressed);

    (* playing area *)
    args.n := 0;
    AddArgI (args, XtN.width,         202);
    AddArgI (args, XtN.height,        402);
    AddArgI (args, XtN.borderWidth,   0);
    AddArgI (args, XtN.sensitive,     ORD (TRUE));
    AddArgI (args, XtN.input,         ORD (TRUE));
    AddArgI (args, XtN.showGrip,      ORD (FALSE));
    playingArea := Xt.CreateManagedWidget (TtoS ("playingArea"),
                               Xaw.simpleWidgetClass, box, args.a, args.n);
    Xt.AddEventHandler (playingArea, X.ExposureMask, ORD (FALSE), DoExpose);

    MaskButtons ("GpsXLHZ");
    Xt.RealizeWidget (topWindow);
    dpy := Xt.Display (topWindow);
    win := Xt.XtWindow (topWindow);
    gc := X.XCreateGC (dpy, win, 0, NIL);
    playWin := Xt.XtWindow (playingArea);

    (* build the colors *) 
    tints [ 0] := MakeColor (255, 255, 255); (* white *)
    tints [ 1] := MakeColor (255, 160,   0); (* orange *)
    tints [ 2] := MakeColor (255, 255,   0); (* yellow *)
    tints [ 3] := MakeColor (  0,   0, 255); (* blue *)
    tints [ 4] := MakeColor (255,   0,   0); (* red *)
    tints [ 5] := MakeColor (  0, 255,   0); (* green *)
    tints [ 6] := MakeColor (255,   0, 255); (* purple *)
    tints [ 7] := MakeColor (222, 215, 108); (* tan/khaki *)
    tints [ 8] := MakeColor (  0, 255, 255); (* light blue *)
    tints [ 9] := MakeColor ( 35,  35, 142); (* navy blue *)
    tints [10] := MakeColor (219, 219, 112); (* goldenrod *)
    tints [11] := MakeColor ( 50, 204,  50); (* lime green *)
    tints [12] := MakeColor (168, 168, 168); (* light gray *)
    tints [13] := MakeColor (159,  95, 159); (* blue violet *)
    tints [14] := MakeColor (216, 216, 185); (* wheat *)
    tints [15] := MakeColor (187,  27, 194); (* light purple *)
    tints [16] := MakeColor (189,  60,  60); (* brick *)
    tints [17] := MakeColor (187,  87, 113); (* sick purple *)
    tints [18] := MakeColor (255, 216,   0); (* gold *)
    FOR i := 19 TO LAST (tints) DO
      tints [i] := MakeColor (rand.integer(), rand.integer(), rand.integer());
    END;
    tints [BLACK] := MakeColor (0, 0, 0); (* black *)

    SetKeyBindings (one_hand);
    ResetGame ();

    X.XGetKeyboardControl (dpy, ADR (kbstate));

    (* catch key strokes (wherever they might be delivered! *)
    Xt.AddEventHandler (topWindow, X.KeyPressMask + X.ButtonPressMask,
                           ORD (FALSE), KeyClick, NIL);
    Xt.AddEventHandler (box, X.KeyPressMask + X.ButtonPressMask,
                           ORD (FALSE), KeyClick, NIL);
    Xt.AddEventHandler (form, X.KeyPressMask + X.ButtonPressMask,
                           ORD (FALSE), KeyClick, NIL);
    Xt.AddEventHandler (playingArea, X.KeyPressMask + X.ButtonPressMask,
                           ORD (FALSE), KeyClick, NIL);

    (* watch for coming and going focus *)
    Xt.AddEventHandler (topWindow, X.EnterWindowMask + X.LeaveWindowMask,
                           ORD (FALSE), MoveFocus, NIL);

  END Init;

BEGIN
  Init ();
  (* GrabFocus (); *)
  Xt.MainLoop ();
END Main.
