(* Copyright (C) 1992, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Fri Nov 12 16:53:54 PST 1993 by kalsow  *)
(*      modified on Tue Mar 17 14:53:11 PST 1992 by muller  *)

MODULE Main;

(* Modula-3 core *)
IMPORT Axis, Date, Env, Fmt, FS, IntArraySort, IO, OSConfig, OSError;
IMPORT Params, Point, Process, Random, Rect, Region, Stdio, Text;
IMPORT TextWr, Thread, Time, Word, Wr;

(* UI toolkit *)
IMPORT BorderedVBT, ButtonVBT, Font, HVSplit, Latin1Key, PaintOp, RigidVBT;
IMPORT Split, TextVBT, Trestle, TrestleComm, TSplit, VBT;

(* Columns *)
IMPORT Columns, Rows, Bars, ScoreDir, ScoreFile, Squares, Threes;
FROM Config IMPORT Piece, Game;

CONST
  ALIAS        = "GAMEALIAS";
  MaxScores    = 9;
  Margin       = 10;
  Aspect_ratio = 3;    (* hUnit / vUnit *)
  MinCell      = 6;
  Score_basis  = 0.065645d0;
  Height_pts   = 10;
  Speedup      = 0.95d0;
  Speedup_step = 5;
  Max_rows     = (30 * Aspect_ratio);
  Max_cols     = 30;
  Max_pieces   = 70;
  Max_tints    = 36;
  Max_tiles    = 30;
  Max_level    = 5;

  Title1    = "                    total            best                    ";
  Title2    = "                ------------  -------------------------------";
  Title3    = "player          games  HH:MM   score  erased lvl date            ";
  Title4    = "--------------- ----- ------  ------- ------ --- ----------------";
  BlankLine = "                                                             ";

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

(* misc. flags & global parameters *)
VAR  speed_up := TRUE;
VAR  one_hand := TRUE;
VAR  running  := FALSE;
VAR  paused   := FALSE;
VAR  level    := 1;
VAR  keymap   : ARRAY [0..255] OF Event;
VAR  rand     : Random.T;

(* clock *)
VAR delay: Time.T := 0.0d0;

(* game state *)
VAR  all_games : ARRAY [0 .. 4] OF Game;
VAR  gameID    : INTEGER;
VAR  game      : Game;
VAR  state     := State.Done;
VAR  counter   := 0;
VAR  curPiece  := 0; (* piece *)
VAR  cur       : Piece;
VAR  curScored : BOOLEAN;
VAR  loc       : Point.T;
VAR  curRot    := 0;
VAR  dropped   := FALSE;
VAR  nWipeouts := 0;
VAR  board     : ARRAY [0 .. Max_cols-1], [0 .. Max_rows-1] OF Color;
VAR  wipeout   : ARRAY [0 .. Max_cols * Max_rows -1] OF Point.T;
VAR  curTint   : ARRAY [0 .. Max_tiles] OF Color;

VAR  startTime : Time.T;
VAR  stopTime  : Time.T;
VAR  pauseTime : Time.T;

(* scoring *)
VAR  score     := 0;
VAR  nWiped    := 0;

(* graphics state *)
VAR  focus       := FALSE;
VAR  domain      := Rect.T { 0,0,0,0 };
VAR  vUnit       := 0;
VAR  hUnit       := 0;
VAR  tints       : ARRAY Color OF PaintOp.T;
VAR  VBT_White   : PaintOp.T;
VAR  VBT_Black   : PaintOp.T;
VAR  chassis     : VBT.T;
VAR  gameVBT     : VBT.T;
VAR  scoresVBT   : VBT.T;
VAR  scoreRows   : ARRAY [0..MaxScores+1] OF VBT.T;
VAR  boardVBT    : VBT.T;
VAR  goButton    : VBT.T;
VAR  goTitle     : VBT.T;
VAR  pauseButton : VBT.T;
VAR  pauseTitle  : VBT.T;
VAR  levelButton : VBT.T;
VAR  levelTitle  : VBT.T;
VAR  gameButton  : VBT.T;
VAR  gameTitle   : VBT.T;
VAR  handButton  : VBT.T;
VAR  handTitle   : VBT.T;
VAR  speedButton : VBT.T;
VAR  speedTitle  : VBT.T;
VAR  scoreButton : VBT.T;
VAR  scoreTitle  : VBT.T;
VAR  quitButton  : VBT.T;
VAR  quitTitle   : VBT.T;
VAR  keyLabel    : ARRAY [0..4] OF VBT.T;
VAR  wipeLabel   : VBT.T;
VAR  scoreLabel  : VBT.T;
VAR  stateLabel  : VBT.T;

VAR  northE      : Rect.T;
VAR  westE       : Rect.T;
VAR  southE      : Rect.T;
VAR  eastE       : Rect.T;

(* threads *)
VAR  clock     : Thread.T;
VAR  machine   : Thread.T;


(*-------------------------------------------------------- VBT constructors ---*)

PROCEDURE Gap (h, v: REAL := 2.0): VBT.T =
  BEGIN
    RETURN RigidVBT.FromHV (TextVBT.New (""), h, v);
    (* an h X v mm white space *)
  END Gap;

PROCEDURE NewButton (name: TEXT;  proc: ButtonVBT.Proc;  VAR label: VBT.T): VBT.T =
  BEGIN
    label := TextVBT.New (name);
    RETURN ButtonVBT.New (BorderedVBT.New (label), proc);
  END NewButton;

VAR
  needFixed : BOOLEAN := TRUE;
  fixedFont : Font.T;

PROCEDURE NewLabel (t: TEXT): VBT.T =
  BEGIN
    IF (needFixed) THEN
      needFixed := FALSE;
      fixedFont := Font.FromName (
                     ARRAY OF TEXT{"-*-courier-medium-r-*-*-*-120-*"});
    END;
    RETURN TextVBT.New (t, 0.0, fnt := fixedFont);
  END NewLabel;

PROCEDURE SetLabel (v: VBT.T;  txt: TEXT) =
  BEGIN
    TextVBT.Put (v, txt);
  END SetLabel;

PROCEDURE VBTRow (a0, a1, a2: VBT.T := NIL): VBT.T =
  BEGIN
    RETURN HVSplit.Cons (Axis.T.Hor, a0, a1, a2, TextVBT.New (" "));
  END VBTRow;

PROCEDURE VBTCol ( a0, a1, a2, a3, a4, a5, a6, a7, a8, a9,
                  a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,
                  a20,a21,a22,a23,a24                      : VBT.T:=NIL): VBT.T =
  VAR v: VBT.T;
  BEGIN
    v := HVSplit.Cons (Axis.T.Ver, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9);
    IF (a10 # NIL) THEN Split.AddChild (v, a10) END;
    IF (a11 # NIL) THEN Split.AddChild (v, a11) END;
    IF (a12 # NIL) THEN Split.AddChild (v, a12) END;
    IF (a13 # NIL) THEN Split.AddChild (v, a13) END;
    IF (a14 # NIL) THEN Split.AddChild (v, a14) END;
    IF (a15 # NIL) THEN Split.AddChild (v, a15) END;
    IF (a16 # NIL) THEN Split.AddChild (v, a16) END;
    IF (a17 # NIL) THEN Split.AddChild (v, a17) END;
    IF (a18 # NIL) THEN Split.AddChild (v, a18) END;
    IF (a19 # NIL) THEN Split.AddChild (v, a19) END;
    IF (a20 # NIL) THEN Split.AddChild (v, a20) END;
    IF (a21 # NIL) THEN Split.AddChild (v, a21) END;
    IF (a22 # NIL) THEN Split.AddChild (v, a22) END;
    IF (a23 # NIL) THEN Split.AddChild (v, a23) END;
    IF (a24 # NIL) THEN Split.AddChild (v, a24) END;
    Split.AddChild (v, TextVBT.New (" "));
    RETURN v;
  END VBTCol;

PROCEDURE MakeColor (r, g, b: INTEGER): PaintOp.T =
  VAR
    rr := FLOAT (Word.And (r, 255)) / 255.0;
    gg := FLOAT (Word.And (g, 255)) / 255.0;
    bb := FLOAT (Word.And (b, 255)) / 255.0;
  BEGIN
    RETURN PaintOp.Pair (VBT_White, PaintOp.FromRGB (rr, gg, bb));
  END MakeColor;

(*-------------------------------------------------------- keyboard focus ---*)

PROCEDURE GetFocus (t: VBT.TimeStamp) =
  BEGIN
    IF (NOT focus) THEN
      TRY
        VBT.Acquire (boardVBT, VBT.KBFocus, t);
        focus := TRUE;
      EXCEPT VBT.Error =>
      END;
    END;
  END GetFocus;

PROCEDURE DropFocus () =
  BEGIN
    IF (focus) THEN
      VBT.Release (boardVBT, VBT.KBFocus);
      focus := FALSE;
    END;
  END DropFocus;

(*------------------------------------------------------- Game primitives ---*)

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
	        qq.h := jj.h;
	        qq.v := jj.v * Aspect_ratio - (Aspect_ratio DIV 2) + k;
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
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    Wr.PutText (Stdio.stdout, g.name);
    Wr.PutText (Stdio.stdout, ": ");
    Wr.PutText (Stdio.stdout, msg);
    Wr.PutText (Stdio.stdout, Wr.EOL);
    Wr.Close   (Stdio.stdout);
    Process.Exit (1);
  END Die;


PROCEDURE ResetGame () =
  VAR speedup: LONGREAL;
  BEGIN
    (* init the scalars *)
    speedup  := 1.0d0 - 0.75d0 * (FLOAT (level - 1, LONGREAL))
                                  / FLOAT (Max_level, LONGREAL);
    state    := State.Done;
    counter  := 2;
    delay    := FLOAT (game.speed, LONGREAL) * speedup / 1000.0d0;
    running  := FALSE;
    paused   := FALSE;
    score    := 0;
    nWiped   := 0;
    cur      := NIL;

    (* start with an empty board *)
    FOR x := 0 TO Max_cols-1 DO
      FOR y := 0 TO Max_rows-1 DO
        board [x][y] := WHITE;
      END;
    END;

    (* and clear the board *)
    SetLabel (scoreLabel, "0        ");
    SetLabel (wipeLabel,  "0        ");
    Resize (VBT.Domain (boardVBT));
    VBT.ForceRepaint (boardVBT, Region.Full);
  END ResetGame;


PROCEDURE SetKeyBindings (on: BOOLEAN) =
  BEGIN
    (* record the global state *)
    one_hand := on;

    (* setup the key mapping *)
    FOR i := FIRST (keymap) TO LAST (keymap) DO keymap[i] := Event.Noop END;
    keymap [Latin1Key.space] := Event.Drop;

    IF (one_hand) THEN
      keymap [Latin1Key.S] := Event.Move_left;
      keymap [Latin1Key.s] := Event.Move_left;
      keymap [Latin1Key.D] := Event.Rotate_right;
      keymap [Latin1Key.d] := Event.Rotate_right;
      keymap [Latin1Key.F] := Event.Move_right;
      keymap [Latin1Key.f] := Event.Move_right; 
      keymap [Latin1Key.J] := Event.Move_left;
      keymap [Latin1Key.j] := Event.Move_left;
      keymap [Latin1Key.K] := Event.Rotate_right;
      keymap [Latin1Key.k] := Event.Rotate_right;
      keymap [Latin1Key.L] := Event.Move_right;
      keymap [Latin1Key.l] := Event.Move_right; 
    ELSE
      keymap [Latin1Key.D] := Event.Move_left;
      keymap [Latin1Key.d] := Event.Move_left;
      keymap [Latin1Key.F] := Event.Rotate_left;
      keymap [Latin1Key.f] := Event.Rotate_left;
      keymap [Latin1Key.J] := Event.Rotate_right;
      keymap [Latin1Key.j] := Event.Rotate_right;
      keymap [Latin1Key.K] := Event.Move_right;
      keymap [Latin1Key.k] := Event.Move_right; 
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
      THEN SetLabel (handTitle, " Hands: one      ");
      ELSE SetLabel (handTitle, " Hands: two      ");
    END;
  END SetKeyBindings;


PROCEDURE Resize (READONLY r: Rect.T) =
  CONST BorderWidth = 2;
  VAR h, v, s: INTEGER;  p: Point.T;
  BEGIN
    (* find the critical dimension *)
    h := (Rect.HorSize (r) - Margin) DIV (game.nCols * Aspect_ratio);
    v := (Rect.VerSize (r) - Margin) DIV (game.nRows);
    s := MAX (MinCell, MIN (h, v));

    (* set the scaling units *)
    vUnit := s;
    hUnit := s * Aspect_ratio;

    (* find the center and full extent of the new board *)
    p := Rect.Middle (r);
    h := game.nCols * hUnit;
    v := game.nRows * vUnit;

    (* and save the domain of the playing area *)
    domain.north := p.v - v DIV 2;
    domain.west  := p.h - h DIV 2;
    domain.south := domain.north + v;
    domain.east  := domain.west + h;

    (* fix the edge boundaries *)
    northE.north := domain.north - BorderWidth;
    northE.south := domain.north;
    northE.west  := domain.west - BorderWidth;
    northE.east  := domain.east + BorderWidth;

    southE.north := domain.south;
    southE.south := domain.south + BorderWidth;
    southE.west  := domain.west - BorderWidth;
    southE.east  := domain.east + BorderWidth;

    westE.north := domain.north;
    westE.south := domain.south;
    westE.west  := domain.west - BorderWidth;
    westE.east  := domain.west;

    eastE.north := domain.north;
    eastE.south := domain.south;
    eastE.west  := domain.east;
    eastE.east  := domain.east + BorderWidth;

    (* reset the background of the window *)
    VBT.PaintTint (boardVBT, r, VBT_White);
  END Resize;


PROCEDURE Paint (READONLY r: Rect.T;  color: Color) =
  BEGIN
    VBT.PaintTint (boardVBT, r, tints [color]);
  END Paint;


PROCEDURE PaintTile (x, y: INTEGER;  color: Color) =
  VAR r: Rect.T;
  BEGIN
    IF (paused) OR (x < 0) OR (y < 0) THEN RETURN END;
    r.west   := domain.west  + x * hUnit;
    r.north  := domain.north + y * vUnit;
    r.east   := r.west + hUnit;
    r.south  := r.north + vUnit;
    Paint (r, color);
  END PaintTile;


PROCEDURE RepaintPiece () =
  BEGIN
    FOR i := 0 TO  game.nTiles-1 DO
      PaintTile (loc.h + cur[i].h, loc.v + cur[i].v,
                   curTint [(i+curRot) MOD game.nTiles] );
    END;
  END RepaintPiece;

PROCEDURE PlacePiece (p, x, y: INTEGER): BOOLEAN =
  VAR
    done: BOOLEAN;
    min, max, x1, y1, z1, nTiles: INTEGER;
    old, new: ARRAY [0..Max_tiles-1] OF Point.T;
    newPiece: Piece;
  BEGIN
    nTiles := game.nTiles;
    newPiece := game.pieces[p];

    (* map the existing and the new pieces *)
    FOR i := 0 TO nTiles-1 DO
      old[i].h := loc.h + cur[i].h;
      old[i].v := loc.v + cur[i].v;
      new[i].h := x + newPiece[i].h;
      new[i].v := y + newPiece[i].v;
    END;

    (* slide the new piece horizontally until it's on the board *)
    max := 0;  min := game.nCols;
    FOR i := 0 TO nTiles-1 DO
      min := MIN (min, new[i].h);
      max := MAX (max, new[i].h);
    END;
    IF (min < 0) THEN (* slide left *)
      DEC (x, min);  FOR i := 0 TO nTiles-1 DO  DEC (new[i].h, min)  END;
    END;
    max := max - game.nCols + 1;
    IF (max > 0) THEN (* slide right *)
      DEC (x, max);  FOR i := 0 TO nTiles-1 DO  DEC (new[i].h, max) END;
    END;

    (* test for a fit *)
    FOR i := 0 TO nTiles-1 DO
      x1 := new[i].h;  y1 := new[i].v;
      IF (y1 >= game.nRows) THEN RETURN FALSE END;
      IF (y1 >= 0) AND (board[x1][y1] # WHITE) THEN RETURN FALSE END;
    END;

    (* IF we got here, it fits! *)
    curPiece := p;
    cur := game.pieces[p];
    loc.h := x;
    loc.v := y;

    (* erase the old piece *)
    FOR i := 0 TO  nTiles-1 DO
      done := FALSE;  x1 := old[i].h;  y1 := old[i].v;
      FOR j := 0 TO  nTiles-1 DO
        done := (new[j].h = x1) AND (new[j].v = y1);
	IF (done) THEN EXIT END;
      END;
      IF (NOT done) THEN PaintTile (x1, y1, WHITE) END;
    END;

    (* paint the new piece *)
    FOR i := 0 TO  nTiles-1 DO
      done := FALSE;  x1 := new[i].h;  y1 := new[i].v;
      z1 := curTint [(i+curRot) MOD nTiles];
      FOR j := 0 TO  nTiles-1 DO
        done := (old[j].h = x1) AND (old[j].v = y1)
                AND (curTint [(j+curRot) MOD nTiles] = z1);
        IF (done) THEN EXIT END;
      END;
      IF (NOT done) THEN PaintTile (x1, y1, z1) END;
    END;

    RETURN TRUE;
  END PlacePiece;


PROCEDURE MoveDown (): BOOLEAN =
  BEGIN
    RETURN PlacePiece (curPiece, loc.h, loc.v + 1)
  END MoveDown;

PROCEDURE MoveLeft () =
  BEGIN
    EVAL PlacePiece (curPiece, loc.h - 1, loc.v);
  END MoveLeft;

PROCEDURE MoveRight () =
  BEGIN
    EVAL PlacePiece (curPiece, loc.h + 1, loc.v);
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
	RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END EndOfGame;

PROCEDURE EndGame () =
  BEGIN
    stopTime := Time.Now ();
    state    := State.Done;
    counter  := 9999999;
    running  := FALSE;
    paused   := FALSE;
    UpdateScore ();
    SetLabel (stateLabel, "DONE     ");
    SetLabel (goTitle, "Go  ");
  END EndGame;


PROCEDURE ScorePiece (pts: INTEGER) =
  BEGIN
    IF NOT curScored THEN
      ScorePoints (pts);
      curScored := TRUE;
    END;
  END ScorePiece;

PROCEDURE ScorePoints (pts: INTEGER) =
  BEGIN
    INC (score, ROUND (FLOAT (pts, LONGREAL) * Score_basis
                       / (delay * FLOAT (Aspect_ratio, LONGREAL))));
    SetLabel (scoreLabel, Fmt.Int (score));
  END ScorePoints;


PROCEDURE AddWipeout (x, y: INTEGER) =
  VAR i: INTEGER;
  BEGIN
    wipeout [nWipeouts].h := x;
    wipeout [nWipeouts].v := y;

    i := 0;
    WHILE (wipeout[i].h # x) OR (wipeout[i].v # y) DO INC (i) END;
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
  VAR new, step: INTEGER;
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
      ScorePoints ((game.nRows - wipeout[x].v) * Height_pts);
    END;

    new  := nWiped + (nWipeouts DIV Aspect_ratio);
    step := Speedup_step * game.nMatches;
    IF ((new DIV step) # (nWiped DIV step)) AND (speed_up) THEN
      delay := delay * Speedup;
    END;
    nWiped := new;

    SetLabel (wipeLabel, Fmt.Int (nWiped));
  END FindWipeouts;


PROCEDURE EndFall () =
  BEGIN
    FindWipeouts ();
    IF (nWipeouts > 0) THEN
      state := State.Blinking;
      counter := 5 * Aspect_ratio;
      BlinkWipeouts (TRUE);
    ELSE
      state := State.Pausing;
      counter := 2 * Aspect_ratio;
    END;
  END EndFall;


PROCEDURE StartNewPiece () =
  VAR i, j, k, minx, maxx, maxy: INTEGER;
  BEGIN
    (* check for end of game *)
    IF EndOfGame () THEN EndGame ();  RETURN END;

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
      minx := MIN (minx, cur[i].h);
      maxx := MAX (maxx, cur[i].h);
      maxy := MAX (maxy, cur[i].v);
    END;
    loc.h := rand.integer ();  IF (loc.h < 0) THEN loc.h := - loc.h END;
    loc.h := (loc.h MOD game.nCols);
    IF (loc.h + maxx >= game.nCols) THEN loc.h := game.nCols - maxx END;
    IF (loc.h + minx < 0) THEN loc.h := -minx END;
    loc.v := -maxy;

    (* assign its colors *)
    FOR i := 0 TO game.nTiles-1 BY Aspect_ratio DO
      j := (rand.integer () MOD game.nColors) + 1;
      FOR k := 0 TO Aspect_ratio-1 DO
        curTint [i+k] := j;
      END;
    END;

    dropped := FALSE;
    curScored := FALSE;
  END StartNewPiece;


PROCEDURE FixPiece () =
  VAR x, y: INTEGER;
  BEGIN
    FOR i := 0 TO game.nTiles-1 DO
      x := loc.h + cur[i].h;
      y := loc.v + cur[i].v;
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
        x := wipeout[i].h;
	y := wipeout[i].v;
        PaintTile (x, y, WHITE);
      END;
    ELSE (* repaint wipeouts *)
      FOR i := 0 TO nWipeouts-1 DO
        x := wipeout[i].h;
	y := wipeout[i].v;
        PaintTile (x, y, board[x][y]);
      END;
    END;
  END BlinkWipeouts;


PROCEDURE CollapseWipeouts () =
  VAR x1, y1, y2, delta: INTEGER;
  BEGIN
    (* mark the wipeouts *)
    FOR i := 0 TO nWipeouts-1 DO
      x1 := wipeout[i].h;
      y1 := wipeout[i].v;
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


(*--------------------------------------------------------------- threads ---*)

PROCEDURE Clock (<*UNUSED*> arg: REFANY): REFANY =
  BEGIN
    LOOP
      Thread.Pause (delay);
      IF (running) AND (NOT paused) THEN
        PutEvent (Event.Tick);
      END;
    END;
  END Clock;

PROCEDURE Machine (<*UNUSED*> arg: REFANY): REFANY =
  BEGIN
    LOOP
      Advance (GetEvent ());
    END;
  END Machine;

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
          ScorePiece ((game.nRows - loc.v) * Height_pts);
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

PROCEDURE GoPressed (<*UNUSED*> v: ButtonVBT.T;  READONLY m: VBT.MouseRec) =
  BEGIN
    IF (state = State.Done) AND (NOT running) THEN
      ResetGame ();
      state   := State.Pausing;
      counter := 2;
      running := TRUE;
      SetLabel (stateLabel, "RUNNING  ");
      SetLabel (goTitle, "Stop");
      startTime := Time.Now ();
    ELSIF (paused) THEN
      startTime := startTime + (Time.Now () - pauseTime);
      paused := FALSE;
      SetLabel (stateLabel, "RUNNING  ");
      PaintGame (boardVBT, Region.Full);
      PutEvent (Event.Drop);
    ELSIF (running) THEN
      EndGame ();
    END;
    GetFocus (m.time);
  END GoPressed;

PROCEDURE PausePressed (<*UNUSED*> v: ButtonVBT.T;  READONLY m: VBT.MouseRec) =
  BEGIN
    IF (running) AND (NOT paused) THEN
      pauseTime := Time.Now ();
      paused := TRUE;
      SetLabel (stateLabel, "PAUSED   ");
      SetLabel (goTitle, "Resume");
      PaintGame (boardVBT, Region.Full);
    END;
    GetFocus (m.time);
  END PausePressed;

PROCEDURE OkPressed (<*UNUSED*> v: ButtonVBT.T;
                     <*UNUSED*> READONLY m: VBT.MouseRec) =
  (* We're done looking at the scores *)
  <*FATAL Split.NotAChild*>
  BEGIN
    TSplit.SetCurrent (chassis, gameVBT);
    VBT.ForceRepaint (chassis, Region.Full);
  END OkPressed;

PROCEDURE LevelPressed (<*UNUSED*> v: ButtonVBT.T;
                        <*UNUSED*> READONLY m: VBT.MouseRec) =
  VAR label: TEXT;
  BEGIN
    IF (running) OR (paused) THEN RETURN END;
    level := (level MOD Max_level) + 1;
    label := " Level: " & Fmt.Int (level);
    SetLabel (levelTitle, Fmt.Pad (label, 17, ' ', Fmt.Align.Left));
  END LevelPressed;

PROCEDURE GamePressed (<*UNUSED*> v: ButtonVBT.T;
                       <*UNUSED*> READONLY m: VBT.MouseRec) =
  BEGIN
    IF (running) OR (paused) THEN RETURN END;
    gameID := (gameID + 1) MOD NUMBER (all_games);
    SetGame (gameID);
  END GamePressed;

PROCEDURE SetGame (id: INTEGER) =
  <*FATAL TrestleComm.Failure*>
  BEGIN
    gameID := id;
    game   := all_games[id];
    SetLabel (gameTitle, "Game: " & game.name);
    ResetScoreFileName ();
    Trestle.Decorate (chassis, game.name, game.name, game.name);
    <* ASSERT game.nPieces <= LAST(Color) *>
    ResetGame ();
  END SetGame;

PROCEDURE HandPressed (<*UNUSED*> v: ButtonVBT.T;
                       <*UNUSED*> READONLY m: VBT.MouseRec) =
  BEGIN
    IF (running) OR (paused) THEN RETURN END;
    one_hand := NOT one_hand;
    SetKeyBindings (one_hand);
  END HandPressed;

PROCEDURE SpeedPressed (<*UNUSED*> v: ButtonVBT.T;
                       <*UNUSED*> READONLY m: VBT.MouseRec) =
  BEGIN
    IF (running) OR (paused) THEN RETURN END;
    speed_up := NOT speed_up;
    IF speed_up
      THEN SetLabel (speedTitle, " Speedup: on     ");
      ELSE SetLabel (speedTitle, " Speedup: off    ");
    END;
  END SpeedPressed;

PROCEDURE QuitPressed (<*UNUSED*> v: ButtonVBT.T;
                       <*UNUSED*> READONLY m: VBT.MouseRec) =
  BEGIN
    Process.Exit (0);
  END QuitPressed;

(*---------------------------------------------------------------- scores ---*)

TYPE
  Result = REF RECORD
    next   : Result;
    player : TEXT;
    score  : ScoreFile.Score;
  END;

VAR
  scoreFile : TEXT := "";
  dumping   : BOOLEAN := FALSE;

PROCEDURE UpdateScore () =
  VAR s: ScoreFile.Score;
  BEGIN
    s.n_games    := 1;
    s.n_seconds  := stopTime - startTime;
    s.best_date  := Time.Now ();
    s.best_wiped := nWiped;
    s.best_level := level;
    s.best_score := score;

    TRY
      ScoreFile.Put (scoreFile, PlayerName (), s);
    EXCEPT ScoreFile.Error(msg) =>
      NoteScoreFileError ("update", msg);
    END;
  END UpdateScore;

PROCEDURE ScoresPressed (<*UNUSED*> v: ButtonVBT.T;
                         <*UNUSED*> READONLY m: VBT.MouseRec) =
  <*FATAL Split.NotAChild*>
  VAR
    j  := 0;
    wr := TextWr.New ();
    r  := GetResults ();
    me := PlayerName ();
    zz : Result := NIL;
  BEGIN
    FOR i := 0 TO MaxScores-1 DO
      IF (r = NIL) THEN EXIT END;
      PrintResult (wr, r);
      SetLabel (scoreRows[j], TextWr.ToText (wr));  INC (j);
      IF (zz = NIL) AND Text.Equal (r.player, me) THEN zz := r END;
      r := r.next;
    END;

    (* see if we can print my score *)
    IF (zz = NIL) THEN
      WHILE (r # NIL) DO
        IF Text.Equal (r.player, me) THEN
          PrintResult (wr, r);
          SetLabel (scoreRows[j], BlankLine);  INC (j);
          SetLabel (scoreRows[j], TextWr.ToText (wr));  INC (j);
        END;
      END;
    END;

    FOR i := j TO LAST (scoreRows) DO
      SetLabel (scoreRows[i], BlankLine);
    END;

    (* and show it *)
    TSplit.SetCurrent (chassis, scoresVBT);
    VBT.ForceRepaint (chassis, Region.Full);
  END ScoresPressed;

PROCEDURE DumpScoreFiles () =
  <*FATAL Thread.Alerted, Wr.Failure*>
  VAR wr := Stdio.stdout;
  BEGIN
    dumping := TRUE;
    FOR i := FIRST (all_games) TO LAST (all_games) DO
      game := all_games[i];
      ResetScoreFileName ();
      IF ScoresExist () THEN
        Wr.PutText (wr, Wr.EOL & "------ ");
        Wr.PutText (wr, game.name);
        Wr.PutText (wr, " ------" & Wr.EOL);
        DumpScoreFile ();
      END;
    END;
    Wr.Flush  (wr);
  END DumpScoreFiles;

PROCEDURE ScoresExist (): BOOLEAN =
  BEGIN
    TRY
      EVAL FS.Status (scoreFile);
      RETURN TRUE;
    EXCEPT OSError.E =>
      RETURN FALSE;
    END;
  END ScoresExist;

PROCEDURE DumpScoreFile () =
  <*FATAL Thread.Alerted, Wr.Failure*>
  VAR r := GetResults ();  wr := Stdio.stdout;
  BEGIN
    Wr.PutText (wr, Title1);  Wr.PutText (wr, Wr.EOL);
    Wr.PutText (wr, Title2);  Wr.PutText (wr, Wr.EOL);
    Wr.PutText (wr, Title3);  Wr.PutText (wr, Wr.EOL);
    Wr.PutText (wr, Title4);  Wr.PutText (wr, Wr.EOL);

    WHILE (r # NIL) DO
      PrintResult (wr, r);
      Wr.PutText (wr, Wr.EOL);
      r := r.next;
    END;
    Wr.PutText (wr, Wr.EOL);
    Wr.Flush (wr);
  END DumpScoreFile;

PROCEDURE GetResults (): Result =
  VAR
    n_results   := 0;
    all_results : Result;

  PROCEDURE NoteScore (p: ScoreFile.Player;  READONLY s: ScoreFile.Score) =
    VAR 
    BEGIN
      all_results := NEW(Result, next := all_results, player := p, score := s);
      INC (n_results);
    END NoteScore;

  BEGIN
    TRY
      ScoreFile.Enumerate (scoreFile, NoteScore);
    EXCEPT ScoreFile.Error(msg) =>
      NoteScoreFileError ("read", msg);
    END;
    RETURN SortResults (all_results, n_results);
  END GetResults;

PROCEDURE SortResults (r: Result;  cnt: INTEGER): Result =
  VAR
    map := NEW (REF ARRAY OF INTEGER, cnt);
    ref := NEW (REF ARRAY OF Result, cnt);
    x   : Result;

  PROCEDURE CmpResult (a, b: INTEGER): [-1..+1] =
    VAR xa := ref[a];  xb := ref[b];
    BEGIN
      IF    (xa.score.best_score > xb.score.best_score) THEN RETURN -1;
      ELSIF (xa.score.best_score < xb.score.best_score) THEN RETURN +1;
      ELSE RETURN Text.Compare (xa.player, xb.player);
      END;
    END CmpResult;

  BEGIN
    FOR i := 0 TO cnt-1 DO
      ref [i] := r;  r := r.next;
      map [i] := i;
    END;

    (* sort them *)
    IntArraySort.Sort (map^, CmpResult);

    (* rebuild the linked list *)
    r := NIL;
    FOR i := cnt-1 TO 0 BY -1 DO
      x := ref [map [i]];
      x.next := r;
      r := x;
    END;

    RETURN r;
  END SortResults;

PROCEDURE PrintResult (wr: Wr.T;  r: Result) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR
    xx      : ARRAY [0..11] OF TEXT;
    minutes := ROUND ((r.score.n_seconds + 30.0d0) / 60.0d0);
    hours   := minutes DIV 60;
    date    := Date.FromTime (r.score.best_date);
  BEGIN
    minutes := minutes - hours * 60;
    xx[0]  := Text.Sub (r.player, 0, 16);
    xx[1]  := Fmt.Int (r.score.n_games);
    xx[2]  := Fmt.Int (hours);
    xx[3]  := Fmt.Int (minutes);
    xx[4]  := Fmt.Int (r.score.best_score);
    xx[5]  := Fmt.Int (r.score.best_wiped);
    xx[6]  := Fmt.Int (r.score.best_level);
    xx[7]  := Fmt.Int (date.year MOD 100);
    xx[8]  := Fmt.Int (ORD (date.month) + 1);
    xx[9]  := Fmt.Int (date.day);
    xx[10] := Fmt.Int (date.hour);
    xx[11] := Fmt.Int (date.minute);
    Wr.PutText (wr, Fmt.FN("%-16s %4s %3s:%02s  %7s %6s  %s  %02s.%02s.%02s %2s:%02s", xx));
  END PrintResult;

PROCEDURE PlayerName (): TEXT =
  VAR n: TEXT;
  BEGIN
    n := Env.Get (ALIAS);
    IF (n # NIL) THEN RETURN n END;
    n := OSConfig.UserName ();
    IF (n # NIL) THEN RETURN n END;
    RETURN "<unknown player>";
  END PlayerName;

PROCEDURE ResetScoreFileName () =
  BEGIN
    scoreFile := ScoreDir.Root & game.name & ".scores";
  END ResetScoreFileName;

PROCEDURE NoteScoreFileError (op, msg: TEXT) =
  <*FATAL ANY*>
  BEGIN
    IF (dumping) THEN
      IO.Put ("** unable to " & op & " score file \"" & scoreFile & "\"");
      IO.Put ("**     " & msg);
    ELSE
      (* clear the score file display *)
      FOR i := FIRST (scoreRows) TO LAST (scoreRows) DO
        SetLabel (scoreRows [i], BlankLine);
      END;

      (* insert the error message *)
      SetLabel (scoreRows [2], "** unable to " & op & " score file \""
                 & scoreFile & "\"");
      SetLabel (scoreRows [3], "**     " & msg);

      (* and show it *)
      TSplit.SetCurrent (chassis, scoresVBT);
      VBT.ForceRepaint (chassis, Region.Full);
    END;
  END NoteScoreFileError;

(*--------------------------------------------------------- game board VBT ---*)

TYPE
  GameVBT = VBT.Leaf OBJECT OVERRIDES
    mouse    := MouseClick;
    key      := KeyClick;
    reshape  := ReshapeGame;
    repaint  := PaintGame;
    shape    := GameShape;
    misc     := GameMisc;
  END;

PROCEDURE KeyClick (<*UNUSED*> v: GameVBT;  READONLY cd: VBT.KeyRec) =
  VAR e: Event;
  BEGIN
    IF (cd.wentDown) THEN
      e := keymap [Word.And (cd.whatChanged, 16_ff)];
      IF (e # Event.Noop) THEN  PutEvent (e);  END;
    END;
  END KeyClick;

PROCEDURE MouseClick (<*UNUSED*> v: GameVBT;  READONLY cd: VBT.MouseRec) =
  BEGIN
    IF (cd.clickType = VBT.ClickType.FirstDown) THEN
      GetFocus (cd.time);
      IF    (cd.whatChanged = VBT.Modifier.MouseL) THEN
        PutEvent (Event.Move_left);
      ELSIF (cd.whatChanged = VBT.Modifier.MouseM) THEN
        PutEvent (Event.Rotate_left);
      ELSIF (cd.whatChanged = VBT.Modifier.MouseR) THEN
        PutEvent (Event.Move_right);
      END;
    END;
  END MouseClick;

PROCEDURE GameShape (<*UNUSED*> v  : GameVBT;
                                ax : Axis.T;
                     <*UNUSED*> n  : CARDINAL): VBT.SizeRange =
  VAR sz: INTEGER;
  BEGIN
    IF (ax = Axis.T.Hor)
      THEN sz := game.nCols * hUnit;
      ELSE sz := game.nRows * vUnit;
    END;
    sz := Margin + sz;
    RETURN VBT.SizeRange {lo := sz, pref := sz, hi := 1024};
  END GameShape;

PROCEDURE GameMisc (<*UNUSED*> v: VBT.T;  READONLY cd: VBT.MiscRec) =
  BEGIN
    IF (cd.type = VBT.Deleted) OR (cd.type = VBT.Lost)
     (* OR (cd.type = VBT.Iconized) *) THEN
      DropFocus ();
    (*
    ELSIF (cd.type = VBT.Deiconized) THEN
      GetFocus (cd.time);
    *)
    END;
  END GameMisc;

PROCEDURE PaintGame (v: GameVBT;  READONLY rgn: Region.T) =
  VAR s: Rect.T;  r := Region.BoundingBox (rgn);
  BEGIN
    VBT.PaintTint (v, r, VBT_White);
    VBT.PaintTint (v, Rect.Meet (r, northE), VBT_Black);
    VBT.PaintTint (v, Rect.Meet (r, eastE), VBT_Black);
    VBT.PaintTint (v, Rect.Meet (r, southE), VBT_Black);
    VBT.PaintTint (v, Rect.Meet (r, westE), VBT_Black);

    IF (paused) THEN
      VBT.PaintTint (v, Rect.Meet (r, domain), VBT_White);
      RETURN;
    END;

    (* paint the fixed pieces *)
    FOR x := 0 TO game.nCols-1 DO
      s.west := domain.west + x * hUnit;
      s.east := s.west + hUnit;
      FOR y := 0 TO game.nRows-1 DO
        s.north := domain.north + y * vUnit;
        s.south := s.north + vUnit;
	IF (board[x][y] # WHITE) THEN
          VBT.PaintTint (boardVBT, Rect.Meet (r, s), tints [board[x][y]]);
          (** PaintTile (x, y, board[x][y]) **)
        END;
      END;
    END;

    (* paint the current piece *)
    IF (running) AND (cur # NIL) THEN  RepaintPiece ()  END;
  END PaintGame;

PROCEDURE ReshapeGame (v: GameVBT;  READONLY cd: VBT.ReshapeRec) =
  BEGIN
    Resize (cd.new);
    PaintGame (v, Region.Full);
  END ReshapeGame;


(**************************************************************)
(*                  main program                              *)
(**************************************************************)


PROCEDURE Init () =
  VAR controls, ignore: VBT.T;
  BEGIN
    (* seed the random number generator *)
    rand := NEW (Random.Default).init (fixed := FALSE);

    (* build the colors *) 
    VBT_White  := PaintOp.FromRGB (1.0, 1.0, 1.0);
    VBT_Black  := PaintOp.FromRGB (0.0, 0.0, 0.0);
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

    (* Build the buttons and labels *)
    scoreLabel  := NewLabel  ("0        ");
    wipeLabel   := NewLabel  ("0        ");
    stateLabel  := NewLabel  ("READY    ");

    keyLabel[0] := NewLabel  ("                 ");
    keyLabel[1] := NewLabel  ("                 ");
    keyLabel[2] := NewLabel  ("                 ");
    keyLabel[3] := NewLabel  ("                 ");
    keyLabel[4] := NewLabel  ("                 ");

    goButton    := NewButton (" Go              ", GoPressed, goTitle);
    pauseButton := NewButton (" Pause           ", PausePressed, pauseTitle);
    gameButton  := NewButton (" Game: Columns   ", GamePressed, gameTitle);
    levelButton := NewButton (" Level: 1        ", LevelPressed, levelTitle);
    handButton  := NewButton (" Hands: one      ", HandPressed, handTitle);
    speedButton := NewButton (" Speedup: on     ", SpeedPressed, speedTitle);
    scoreButton := NewButton (" Scores          ", ScoresPressed, scoreTitle);
    quitButton  := NewButton (" Quit            ", QuitPressed, quitTitle);

    (* create the column of labels & buttons *)
    controls := VBTCol (
                VBTRow (NewLabel ("Score:  "), scoreLabel),
                VBTRow (NewLabel ("Erased: "), wipeLabel),
                VBTRow (NewLabel ("State:  "), stateLabel),
                Gap (40.0, 3.0),
                keyLabel[0],
                keyLabel[1],
                keyLabel[2],
                keyLabel[3],
                keyLabel[4],
                Gap (),
                goButton,
                Gap (),
                pauseButton,
                Gap (),
                gameButton,
                Gap (),
                levelButton,
                Gap (),
                handButton,
                Gap (),
                speedButton,
                Gap (),
                scoreButton,
                Gap (),
                quitButton);


    (* playing area *)
    boardVBT := NEW (GameVBT);
    gameVBT := HVSplit.Cons (Axis.T.Hor, VBTCol (Gap()), controls, boardVBT);

    (* score display *)
    scoresVBT := HVSplit.New (Axis.T.Ver);
    Split.AddChild (scoresVBT, NewLabel (Title1));
    Split.AddChild (scoresVBT, NewLabel (Title2));
    Split.AddChild (scoresVBT, NewLabel (Title3));
    Split.AddChild (scoresVBT, NewLabel (Title4));
    FOR i := 0 TO LAST (scoreRows) DO
      scoreRows[i] := NewLabel (BlankLine);
      Split.AddChild (scoresVBT, scoreRows[i]);
    END;
    Split.AddChild (scoresVBT, VBTRow (NewButton ("Ok", OkPressed, ignore)));
    Split.AddChild (scoresVBT, NewLabel (" "));      

    (* top-level installed window *)
    chassis := TSplit.Cons (gameVBT, scoresVBT);

    (* select a game, key bindings & playing speed *)
    SetGame (0);
    SetKeyBindings (one_hand);
    ResetGame ();
    Resize (Rect.Empty);
    InitQueue ();

    (* start the threads *)
    machine := Thread.Fork (NEW (Thread.Closure, apply := Machine));
    clock   := Thread.Fork (NEW (Thread.Closure, apply := Clock));
  END Init;

(*----------------------------------------------------------- event queue ---*)

VAR
  events: RECORD
    mutex    : MUTEX;
    cnt      : INTEGER;
    head     : INTEGER;
    tail     : INTEGER;
    nonempty : Thread.Condition;
    nonfull  : Thread.Condition;
    contents : ARRAY [0..2] OF Event;
  END;

PROCEDURE InitQueue () =
  BEGIN
    events.cnt      := 0;
    events.head     := 0;
    events.tail     := 0;
    events.mutex    := NEW (MUTEX);
    events.nonfull  := NEW (Thread.Condition);
    events.nonempty := NEW (Thread.Condition);
  END InitQueue;

PROCEDURE PutEvent (evt: Event) =
  BEGIN
    WITH z = events DO
      LOCK z.mutex DO
        WHILE (z.cnt >= NUMBER (z.contents)) DO
          Thread.Wait (z.mutex, z.nonfull);
        END;
        z.contents [z.head] := evt;
        INC (z.head);
        IF (z.head >= NUMBER (z.contents)) THEN z.head := 0; END;
        INC (z.cnt);
      END;
      Thread.Signal (z.nonempty);
    END;
  END PutEvent;

PROCEDURE GetEvent (): Event =
  VAR evt: Event;
  BEGIN
    WITH z = events DO
      LOCK z.mutex DO
        WHILE (z.cnt <= 0) DO
          Thread.Wait (z.mutex, z.nonempty);
        END;
        evt := z.contents [z.tail];
        INC (z.tail);
        IF (z.tail >= NUMBER (z.contents)) THEN z.tail := 0; END;
        DEC (z.cnt);
      END;
      Thread.Signal (z.nonfull);
    END;
    RETURN evt;
  END GetEvent;

(*------------------------------------------------------------- main program ---*)

PROCEDURE DoIt () =
  <* FATAL TrestleComm.Failure*>
  VAR i: INTEGER;  arg: TEXT;
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

    (* check the command line options *)
    i := 1;
    WHILE (i < Params.Count) DO
      arg := Params.Get (i);
      IF Text.Equal (arg, "-scores") THEN
        DumpScoreFiles ();
        RETURN;
      ELSE
        IO.Put ("Unrecognized option: \"" & arg & "\", ignored" & Wr.EOL);
      END;
      INC (i);
    END;

    Init ();
    ResetGame ();
    Trestle.Install (chassis, game.name, game.name, game.name);
    Trestle.AwaitDelete (chassis);
  END DoIt;

BEGIN
  DoIt ();
END Main.
