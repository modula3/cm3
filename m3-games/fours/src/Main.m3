(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jun 14 16:52:41 PDT 1995 by kalsow     *)

MODULE Main;

IMPORT Point, Rect, VBT, Thread, Trestle, Random, Time, HVSplit;
IMPORT TextVBT, Wr, Axis, ButtonVBT, BorderedVBT, Text, Latin1Key;
IMPORT PaintOp, Params, Stdio, TSplit, Word, Region;
IMPORT Fmt, Split, TextWr, IO, Env, Font, Date;
IMPORT IntArraySort, RigidVBT, Process, TrestleComm;
IMPORT Config, ScoreFile, ScoreDir;

CONST
  ALIAS       = "GAMEALIAS";
  MaxScores   = 9;
  VGrain      = 3;
  ScoreBasis  = 0.656832d0;
  HeightPts   = 10;
  SpeedUp     = 0.95d0;
  SpeedUpRows = 5;
  MinCell     = 5;
  Margin      = 15;
  WHITE       = 0;
  MaxRows     = 24;
  MaxCols     = 12;
  MaxTiles    = 5;
  MaxLevel    = 5;

  Title1    = "                    total            best                    ";
  Title2    = "                ------------  -------------------------------";
  Title3    = "player          games  HH:MM   score   rows  lvl date            ";
  Title4    = "--------------- ----- ------  ------- ------ --- ----------------";
  BlankLine = "                                                             ";

TYPE
  EVT = { NONE, TICK, DROP, MOVE_LEFT, ROTATE_LEFT, ROTATE_RIGHT, MOVE_RIGHT };
  Color = [0..36];

  State = { FALLING, RESTING, BLINKING, PAUSING, DONE };

  FullTileList = ARRAY [0..MaxTiles-1] OF Point.T;

VAR
  games     : ARRAY [0..3] OF Config.T;
  cur_game  : INTEGER;
  cur_level : INTEGER;
  config    : Config.T;

VAR (* VBTs *)
  chassis   : VBT.T;
  tsplitVBT : VBT.T;
  scoresVBT : VBT.T;
  scoreRows : ARRAY [0..MaxScores+1] OF VBT.T;
  gameVBT   : VBT.T;
  imageV    : VBT.T;
  scoreV    : VBT.T;
  rowsV     : VBT.T;
  header    : VBT.T;
  stateV    : VBT.T;
  gameV     : VBT.T;
  gameTitle : VBT.T;
  handV     : VBT.T;
  handTitle : VBT.T;
  levelV    : VBT.T;
  levelTitle: VBT.T;
  keyLabelV : ARRAY [0..4] OF VBT.T;
  goV       : VBT.T;
  goTitle   : VBT.T;
  pauseV    : VBT.T;
  bestV     : VBT.T;
  quitV     : VBT.T;
  domain    : Rect.T;
  tints     : ARRAY Color OF PaintOp.T;
  keymap    : ARRAY [0..255] OF EVT;
  VBT_White : PaintOp.T;
  VBT_Black : PaintOp.T;

  clock     : Thread.T;
  machine   : Thread.T;

  oneHand   : BOOLEAN  := TRUE;
  focus     : BOOLEAN  := FALSE;
  running   : BOOLEAN  := FALSE;
  paused    : BOOLEAN  := FALSE;

  rowsDone  : INTEGER;
  score     : INTEGER;
  delay     : Time.T;
  unit      : INTEGER;

  northE    : Rect.T;
  westE     : Rect.T;
  southE    : Rect.T;
  eastE     : Rect.T;

  initDelay : Time.T;

  board     : ARRAY [0..MaxRows-1], [0..MaxCols-1] OF Color;
  counter   : INTEGER;
  curpiece  : INTEGER;
  position  : Point.T;
  rotation  : INTEGER;
  curLoc    : FullTileList;
  curColor  : Color;
  curScored : BOOLEAN;

  random    := NEW (Random.Default).init();
  state     := State.DONE;

  startTime : Time.T;
  stopTime  : Time.T;
  pauseTime : Time.T;

PROCEDURE Init () =
  VAR ignore: VBT.T;
  BEGIN
    (* build the colors *)
    VBT_White  := PaintOp.FromRGB (1.0, 1.0, 1.0);
    VBT_Black  := PaintOp.FromRGB (0.0, 0.0, 0.0);
    tints [ 0] := MakeColor (1.00, 1.00, 1.00); (* white *)
    tints [ 1] := MakeColor (1.00, 0.30, 0.00); (* orange *)
    tints [ 2] := MakeColor (1.00, 1.00, 0.00); (* yellow *)
    tints [ 3] := MakeColor (0.00, 0.00, 1.00); (* blue *)
    tints [ 4] := MakeColor (1.00, 0.00, 0.00); (* red *)
    tints [ 5] := MakeColor (0.00, 1.00, 0.00); (* green *)
    tints [ 6] := MakeColor (1.00, 0.00, 0.80); (* purplish red *)
    tints [ 7] := MakeColor (0.72, 0.66, 0.25); (* tan/khaki *)
    tints [ 8] := MakeColor (0.00, 1.00, 1.00); (* cyan *)
    tints [ 9] := MakeColor (0.00, 0.00, 0.25); (* navy blue *)
    (* tints [10] := MakeColor (0.69, 0.69, 0.14); (* goldenrod *) *)
    tints [10] := MakeColor (0.25, 0.20, 0.00); (* brown *)
    tints [11] := MakeColor (0.00, 0.59, 0.00); (* lime green *)
    tints [12] := MakeColor (0.37, 0.37, 0.37); (* light gray *)
    tints [13] := MakeColor (0.32, 0.10, 0.80); (* blue violet *)
    tints [14] := MakeColor (0.67, 0.67, 0.46); (* wheat *)
    tints [15] := MakeColor (0.50, 0.00, 0.52); (* light purple *)
    tints [16] := MakeColor (0.50, 0.00, 0.00); (* brick *)
    tints [17] := MakeColor (0.50, 0.09, 0.14); (* sick purple *)
    tints [18] := MakeColor (1.00, 0.70, 0.00); (* gold *)
    FOR i := 19 TO LAST(tints) DO
      tints [i] := MakeColor (random.real(), random.real(), random.real());
    END;

    (* build the vbts *)
    imageV := NEW (GameVBT);
    scoreV := LabelVBT ("0      ");
    rowsV  := LabelVBT ("0      ");
    stateV := LabelVBT ("READY  ");

    keyLabelV[0] := LabelVBT ("                     ");
    keyLabelV[1] := LabelVBT ("                     ");
    keyLabelV[2] := LabelVBT ("                     ");
    keyLabelV[3] := LabelVBT ("                     ");
    keyLabelV[4] := LabelVBT ("                     ");

    gameV  := NewButton ("Game: Fours  ", GamePressed, gameTitle);
    levelV := NewButton ("Level: 1     ", LevelPressed, levelTitle);
    handV  := NewButton ("Hands: one   ", HandPressed, handTitle);

    goV    := NewButton ("Go   ", GoPressed, goTitle);
    pauseV := NewButton ("Pause", PausePressed, ignore);
    bestV  := NewButton ("Scores", ScoresPressed, ignore);
    quitV  := NewButton ("Exit",   QuitPressed, ignore);

    header := VBTCol (
                VBTRow (LabelVBT ("Score: "), scoreV),
                VBTRow (LabelVBT ("Rows:  "), rowsV),
                VBTRow (LabelVBT ("State: "), stateV),
                Gap (40.0, 3.0),
                gameV,
                Gap (),
                levelV,
                Gap (),
                handV,
                Gap (),
                keyLabelV[0],
                keyLabelV[1],
                keyLabelV[2],
                keyLabelV[3],
                keyLabelV[4],
                LabelVBT (" "),
                goV,
                Gap (),
                pauseV,
                Gap (),
                bestV,
                Gap (),
                quitV
                );
    gameVBT := HVSplit.Cons (Axis.T.Hor, VBTCol (Gap()), header, imageV);

    scoresVBT := HVSplit.New (Axis.T.Ver);
    Split.AddChild (scoresVBT, LabelVBT (Title1));
    Split.AddChild (scoresVBT, LabelVBT (Title2));
    Split.AddChild (scoresVBT, LabelVBT (Title3));
    Split.AddChild (scoresVBT, LabelVBT (Title4));
    FOR i := 0 TO LAST (scoreRows) DO
      scoreRows[i] := LabelVBT (BlankLine);
      Split.AddChild (scoresVBT, scoreRows[i]);
    END;
    Split.AddChild (scoresVBT, VBTRow (NewButton ("Ok", DonePressed, ignore)));
    Split.AddChild (scoresVBT, LabelVBT (" "));      

    tsplitVBT := TSplit.Cons (gameVBT, scoresVBT);
    chassis := tsplitVBT;

    (* select a game, key bindings & playing speed *)
    SetGame (2);
    SetKeyBindings (TRUE);
    SetLevel (1);
    Resize (Rect.Empty);
    InitQueue ();

    (* start the threads *)
    machine := Thread.Fork (NEW (Thread.Closure, apply := Machine));
    clock   := Thread.Fork (NEW (Thread.Closure, apply := Clock));
  END Init;

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

PROCEDURE LabelVBT (t: TEXT): VBT.T =
  BEGIN
    IF (needFixed) THEN
      needFixed := FALSE;
      fixedFont := Font.FromName (
                     ARRAY OF TEXT{"-*-courier-medium-r-*-*-*-120-*"});
    END;
    RETURN TextVBT.New (t, 0.0, fnt := fixedFont);
  END LabelVBT;

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
                  a20,a21,a22,a23                        : VBT.T:=NIL): VBT.T =
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
    Split.AddChild (v, TextVBT.New (" "));
    RETURN v;
  END VBTCol;

PROCEDURE MakeColor (r, g, b: REAL): PaintOp.T =
  BEGIN
    RETURN PaintOp.Pair (VBT_White, PaintOp.FromRGB (r, g, b));
  END MakeColor;

(*----------------------------------------------- per Game initialization ---*)

PROCEDURE ResetGame () =
  BEGIN
    (* init the scalars *)
    state     := State.DONE;
    counter   := 2;
    running   := FALSE;
    paused    := FALSE;
    score     := 0;
    rowsDone  := 0;
    delay     := initDelay;

    (* start with an empty board *)
    FOR i := 0 TO MaxRows-1 DO
      FOR j := 0 TO MaxCols-1 DO
        board [i, j] := WHITE;
      END;
    END;

    (* move the currently "falling" piece off the visible board *)
    FOR i := 0 TO LAST (curLoc) DO
      curLoc[i].h := -10000;
      curLoc[i].v := -10000;
    END;

    (* and clear the board *)
    SetLabel (scoreV, "0");
    SetLabel (rowsV,  "0");
    Resize (VBT.Domain (imageV));
    VBT.ForceRepaint (imageV, Region.Full);
  END ResetGame;

(*--------------------------------------------------------------- threads ---*)

PROCEDURE Clock (<*UNUSED*> arg: REFANY): REFANY =
  BEGIN
    LOOP
      Thread.Pause (delay);
      IF (running) AND (NOT paused) THEN
        PutEvent (EVT.TICK);
      END;
    END;
  END Clock;

PROCEDURE Machine (<*UNUSED*> arg: REFANY): REFANY =
  VAR event: EVT;
  BEGIN
    (** wr := Wr.New ();  ??? ***)
    LOOP
      event := GetEvent ();
      CASE event OF
      | EVT.TICK =>
          CASE state OF
          | State.DONE =>
              (* ignore *)
          | State.FALLING =>
              IF NOT MoveDown () THEN
                state := State.RESTING;
                counter := 2 * VGrain;
              END;
          | State.RESTING =>
              IF NOT MoveDown () THEN
                DEC (counter);
                IF (counter <= 0) THEN EndFall () END;
              ELSE
                state := State.FALLING;
              END;
          | State.BLINKING =>
              DEC (counter);
              IF (counter <= 0) THEN
                RemoveSolidRows ();  state := State.PAUSING;
                counter := 4 * VGrain;
              ELSIF ((counter MOD VGrain) = 0) THEN
                BlinkSolidRows (Word.And (counter, 1) = 1);
              END;
          | State.PAUSING =>
              DEC (counter);
              IF (counter <= 0) THEN
                state := State.FALLING;
                StartNewPiece ();
              END;
          END;
      | EVT.DROP =>
          IF (state = State.FALLING) THEN
            ScorePiece ();
            WHILE MoveDown () DO (* plummet *) END;
            counter := 1;
          END;
      | EVT.MOVE_LEFT =>
          IF (state = State.FALLING) OR (state = State.RESTING) THEN
            MoveLeft ();
          END;
      | EVT.ROTATE_LEFT =>
          IF (state = State.FALLING) OR (state = State.RESTING) THEN 
            RotateLeft ();
          END;
      | EVT.ROTATE_RIGHT =>
          IF (state = State.FALLING) OR (state = State.RESTING) THEN 
            RotateRight ();
          END;
      | EVT.MOVE_RIGHT =>
          IF (state = State.FALLING) OR (state = State.RESTING) THEN 
            MoveRight () ;
          END;
      ELSE (* ignore *)
      END;
    END;
  END Machine;

PROCEDURE EndFall () =
  VAR nSolid: INTEGER;
  BEGIN
    ScorePiece ();
    FixPiece ();
    nSolid := 0;
    FOR i := config.nRows-1 TO 0 BY -1 DO
      IF SolidRow (i) THEN  INC (nSolid);  ScoreRow (i)  END;
    END;
    IF (nSolid > 0) THEN
      state := State.BLINKING;  counter := 5 * VGrain;  BlinkSolidRows (TRUE);
    ELSE
      state := State.PAUSING;   counter := 4 * VGrain;
    END;
  END EndFall;

PROCEDURE ScorePiece () =
  BEGIN
    IF (NOT curScored) THEN
      ScorePoints ((config.nRows - (position.v DIV VGrain)) * HeightPts);
      curScored := TRUE;
    END;
  END ScorePiece;

PROCEDURE ScoreRow (row: INTEGER) =
  BEGIN
    ScorePoints (HeightPts * (config.nRows-row) * config.nCols * 2);
    INC (rowsDone);
    IF (rowsDone MOD SpeedUpRows) = 0 THEN
      delay := delay * SpeedUp;
    END;
    SetLabel (rowsV, Fmt.Int (rowsDone));
    SetLabel (stateV, "RUNNING");
  END ScoreRow;

PROCEDURE ScorePoints (pts: INTEGER) =
  BEGIN
    INC (score, TRUNC (FLOAT (pts, LONGREAL) * ScoreBasis / delay));
    SetLabel (scoreV, Fmt.Int (score));
  END ScorePoints;

PROCEDURE EndGame () =
  BEGIN
    stopTime := Time.Now ();
    state    := State.DONE;
    counter  := 9999999;
    running  := FALSE;
    paused   := FALSE;
    UpdateScore ();
    SetLabel (stateV, "DONE");
    SetLabel (goTitle, "Go  ");
  END EndGame;

PROCEDURE StartNewPiece () =
  VAR i, j, k: INTEGER;  ok: BOOLEAN;
  BEGIN
    (* check for end of game *)
    FOR i := 0 TO config.nCols-1 DO
      IF (board [0, i] # WHITE) THEN
        (* the top row has a non-white cell *)
        EndGame ();
        RETURN;
      END;
    END;

    (* find a legal piece, position & rotation *)
    LOOP
      i := random.integer (0, config.nCols - 1);
      j := random.integer (0, config.nPieces - 1);
      k := random.integer (0, Config.NRotations - 1);
      WITH p = config.pieces[j][k] DO
        FOR z := 0 TO config.nTiles-1 DO
          curLoc [z].v := p.tiles[z].v - p.voffset;
          curLoc [z].h := p.tiles[z].h - p.hoffset + i;
        END;
      END;
      ok := TRUE;
      FOR z := 0 TO config.nTiles-1 DO
        ok := ok AND (0 <= curLoc[z].v) AND (curLoc[z].v < config.nRows)
                 AND (0 <= curLoc[z].h) AND (curLoc[z].h < config.nCols)
                 AND (board [curLoc[z].v, curLoc[z].h] = WHITE);
      END;
      IF ok THEN EXIT END;
    END;
    curpiece := j;
    rotation := k;
    WITH z = config.pieces[j][k] DO
      curColor := j+1;
      position := Point.FromCoords (i - z.hoffset,
                                    0 - z.voffset * VGrain);
    END;
    FOR j := 0 TO config.nTiles-1 DO
      curLoc[j].v := curLoc[j].v * VGrain;
      PaintSquare (curLoc[j].v, curLoc[j].h, curColor);
    END;

    curScored := FALSE;
  END StartNewPiece;

PROCEDURE BlinkSolidRows (on: BOOLEAN) =
  VAR r: Rect.T;
  BEGIN
    IF on THEN 
      (* paint highlight *)
      r.west := domain.west;
      r.east := domain.east;
      FOR i := config.nRows-1 TO 0 BY -1 DO
        IF SolidRow (i) THEN
          r.north := domain.north + i * unit;
          r.south := r.north + unit;
          VBT.PaintTint (imageV, r, VBT_White);
        END;
      END;
    ELSE
      (* erase highlight *)
      FOR i := config.nRows-1 TO 0 BY -1 DO
        IF SolidRow (i) THEN
          FOR j := 0 TO config.nCols-1 DO
            PaintSquare (i * VGrain, j, board [i, j]);
          END;
        END;
      END;
    END;
  END BlinkSolidRows;

PROCEDURE RemoveSolidRows () =
  VAR k, delta: INTEGER;
  BEGIN
    delta := 0;
    FOR i := config.nRows-1 TO 0 BY -1 DO
      IF SolidRow (i) THEN
        INC (delta);
        FOR j := 0 TO config.nCols-1 DO
          IF (board [i, j] # WHITE) THEN
            board [i, j] := WHITE;
            PaintSquare (i * VGrain, j, WHITE);
          END;
        END;
      ELSIF (delta > 0) THEN
        k := i + delta;
        FOR j := 0 TO config.nCols-1 DO
          IF (board [k, j] # board [i,j]) THEN
            board [k, j] := board [i,j];
            PaintSquare (k * VGrain, j, board [k, j]);
          END;
        END;
        FOR j := 0 TO config.nCols-1 DO
          IF (board [i, j] # WHITE) THEN
            board [i, j] := WHITE;
            PaintSquare (i * VGrain, j, WHITE);
          END;
        END;
      END;
    END;
  END RemoveSolidRows;

PROCEDURE SolidRow (r: INTEGER): BOOLEAN =
  BEGIN
    FOR i := 0 TO config.nCols-1 DO
      IF (board [r, i] = WHITE) THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END SolidRow;

PROCEDURE FixPiece () =
  BEGIN
    FOR i := 0 TO config.nTiles-1 DO
      board [curLoc[i].v DIV VGrain, curLoc[i].h] := curColor;
    END;
  END FixPiece;

PROCEDURE MoveDown (): BOOLEAN =
  BEGIN
    RETURN PlacePiece (rotation, position.v + 1, position.h);
  END MoveDown;

PROCEDURE MoveLeft () =
  BEGIN
    IF PlacePiece (rotation, position.v, position.h - 1) THEN END;
  END MoveLeft;

PROCEDURE MoveRight () =
  BEGIN
    IF PlacePiece (rotation, position.v, position.h + 1) THEN END;
  END MoveRight;

PROCEDURE RotateLeft () =
  BEGIN
    IF PlacePiece ((rotation + 1) MOD Config.NRotations,
                                          position.v, position.h) THEN END;
  END RotateLeft;

PROCEDURE RotateRight () =
  BEGIN
    IF PlacePiece ((rotation + Config.NRotations - 1) MOD Config.NRotations,
                                           position.v, position.h) THEN END;
  END RotateRight;

TYPE RectList = ARRAY [0..MaxTiles-1] OF Rect.T;

PROCEDURE PlacePiece (rot: INTEGER;  row, col: INTEGER): BOOLEAN =
  VAR j, h, v: INTEGER;  loc: FullTileList;
    old, new, toPaint, toErase: RectList;
  BEGIN
    (* map the piece *)
    FOR i := 0 TO config.nTiles-1 DO
      WITH p = config.pieces [curpiece][rot] DO
        loc[i].h := p.tiles[i].h + col;
        loc[i].v := p.tiles[i].v * VGrain + row;
      END;
    END;
    (* slide it back on the board *)
    h := config.nCols; v := 0; j := 0;
    FOR i := 0 TO config.nTiles-1 DO
      h := MIN (h, loc[i].h);
      j := MAX (j, loc[i].h);
      v := MIN (v, loc[i].v);
    END;
    IF (h < 0) THEN
      DEC (col, h);
      FOR i := 0 TO config.nTiles-1 DO DEC (loc[i].h, h) END;
    END;
    IF (j >= config.nCols) THEN
      j := j - config.nCols + 1;
      DEC (col, j);
      FOR i := 0 TO config.nTiles-1 DO DEC (loc[i].h, j) END;
    END;
    IF (v < 0) THEN
      DEC (row, v);
      FOR i := 0 TO config.nTiles-1 DO DEC (loc[i].v, v) END;
    END;
    (* test for a fit *)
    FOR i := 0 TO config.nTiles-1 DO
      h := loc[i].h;
      v := loc[i].v;
      j := v + (VGrain - 1);
      <* ASSERT (0 <= h) AND (h < config.nCols) AND (0 <= v) *>
      IF (config.nRows * VGrain <= j)      THEN RETURN FALSE END;
      IF (board [j DIV VGrain, h] # WHITE) THEN RETURN FALSE END;
      IF (board [v DIV VGrain, h] # WHITE) THEN RETURN FALSE END;
    END;

    (* map the old and new squares *)
    MapSquares (curLoc, old);
    MapSquares (loc, new);

    (* find the paint lists *)
    SubtractRects (new, old, toPaint);
    SubtractRects (old, new, toErase);

    (* finally, paint the rectangles *)
    PaintRects (toErase, WHITE);
    PaintRects (toPaint, curColor);

    (* place the new piece *)
    position.v := row;
    position.h := col;
    rotation   := rot;
    curLoc     := loc;
    RETURN TRUE;
  END PlacePiece;

PROCEDURE MapSquares (READONLY loc   : FullTileList;
                   VAR (*OUT*) rects : RectList) =
  BEGIN
    FOR i := 0 TO config.nTiles-1 DO
      WITH z = rects[i] DO
        z.north := domain.north + (loc[i].v * unit) DIV VGrain;
        z.west  := domain.west + (loc[i].h * unit);
        z.south := z.north + unit;
        z.east  := z.west + unit;
      END;
    END;
  END MapSquares;

PROCEDURE SubtractRects (READONLY a, b: RectList;  VAR(*OUT*) c: RectList) =
  (* c := a - b *)
  VAR r: Rect.T;
  BEGIN
    FOR i := 0 TO config.nTiles-1 DO
      r := a[i];
      FOR j := 0 TO config.nTiles-1 DO
        SubtractRect (r, b[j]);
      END;
      c[i] := r;
    END;
  END SubtractRects;

PROCEDURE SubtractRect (VAR a: Rect.T;  READONLY b: Rect.T) =
  (* a := a - b *)
  BEGIN
    IF (a.north >= a.south) OR (a.west >= a.east) THEN
      (* a is empty *)
      RETURN;
    END;
    IF (a.west >= b.east) OR (b.west >= a.east)
      OR (a.north >= b.south) OR (b.north >= a.south) THEN
      (* no overlap *)
      RETURN;
    END;
    IF (a.west = b.west) AND (a.east = b.east) THEN
      IF (a.north >= b.north) AND (b.south >= a.south) THEN
        (* a is contained in b *)
        a.south := a.north;
      ELSIF (a.north >= b.north) THEN
        a.north := b.south;
      ELSIF (b.south >= a.south) THEN
        a.south := b.north;
      ELSE (* a.north < b.north  AND  b.south < a.south  *)
        <* ASSERT FALSE *>
      END;
    ELSIF (a.north = b.north) AND (a.south = b.south) THEN
      IF (a.west >= b.west) AND (b.east >= a.east) THEN
        (* a is contained in b *)
        a.south := a.north;
      ELSIF (a.west >= b.west) THEN
        a.west := b.east;
      ELSIF (b.east >= a.east) THEN
        a.east := b.west;
      ELSE (* a.west < b.west  AND  b.east < a.east  *)
        <* ASSERT FALSE *>
      END;
    ELSE
      <* ASSERT FALSE *>
    END;
  END SubtractRect;

PROCEDURE PaintRects (READONLY x: RectList;  color: Color) =
  BEGIN
    FOR i := 0 TO config.nTiles-1 DO
      WITH z = x[i] DO
        IF (z.north < z.south) AND (z.west < z.east) THEN
          VBT.PaintTint (imageV, z, tints [color]);
        END;
      END;
    END;
  END PaintRects;

PROCEDURE PaintSquare (row, col: INTEGER;  color: Color) =
  VAR r: Rect.T;
  BEGIN
    IF (NOT paused) THEN
      r.north := domain.north + (row * unit) DIV VGrain;
      r.west  := domain.west  + col * unit;
      r.south := r.north + unit;
      r.east  := r.west  + unit;
      VBT.PaintTint (imageV, r, tints [color]);
    END;
  END PaintSquare;

(*-------------------------------------------------------- game selection ---*)

PROCEDURE GamePressed (<*UNUSED*> v: ButtonVBT.T;
                       <*UNUSED*> READONLY m: VBT.MouseRec) =
  BEGIN
    IF (NOT running) AND (state = State.DONE) THEN
      INC (cur_game);  IF (cur_game >= NUMBER (games)) THEN cur_game := 0; END;
      SetGame (cur_game);
    END;
  END GamePressed;

PROCEDURE SetGame (id: INTEGER) =
  <*FATAL TrestleComm.Failure*>
  BEGIN
    cur_game := id;
    config   := games [cur_game];
    SetLabel (gameTitle, "Game: " & config.name);
    ResetScoreFileName ();
    Trestle.Decorate (chassis, config.name, config.name, config.name);
    <* ASSERT config.nPieces <= LAST(Color) *>
    ResetGame ();
  END SetGame;

(*---------------------------------------------------------- key bindings ---*)

PROCEDURE HandPressed (<*UNUSED*> v: ButtonVBT.T;
                       <*UNUSED*> READONLY m: VBT.MouseRec) =
  BEGIN
    IF (NOT running) AND (state = State.DONE) THEN
      oneHand := NOT oneHand;
      SetKeyBindings (oneHand);
    END;
  END HandPressed;

PROCEDURE SetKeyBindings (on: BOOLEAN) =
  BEGIN
    (* record the global state *)
    oneHand := on;

    (* build the key mapping *)
    FOR k := FIRST (keymap) TO LAST (keymap) DO keymap [k] := EVT.NONE END;
    keymap [Latin1Key.space] := EVT.DROP;

    IF (oneHand) THEN
      keymap [Latin1Key.S] := EVT.MOVE_LEFT;
      keymap [Latin1Key.s] := EVT.MOVE_LEFT;
      keymap [Latin1Key.D] := EVT.ROTATE_LEFT;
      keymap [Latin1Key.d] := EVT.ROTATE_LEFT;
      keymap [Latin1Key.F] := EVT.MOVE_RIGHT; 
      keymap [Latin1Key.f] := EVT.MOVE_RIGHT; 
      keymap [Latin1Key.J] := EVT.MOVE_LEFT;
      keymap [Latin1Key.j] := EVT.MOVE_LEFT;
      keymap [Latin1Key.K] := EVT.ROTATE_LEFT;
      keymap [Latin1Key.k] := EVT.ROTATE_LEFT;
      keymap [Latin1Key.L] := EVT.MOVE_RIGHT; 
      keymap [Latin1Key.l] := EVT.MOVE_RIGHT; 
    ELSE
      keymap [Latin1Key.D] := EVT.MOVE_LEFT;
      keymap [Latin1Key.d] := EVT.MOVE_LEFT;
      keymap [Latin1Key.F] := EVT.ROTATE_LEFT;
      keymap [Latin1Key.f] := EVT.ROTATE_LEFT;
      keymap [Latin1Key.J] := EVT.ROTATE_RIGHT;
      keymap [Latin1Key.j] := EVT.ROTATE_RIGHT;
      keymap [Latin1Key.K] := EVT.MOVE_RIGHT; 
      keymap [Latin1Key.k] := EVT.MOVE_RIGHT; 
    END;

    (* set the window labels *)
    IF (oneHand) THEN
      SetLabel (keyLabelV[0], "s, j - move left ");
      SetLabel (keyLabelV[1], "d, k - rotate    ");
      SetLabel (keyLabelV[2], "f, l - move right");
      SetLabel (keyLabelV[3], "<space> - drop   ");
      SetLabel (keyLabelV[4], "                 ");
    ELSE
      SetLabel (keyLabelV[0], "d - move left    ");
      SetLabel (keyLabelV[1], "f - rotate down  ");
      SetLabel (keyLabelV[2], "j - rotate up    ");
      SetLabel (keyLabelV[3], "k - move right   ");
      SetLabel (keyLabelV[4], "<space> - drop   ");
    END;

    (* reset the button *)
    IF (oneHand)
      THEN SetLabel (handTitle, "Hands: one");
      ELSE SetLabel (handTitle, "Hands: two");
    END;
  END SetKeyBindings;

(*------------------------------------------------------------ game level ---*)

PROCEDURE LevelPressed (<*UNUSED*> v: ButtonVBT.T;
                        <*UNUSED*> READONLY m: VBT.MouseRec) =
  BEGIN
    
    IF (NOT running) AND (state = State.DONE) THEN
      cur_level := cur_level + 1;
      IF (cur_level > MaxLevel) THEN cur_level := 1; END;
      SetLevel (cur_level);
    END;
  END LevelPressed;

PROCEDURE SetLevel (lev: INTEGER) =
  BEGIN
    cur_level := MIN (MAX (1, lev), MaxLevel);
    initDelay := config.delay * (1.0d0 - 0.15d0 * FLOAT(cur_level, LONGREAL));
    SetLabel (levelTitle, "Level: " & Fmt.Int (cur_level));
  END SetLevel;

(*---------------------------------------------------------- misc control ---*)

PROCEDURE GoPressed (<*UNUSED*> v: ButtonVBT.T;  READONLY m: VBT.MouseRec) =
  BEGIN
    IF (state = State.DONE) AND (NOT running) THEN
      ResetGame ();
      state   := State.PAUSING;
      counter := 2;
      running := TRUE;
      SetLabel (stateV, "RUNNING");
      SetLabel (goTitle, "Stop");
      startTime := Time.Now ();
    ELSIF (paused) THEN
      startTime := startTime + (Time.Now () - pauseTime);
      paused := FALSE;
      SetLabel (stateV, "RUNNING");
      PaintGame (imageV, Region.Full);
      PutEvent (EVT.DROP);
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
      SetLabel (stateV, "PAUSED");
      PaintGame (imageV, Region.Full);
      SetLabel (goTitle, "Resume");
    END;
    GetFocus (m.time);
  END PausePressed;

PROCEDURE DonePressed (<*UNUSED*> v: ButtonVBT.T;
                       <*UNUSED*> READONLY m: VBT.MouseRec) =
  <*FATAL Split.NotAChild*>
  BEGIN
    TSplit.SetCurrent (tsplitVBT, gameVBT);
    VBT.ForceRepaint (tsplitVBT, Region.Full);
  END DonePressed;

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
    s.best_level := cur_level;
    s.best_rows  := rowsDone;
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
    TSplit.SetCurrent (tsplitVBT, scoresVBT);
    VBT.ForceRepaint (tsplitVBT, Region.Full);
  END ScoresPressed;

PROCEDURE DumpScoreFiles () =
  <*FATAL Thread.Alerted, Wr.Failure*>
  VAR wr := Stdio.stdout;
  BEGIN
    dumping := TRUE;
    FOR i := FIRST (games) TO LAST (games) DO
      config := games[i];
      ResetScoreFileName ();
      Wr.PutText (wr, "\n------ ");
      Wr.PutText (wr, config.name);
      Wr.PutText (wr, " ------\n");
      DumpScoreFile ();
    END;
    Wr.Flush  (wr);
  END DumpScoreFiles;

PROCEDURE DumpScoreFile () =
  <*FATAL Thread.Alerted, Wr.Failure*>
  VAR r := GetResults ();  wr := Stdio.stdout;
  BEGIN
    Wr.PutText (wr, Title1);  Wr.PutChar (wr, '\n');
    Wr.PutText (wr, Title2);  Wr.PutChar (wr, '\n');
    Wr.PutText (wr, Title3);  Wr.PutChar (wr, '\n');
    Wr.PutText (wr, Title4);  Wr.PutChar (wr, '\n');

    WHILE (r # NIL) DO
      PrintResult (wr, r);
      Wr.PutChar (wr, '\n');
      r := r.next;
    END;
    Wr.PutText (wr, "\n");
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
    xx[5]  := Fmt.Int (r.score.best_rows);
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
    RETURN Env.Get ("USER");
  END PlayerName;

PROCEDURE ResetScoreFileName () =
  BEGIN
    scoreFile := ScoreDir.Root & config.name & ".scores";
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
      TSplit.SetCurrent (tsplitVBT, scoresVBT);
      VBT.ForceRepaint (tsplitVBT, Region.Full);
    END;
  END NoteScoreFileError;

(*-------------------------------------------------------- game board VBT ---*)

TYPE
  GameVBT = VBT.Leaf OBJECT OVERRIDES
    mouse    := MouseGame;
    key      := GameKey;
    reshape  := ReshapeGame;
    repaint  := PaintGame;
    shape    := GameShape;
    misc     := GameMisc;
  END;

PROCEDURE MouseGame (<*UNUSED*> v: GameVBT;  READONLY cd: VBT.MouseRec) =
  BEGIN
    IF (cd.clickType = VBT.ClickType.FirstDown) THEN
      GetFocus (cd.time);
      IF    (cd.whatChanged = VBT.Modifier.MouseL) THEN
        PutEvent (EVT.MOVE_LEFT);
      ELSIF (cd.whatChanged = VBT.Modifier.MouseM) THEN
        PutEvent (EVT.ROTATE_LEFT);
      ELSIF (cd.whatChanged = VBT.Modifier.MouseR) THEN
        PutEvent (EVT.MOVE_RIGHT);
      END;
    END;
  END MouseGame;

PROCEDURE GameKey (<*UNUSED*> v: GameVBT;  READONLY cd: VBT.KeyRec) =
  VAR e: EVT;
  BEGIN
    IF (cd.wentDown) THEN
      e := keymap [Word.And (cd.whatChanged, 16_ff)];
      IF (e # EVT.NONE) THEN  PutEvent (e);  END;
    END;
  END GameKey;

PROCEDURE GameShape (<*UNUSED*> v  : GameVBT;
                                ax : Axis.T;
                     <*UNUSED*> n  : CARDINAL): VBT.SizeRange =
  VAR sz: INTEGER;
  BEGIN
    IF (ax = Axis.T.Hor)
      THEN sz := config.nCols;
      ELSE sz := config.nRows;
    END;
    sz := Margin + sz * VGrain * MinCell;
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
    FOR i := 0 TO config.nRows-1 DO
      s.north := domain.north + i * unit;
      s.south := s.north + unit;
      FOR j := 0 TO config.nCols-1 DO
        s.west := domain.west + j * unit;
        s.east := s.west + unit;
        IF (board[i,j] # WHITE) THEN
          VBT.PaintTint (v, Rect.Meet (r, s), tints [board[i,j]]);
        END;
      END;
    END;
    IF (running) THEN
      FOR i := 0 TO config.nTiles-1 DO
        PaintSquare (curLoc[i].v, curLoc[i].h, curColor);
      END;
    END;
  END PaintGame;

PROCEDURE ReshapeGame (v: GameVBT;  READONLY cd: VBT.ReshapeRec) =
  BEGIN
    Resize (cd.new);
    PaintGame (v, Region.Full);
  END ReshapeGame;

PROCEDURE Resize (READONLY r: Rect.T) =
  CONST BorderWidth = 2;
  VAR h, v, s: INTEGER;  p: Point.T;
  BEGIN
    h := (Rect.HorSize (r) - Margin) DIV (config.nCols * VGrain);
    v := (Rect.VerSize (r) - Margin) DIV (config.nRows * VGrain);
    s := MAX (MinCell, MIN (h, v)) * VGrain;
    p := Rect.Middle (r);

    unit := s;

    domain.north := p.v - (config.nRows * s) DIV 2;
    domain.west  := p.h - (config.nCols * s) DIV 2;
    domain.south := domain.north + config.nRows * s;
    domain.east  := domain.west + config.nCols * s;

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
  END Resize;

(*-------------------------------------------------------- keyboard focus ---*)

PROCEDURE GetFocus (t: VBT.TimeStamp) =
  BEGIN
    IF (NOT focus) THEN
      TRY
        VBT.Acquire (imageV, VBT.KBFocus, t);
        focus := TRUE;
      EXCEPT VBT.Error =>
      END;
    END;
  END GetFocus;

PROCEDURE DropFocus () =
  BEGIN
    IF (focus) THEN
      VBT.Release (imageV, VBT.KBFocus);
      focus := FALSE;
    END;
  END DropFocus;

(*----------------------------------------------------------- event queue ---*)

VAR
  events: RECORD
    mutex    : MUTEX;
    cnt      : INTEGER;
    head     : INTEGER;
    tail     : INTEGER;
    nonempty : Thread.Condition;
    nonfull  : Thread.Condition;
    contents : ARRAY [0..2] OF EVT;
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

PROCEDURE PutEvent (evt: EVT) =
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

PROCEDURE GetEvent (): EVT =
  VAR evt: EVT;
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

(*---------------------------------------------------------- main program ---*)

PROCEDURE DoIt () =
  <*FATAL TrestleComm.Failure*>
  VAR i: INTEGER;  arg: TEXT;
  BEGIN
    (* build the game descriptions *)
    games[0] := Config.New (2);
    games[1] := Config.New (3);
    games[2] := Config.New (4);
    games[3] := Config.New (5);

    (* check the command line options *)
    i := 1;
    WHILE (i < Params.Count) DO
      arg := Params.Get (i);
      IF Text.Equal (arg, "-scores") THEN
        DumpScoreFiles ();
        RETURN;
      ELSE
        IO.Put ("Unrecognized option: \"" & arg & "\", ignored\n");
      END;
      INC (i);
    END;

    Init ();
    ResetGame ();
    Trestle.Install (chassis, config.name, config.name, config.name);
    Trestle.AwaitDelete (chassis);
  END DoIt;

BEGIN
  DoIt ();
END Main.
