(* 
  Translation to Modula-3:
        Copyright 1990, 1991 Digital Equipment Corporation 

  Modifications for Xaw widget set, and other improvements,
  designated XTETRIS versions 2.X:
        Copyright 1990, 1991 Daniel R. Greening

  Modifications for X : Didier Tallot <tallot@bdblues.altair.fr>
        Copyright 1989 Didier Tallot

  Copyright 1989 Phill Everson & Martyn Shortley

  This notice and any statement of authorship must be reproduced
  on all copies.  The authors do not make any warranty expressed
  or implied, or assume any liability or responsiblity for the
  use of this software.

  Any distributor of copies of this software shall grant the
  recipient permission for further redistribution as permitted
  by this notice.  Any distributor must distribute this software
  without any fee or other monetary gains, unless expressed written
  permission is granted by the authors.

  This software or its use shall not be: sold, rented, leased,
  traded, or otherwise marketed without the expressed written
  permission of the authors.
*)

(* Last modified on Mon Jan 30 16:00:07 PST 1995 by kalsow       *)
(*      modified on Wed Oct 14 13:12:38 PDT 1992 by muller       *)
(*      modified on Tue Apr 24 09:07:35 1990 by jerome           *)

UNSAFE MODULE Main;

FROM M3toC IMPORT TtoS, StoT;

IMPORT FileRd, FileWr, Fmt, Random, Rd, Scan, Stdio, Text, Wr, Word;
IMPORT Ctypes, Cstdlib, Utime, Uutmp, RTLinker, Thread, OSError;
IMPORT X, Xrm, Xt, XtN, XtR, Xaw, M3toC, Params;
<*FATAL ANY*>

CONST

  NWIDTH           =  10;               (* canvas width in units *)
  MWIDTH           =  NWIDTH-1;         (* max index *)
  NHEIGHT          =  30;               (* canvas height in units *)
  MHEIGHT          =  NHEIGHT-1;        (* max index *)
  NSHAPE           =  7;                (* number of different shapes *)
  MSHAPE           =  NSHAPE-1;         (* max index *)
  NROT             =  4;                (* number of rotations *)
  MROT             =  NROT-1;           (* max index *)
  NHSCORE          =  20;               (* number of recorded high scores *)
  MHSCORE          =  NHSCORE-1;        (* max index *)

  HIGH_SCORE_FILE  =  "tetris_scores";

TYPE

  READER           =  Rd.T;
  WRITER           =  Wr.T;
  WORD             =  Word.T;

  resource_res = RECORD
    foreground:   Xt.Pixel;
    background:   Xt.Pixel;
    boxsize:      Xt.Dimension;
    scorefilep:   INTEGER;
    startscore:   INTEGER;
    startrows:    INTEGER;
    startlevel:   INTEGER
  END;

  score_rec = RECORD
    name:  TEXT;
    score: INTEGER;
    rows:  INTEGER;
    level: INTEGER;
    date:  TEXT
  END;

  Int4 = ARRAY [0..MROT] OF INTEGER;

  rotate_rec = RECORD
    unitson:  WORD;         (* an array of 4x4 = 16 bits, indicating the 
                               on units in this order:
                                   <3,3> <2,3> <1,3> <0,3> <3,2> ... <0,0> *)
    points:   INTEGER;      (* Points for acceptance in this position. *)
    highesty: Int4;         (* highest non-0 y in unitson, for each x *)
    highestx: Int4;         (* highest non-0 x in unitson, for each y *)
    lowestx:  Int4          (* lowest  non-0 y in unitson, for each y *)
  END;

  shape_rec = RECORD
    forms:        ARRAY [0..MROT] OF rotate_rec;
    foreground:   Xt.Pixel;
    background:   Xt.Pixel;
    gc:           X.GC
  END;

VAR
  rand : Random.T := NEW (Random.Default).init ();

  (* get from resource database *)

  foreground:         Xt.Pixel;
  background:         Xt.Pixel;
  scorefilep:         BOOLEAN;          (* use / don't use score file *)
  bsize:              Xt.Dimension;     (* size of one unit box *)
  startscore:         INTEGER;          (* starting value *)
  startrows:          INTEGER;          (* starting value *)
  startlevel:         INTEGER;          (* starting value *)

  score:              INTEGER;          (* the actual score *)
  rows:               INTEGER;          (* the number of removed rows *)
  level:              INTEGER;          (* the actual level *)

  False: X.Bool       := 0;             (* to de defined in X.i3 ... *)

  user_name:          TEXT;
  running:            BOOLEAN;

  cur_shape:          INTEGER;          (* the current moving shape *)
  cur_xpos:           INTEGER;          (* the current xpos for cur_shape *)
  cur_ypos:           INTEGER;          (* the current ypos for cur_shape *)
  cur_rot:            INTEGER;          (* the current rot for cur_shape *)

  next_shape:         INTEGER;          (* the next moving shape *)
  next_rot:           INTEGER;          (* the current rot for next_shape *)

  context:            Xt.AppContext;

  toplevel:           Xt.Widget;
  frame:              Xt.Widget;
  left_frame:         Xt.Widget;
  right_frame:        Xt.Widget;
  stat_frame:         Xt.Widget;
  stat_shapes:        ARRAY [0..MSHAPE] OF Xt.Widget;
  stat_labels:        ARRAY [0..MSHAPE] OF Xt.Widget;
  score_frame:        Xt.Widget;
  score_panel:        Xt.Widget;
  canvas:             Xt.Widget;
  shadow:             Xt.Widget;
  nextobj:            Xt.Widget;
  stats:              Xt.Widget;
  start_bt:           Xt.Widget;
  pause_bt:           Xt.Widget;
  newgame_bt:         Xt.Widget;
  quit_bt:            Xt.Widget;
  score_item:         Xt.Widget;
  level_item:         Xt.Widget;
  rows_item:          Xt.Widget;
  game_over:          Xt.Widget;

  shadegc:            X.GC;   (* to add a shadow for block unit *)
  erasegc:            X.GC;   (* when a line is removed *)
  movegc:             X.GC;   (* to blit the canvas after removing a line *)

  gcval:=             NEW (UNTRACED REF X.XGCValues);

  grid:               ARRAY [0..MWIDTH], [0..MHEIGHT] OF X.GC;
  shapes:             ARRAY [0..MSHAPE] OF shape_rec;
  stat_totals:        ARRAY [0..MSHAPE] OF INTEGER;
  stat_counts:        ARRAY [0..MSHAPE] OF INTEGER;
  high_scores:        ARRAY [0..MHSCORE] OF score_rec;
  high_score_item:    ARRAY [0..MHSCORE+2] OF Xt.Widget;
  high_score_def      := score_rec {"", 0, 0, 0,""};


  shapenames          := ARRAY [0..7] OF TEXT 
                            {"Shape",  "shape0", "shape1", "shape2", 
                             "shape3", "shape4", "shape5", "shape6"};

  fallbacklist        := NEW (Xt.FallbackResList);
  options             := NEW (Xrm.OptionDescList);
  resources           := NEW (UNTRACED REF resource_res);
  actions             := NEW (Xt.ActionList);
  reslist             := NEW (Xt.ResourceList);
  args                := NEW (Xt.ArgList);

  count:              INTEGER;

CONST
  fallbacktext  = ARRAY [0..30] OF TEXT {
    "*LeftFrame.Buttons.NewGame.translations: #augment \\n" &
    "        <Btn1Down>,<Btn1Up>: NewGame() notify()",
    "*LeftFrame.Buttons.Pause.translations: #override \\n" &
    "        <Btn1Down>,<Btn1Up>: Pause()",
    "*LeftFrame.Buttons.Quit.translations: #augment \\n" &
    "        <Btn1Down>,<Btn1Up>: Quit() notify()",
    "*LeftFrame.Buttons.Scores.translations: #augment \\n" &
    "        <Btn1Down>,<Btn1Up>: Scores() notify()",
    "*LeftFrame.Buttons.Start.translations: #override \\n" &
    "        <Btn1Down>,<Btn1Up>: Start()",
    "*RightFrame.Canvas.translations: " &
    "      <Expose>: Refresh() \\n" &
    "      !Shift<Btn1Down>: RotateCCW() \\n" &
    "      !<Btn1Down>: MoveLeft() \\n" &
    "      !Shift<Btn3Down>: RotateCW() \\n" &
    "      !<Btn3Down>: MoveRight() \\n" &
    "      !Shift<Btn2Down>: Drop()",
    "*RightFrame.Canvas.accelerators: " &
    "      <Key>space: Drop() \\n" &
    "      <Key>h: MoveLeft() \\n" &
    "      <Key>q: Quit() \\n" &
    "      <Key>p: Pause() \\n" &
    "      <Key>s: Start() \\n" &
    "      <Key>r: NewGame() \\n" &
    "      <Key>Left: MoveLeft() \\n" &
    "      <Key>j: RotateCW() \\n" &
    "      <Key>Down: RotateCW() \\n" &
    "      <Key>k: RotateCCW() \\n" &
    "      <Key>Up: RotateCCW() \\n" &
    "      <Key>l: MoveRight() \\n" &
    "      <Key>Right: MoveRight()",
    "*LeftFrame.NextObject.translations: " &
    "      <Expose>:Refresh()",
    "*RightFrame.Shadow.translations: " &
    "      <Expose>:Refresh()",
    "*StatFrame.Shapes.translations: " &
    "      <Expose>:Refresh()",
    "*ScoreFrame.ScorePanel.Close.translations: #augment \\n" &
    "         <Btn1Down>,<Btn1Up>:Close()",
    "*ScorePanel.Close.translations:        #augment \\n" &
    "        <Btn1Down>,<Btn1Up>:Close()",
    "Tetris*BorderWidth: 1",
    "Tetris*ShapeStyle: Oval",
    "Tetris.shape0.foreground:   #ff0000", (* red1 *)
    "Tetris.shape0.background:   #8b0000", (* red4 *)
    "Tetris.shape1.foreground:   #ffa500", (* orange1 *)
    "Tetris.shape1.background:   #8b5a00", (* orange4 *)
    "Tetris.shape2.foreground:   #ffff00", (* yellow1 *)
    "Tetris.shape2.background:   #8b8b00", (* yellow4 *)
    "Tetris.shape3.foreground:   #00ff00", (* green1 *)
    "Tetris.shape3.background:   #008b00", (* green4 *)
    "Tetris.shape4.foreground:   #0000ff", (* blue1 *)
    "Tetris.shape4.background:   #00008b", (* blue4 *)
    "Tetris.shape5.foreground:   #00ffff", (* cyan1 *)
    "Tetris.shape5.background:   #008b8b", (* cyan4 *)
    "Tetris.shape6.foreground:   #9b30ff", (* purple1 *)
    "Tetris.shape6.background:   #551a8b", (* purple4 *)
    "Tetris*ScorePanel*Font:     8x13",
    "Tetris*Font:      -*-helvetica-bold-r-*-*-*-120-*-*-*-*-*-*",
    "Tetris*Buttons*width:        100"
  };

(********************************************************************

                        X 1 1   U T I L I T I E S

 ********************************************************************)


PROCEDURE SetValueText (w: Xt.Widget; name: Xt.String; val: TEXT) =
BEGIN
  args[0] := Xt.Arg {name, TtoS (val)};
  Xt.SetValues (w, args, 1)
END SetValueText;

PROCEDURE CreateWidget (name: TEXT; class: Xt.WidgetClass; parent: Xt.Widget;
                      args: Xt.ArgList := NIL; narg: INTEGER := 0): Xt.Widget =
BEGIN
  RETURN (Xt.CreateManagedWidget (TtoS (name), class, parent, args, narg))
END CreateWidget; 

PROCEDURE SetArgI (VAR args: Xt.ArgList; VAR count: INTEGER; 
                   name: Xt.String; val: INTEGER) =
BEGIN
  args[count] := Xt.Arg {name, LOOPHOLE (val, ADDRESS)};
  INC (count)
END SetArgI;

PROCEDURE SetArgA (VAR args: Xt.ArgList; VAR count: INTEGER; 
                   name: Xt.String; val: ADDRESS) =
BEGIN
  args[count] := Xt.Arg {name, val};
  INC (count)
END SetArgA;

PROCEDURE SetArgT (VAR args: Xt.ArgList; VAR count: INTEGER; 
                   name: Xt.String; val: TEXT) =
BEGIN
  args[count] := Xt.Arg {name, TtoS (val)};
  INC (count)
END SetArgT;

PROCEDURE SetOption (VAR optlist: Xrm.OptionDescList; VAR count: INTEGER; 
                     optionn, specifier: TEXT;
                     argKind: Xrm.OptionKind; val: TEXT) =
BEGIN
  optlist[count] := Xrm.OptionDescRec{ TtoS (optionn), TtoS (specifier),
                                       argKind, TtoS (val)};
  INC (count)
END SetOption;

PROCEDURE SetAction (VAR optlist: Xt.ActionList; VAR count: INTEGER; 
                     name: TEXT; proc: Xt.ActionProc) =
BEGIN
  optlist[count] :=  Xt.ActionsRec {TtoS (name), proc};
  INC (count)
END SetAction;

PROCEDURE SetResource (VAR reslist: Xt.ResourceList; VAR count: INTEGER; 
                       name, class: TEXT; type: Xt.String; size, offset: Xt.Cardinal;
                       default_type: Xt.String; default_addr: TEXT) =
BEGIN
  reslist[count] :=  Xt.Resource {TtoS (name), TtoS (class), type,
                                  size, offset, default_type, TtoS (default_addr)};
  INC (count)
END SetResource;

PROCEDURE MapWidget (widget: Xt.Widget) =
BEGIN
  X.XMapWindow (Xt.Display (widget), Xt.XtWindow (widget))
END MapWidget;


PROCEDURE UnmapWidget (widget: Xt.Widget) =
BEGIN
  X.XUnmapWindow (Xt.Display (widget), Xt.XtWindow (widget))
END UnmapWidget;


(********************************************************************

                        The High Scores Display

 ********************************************************************)


PROCEDURE ReadHighScores () =
VAR
   rd: READER;
BEGIN
  IF NOT scorefilep THEN RETURN END;
  (* clear the high_scores structure *)        
  FOR i := 0 TO MHSCORE DO
      high_scores[i] := high_score_def
  END;
  TRY rd := FileRd.Open (HIGH_SCORE_FILE);
  EXCEPT
  | OSError.E =>
      scorefilep := FALSE;
      Wr.PutText (Stdio.stderr, 
                    "tetris: can\'t read score file: " &
                    HIGH_SCORE_FILE &
                    " Run with \'-noscore\' to avoid this message.\n");
     RETURN
  END;
  TRY FOR i := 0 TO MHSCORE DO
          high_scores[i].name  := Rd.GetLine (rd);
          high_scores[i].score := Scan.Int (Rd.GetLine (rd));
          high_scores[i].rows  := Scan.Int (Rd.GetLine (rd));
          high_scores[i].level := Scan.Int (Rd.GetLine (rd));
          high_scores[i].date  := Rd.GetLine (rd)
      END
   EXCEPT
      Rd.EndOfFile => 
   END;
   Rd.Close (rd)
END ReadHighScores;


PROCEDURE WriteHighScores () =
VAR
  wr: WRITER;
BEGIN
  TRY wr := FileWr.Open (HIGH_SCORE_FILE);
  EXCEPT
  | OSError.E =>
      scorefilep := FALSE;
      Wr.PutText (Stdio.stderr, 
                    "tetris: can\'t write score file: " &
                    HIGH_SCORE_FILE &
                    " Run with \'-noscore\' to avoid this message.\n");
     RETURN
  END;
  FOR i := 0 TO MHSCORE DO
      Wr.PutText (wr, high_scores[i].name & "\n" &
                      Fmt.Int (high_scores[i].score) & "\n" &
                      Fmt.Int (high_scores[i].rows) & "\n" &
                      Fmt.Int (high_scores[i].level) & "\n" &
                      high_scores[i].date & "\n");
  END;
  Wr.Close (wr)
END WriteHighScores;


PROCEDURE ShowHighScores  (<*UNUSED*> w: Xt.Widget;
                           <*UNUSED*>  event: X.XAnyEventStar; 
                           <*UNUSED*> pars: Xt.StringStar;
                           <*UNUSED*>  npars: Xt.CardinalStar) =
BEGIN
  IF NOT scorefilep THEN RETURN END;
  (* re-read high-score table in case someone else on the network is
   * playing at the same time *)
  ReadHighScores ();
  SetValueText (high_score_item[0], XtN.string,
                "Pos Name       Score  Rows Level  When                      ");
  SetValueText (high_score_item[1], XtN.string, " ");
  FOR i := 0 TO MHSCORE DO
     SetValueText (high_score_item[i+2], XtN.string, " " &
                     Fmt.Pad (Fmt.Int (i+1), 3, ' ', Fmt.Align.Left) &
                     Fmt.Pad (high_scores[i].name, 10, ' ', Fmt.Align.Left) &
                     Fmt.Pad (Fmt.Int (high_scores[i].score), 6) &
                     Fmt.Pad (Fmt.Int (high_scores[i].rows), 6) &
                      Fmt.Pad (Fmt.Int (high_scores[i].level), 6) & "  " &
                     high_scores[i].date);
  END;
  Xt.Popup (score_frame, Xt.GrabExclusive)
END ShowHighScores;


PROCEDURE UpdateHighScores () = 
VAR
  pos: INTEGER;
  strdate: Ctypes.char_star;
  tloc := NEW (UNTRACED REF Ctypes.long);
BEGIN
  IF NOT scorefilep THEN  RETURN END;
  (* re-read high-score table in case someone else on the network is
   * playing at the same time *)
  ReadHighScores ();
  (* Check for previous best score *)
  pos := -1;
  FOR i := 0 TO MHSCORE DO
      IF score >= high_scores[i].score THEN pos := i; EXIT END;
  END;
  IF pos # -1 THEN
     (* Blit the high_score *)
     FOR i := MHSCORE TO pos+1 BY -1 DO 
         high_scores[i] := high_scores[i-1]
     END;
     (* Force the new score *)
     high_scores[pos].name  := user_name;
     high_scores[pos].score := score;
     high_scores[pos].rows  := rows;
     high_scores[pos].level := level;
     EVAL Utime.time (ADR (tloc^));
     strdate := Utime.ctime (ADR (tloc^));
     high_scores[pos].date  := Text.Sub (StoT (strdate), 0, 24);
     (* and write back the new score *)
     WriteHighScores()
  END;
END UpdateHighScores;



(**********************************************************************

                        T I M E R  +  E V E N T S

 **********************************************************************)


VAR
  timer: Xt.IntervalId;


PROCEDURE StartTimer () =
VAR
  interval: Ctypes.unsigned_long;
  pevel: INTEGER;
BEGIN
  pevel := 50 - level;
  IF pevel < 0 THEN pevel := 0 END;
  interval := pevel * 6;
  timer := Xt.AppAddTimeOut (context, interval, MoveBlock, NIL)
END StartTimer;


PROCEDURE MoveBlock (<*UNUSED*> closure: Xt.Pointer; 
                     <*UNUSED*> id: Xt.InputIdStar) =
BEGIN
  StartTimer ();
  IF BlockCanDown (cur_shape, cur_xpos, cur_ypos, cur_rot) THEN
     ShowShape (canvas, cur_shape, cur_xpos, cur_ypos, cur_rot, TRUE);
     cur_ypos := cur_ypos + 1;
  ELSE
     IF cur_ypos < 0 THEN 
        EndGame (toplevel, NIL, NIL, NIL)
     ELSE
        score := score + shapes[cur_shape].forms[cur_rot].points;
        UpdateGrid (cur_shape, cur_xpos, cur_ypos, cur_rot);
        RemoveFullLines (cur_ypos);
        CreateShape (TRUE);
        ShowStatLabel (cur_shape);
        ShowScoreProc (toplevel, NIL, NIL, NIL);
        ShowNext ();
        DrawShadow (cur_shape, cur_xpos, cur_ypos, cur_rot)
     END;
  END;
  ShowShape (canvas, cur_shape, cur_xpos, cur_ypos, cur_rot, FALSE)
END MoveBlock;


PROCEDURE StopTimer () =
BEGIN
  IF timer # 0 THEN 
     Xt.RemoveTimeOut (timer); 
     timer := 0 
  END;
END StopTimer;


PROCEDURE SetEvents () =
BEGIN
  running := TRUE;
  UnmapWidget (start_bt);
  MapWidget (pause_bt);
END SetEvents;


PROCEDURE ClearEvents () =
BEGIN
  running := FALSE;
  UnmapWidget (pause_bt);
  MapWidget (start_bt)
END ClearEvents;

(***********************************************************************

                        A C T I O N S

 ***********************************************************************)

(* "Refresh" Action *)

PROCEDURE RefreshProc (w: Xt.Widget; <*UNUSED*> event: X.XAnyEventStar; 
                       <*UNUSED*> pars: Xt.StringStar; 
                       <*UNUSED*> npars: Xt.CardinalStar) =
BEGIN
  IF w = canvas THEN
     FOR x := 0 TO MWIDTH DO
         FOR y := 0 TO MHEIGHT DO
             IF grid[x, y] # NIL THEN
                ShowRectangle (Xt.Display (w), Xt.XtWindow(w), grid[x, y],
                               x * bsize, y * bsize, bsize, bsize);
             END;
         END;
         ShowShape (canvas, cur_shape, cur_xpos, cur_ypos, cur_rot, FALSE)
     END
  ELSE
     IF w = shadow THEN
        DrawShadow (cur_shape, cur_xpos, cur_ypos, cur_rot);
     ELSE
        IF w = nextobj THEN
           ShowNext ()
        ELSE 
           IF w = stats THEN
              FOR i := 0 TO MSHAPE DO
                  ShowShape (stat_shapes[i], i, 0, 0, 1, FALSE);
              END
           ELSE
           Wr.PutText (Stdio.stderr, 
                       "Hmm. I got a Refresh() for an unrecognized window!\n")
           END;
        END;
     END;
  END;
END RefreshProc;

(* "ShowScore" Action *)

PROCEDURE ShowScoreProc (<*UNUSED*> w: Xt.Widget; 
                         <*UNUSED*> event: X.XAnyEventStar; 
                         <*UNUSED*> pars: Xt.StringStar;
                         <*UNUSED*>  npars: Xt.CardinalStar) =
BEGIN
  SetValueText (score_item, XtN.string, "Score: " & Fmt.Int (score));
  SetValueText (level_item, XtN.string, "Level: " & Fmt.Int (level));
  SetValueText (rows_item,  XtN.string, "Rows:  " & Fmt.Int (rows));
END ShowScoreProc;

(* "Quit" Action *)

PROCEDURE QuitProc (<*UNUSED*> w: Xt.Widget; 
                    <*UNUSED*> event: X.XAnyEventStar; 
                    <*UNUSED*> pars: Xt.StringStar;
                    <*UNUSED*>  npars: Xt.CardinalStar) =
BEGIN
  ClearEvents ();
  StopTimer ();
  Xt.DestroyWidget (toplevel);
  Cstdlib.exit (0)
END QuitProc;

(* "Close" Action *)

PROCEDURE CloseProc (<*UNUSED*> w: Xt.Widget;
                     <*UNUSED*>  event: X.XAnyEventStar; 
                     <*UNUSED*> pars: Xt.StringStar;
                     <*UNUSED*>  npars: Xt.CardinalStar) =
BEGIN
  Xt.Popdown (score_frame);
END CloseProc;

(* "EndGame" Action *)

PROCEDURE EndGame  (w: Xt.Widget; 
                    <*UNUSED*> event: X.XAnyEventStar; 
                    <*UNUSED*> pars: Xt.StringStar;
                    <*UNUSED*>  npars: Xt.CardinalStar) =
BEGIN
  ClearEvents ();
  StopTimer ();
  UnmapWidget (start_bt);
  UnmapWidget (pause_bt);
  SetValueText (game_over, XtN.string, "Game Over");
  UpdateHighScores ();
  ShowHighScores (w, NIL, NIL, NIL)
END EndGame;

(* "Restart" Action *)

PROCEDURE RestartProc (<*UNUSED*> w: Xt.Widget;
                       <*UNUSED*> event: X.XAnyEventStar; 
                       <*UNUSED*> pars: Xt.StringStar;
                       <*UNUSED*>  npars: Xt.CardinalStar) =
BEGIN
  ClearEvents ();
  StopTimer ();
  ClearGame ()
END RestartProc;

(* "Start" Action *)

PROCEDURE StartProc (<*UNUSED*> w: Xt.Widget;
                     <*UNUSED*>  event: X.XAnyEventStar; 
                     <*UNUSED*> pars: Xt.StringStar;
                     <*UNUSED*>  npars: Xt.CardinalStar) =
BEGIN
  IF running THEN RETURN END;
  SetEvents ();
  StartTimer ()
END StartProc;

(* "Pause" Action *)

PROCEDURE PauseProc (<*UNUSED*> w: Xt.Widget;
                     <*UNUSED*>  event: X.XAnyEventStar; 
                     <*UNUSED*> pars: Xt.StringStar;
                     <*UNUSED*>  npars: Xt.CardinalStar) =
BEGIN
  IF NOT running THEN RETURN END;
  ClearEvents();
  StopTimer();
END PauseProc;

(* Moving Actions *)

PROCEDURE LeftProc (<*UNUSED*> w: Xt.Widget;
                    <*UNUSED*>  event: X.XAnyEventStar; 
                    <*UNUSED*> pars: Xt.StringStar;
                    <*UNUSED*>  npars: Xt.CardinalStar) =
BEGIN
  IF NOT running THEN RETURN END;
  IF BlockCanLeft (cur_shape, cur_xpos, cur_ypos, cur_rot) THEN
     ShowShape (canvas, cur_shape, cur_xpos, cur_ypos, cur_rot, TRUE);
     cur_xpos := cur_xpos - 1;
     ShowShape (canvas, cur_shape, cur_xpos, cur_ypos, cur_rot, FALSE);
     DrawShadow (cur_shape, cur_xpos, cur_ypos, cur_rot)
  END;
END LeftProc;

PROCEDURE RightProc (<*UNUSED*> w: Xt.Widget;
                     <*UNUSED*> event: X.XAnyEventStar; 
                     <*UNUSED*> pars: Xt.StringStar;
                     <*UNUSED*> npars: Xt.CardinalStar) =
BEGIN
  IF NOT running THEN RETURN END;
  IF BlockCanRight (cur_shape, cur_xpos, cur_ypos, cur_rot) THEN
     ShowShape (canvas, cur_shape, cur_xpos, cur_ypos, cur_rot, TRUE);
     cur_xpos := cur_xpos + 1;
     ShowShape (canvas, cur_shape, cur_xpos, cur_ypos, cur_rot, FALSE);
     DrawShadow (cur_shape, cur_xpos, cur_ypos, cur_rot)
  END;
END RightProc;

PROCEDURE AntiProc (<*UNUSED*> w: Xt.Widget;
                    <*UNUSED*> event: X.XAnyEventStar; 
                    <*UNUSED*> pars: Xt.StringStar;
                    <*UNUSED*> npars: Xt.CardinalStar) =
VAR
  rot: INTEGER;
BEGIN
  IF NOT running THEN RETURN END;
  rot := (cur_rot + 3) MOD 4;
  IF BlockCanRot (cur_shape, cur_xpos, cur_ypos, rot) THEN
     ShowShape (canvas, cur_shape, cur_xpos, cur_ypos, cur_rot, TRUE);
     cur_rot := rot;
     ShowShape (canvas, cur_shape, cur_xpos, cur_ypos, cur_rot, FALSE);
     DrawShadow (cur_shape, cur_xpos, cur_ypos, cur_rot)
  END;
END AntiProc;

PROCEDURE ClockProc (<*UNUSED*> w: Xt.Widget;
                     <*UNUSED*> event: X.XAnyEventStar; 
                     <*UNUSED*> pars: Xt.StringStar;
                     <*UNUSED*>  npars: Xt.CardinalStar) =
VAR
  rot: INTEGER;
BEGIN
  IF NOT running THEN RETURN END;
  rot := (cur_rot + 1) MOD 4;
  IF BlockCanRot (cur_shape, cur_xpos, cur_ypos, rot) THEN
     ShowShape (canvas, cur_shape, cur_xpos, cur_ypos, cur_rot, TRUE);
     cur_rot := rot;
     ShowShape (canvas, cur_shape, cur_xpos, cur_ypos, cur_rot, FALSE);
     DrawShadow (cur_shape, cur_xpos, cur_ypos, cur_rot)
  END;
END ClockProc;

(* "Fast" Action *)

PROCEDURE DropProc (<*UNUSED*> w: Xt.Widget;
                    <*UNUSED*>  event: X.XAnyEventStar; 
                    <*UNUSED*> pars: Xt.StringStar;
                    <*UNUSED*>  npars: Xt.CardinalStar) =
BEGIN
  IF NOT running THEN RETURN END;
  WHILE BlockCanDown (cur_shape, cur_xpos, cur_ypos, cur_rot) DO
    ShowShape (canvas, cur_shape, cur_xpos, cur_ypos, cur_rot, TRUE);
    cur_ypos := cur_ypos + 1;
    ShowShape (canvas, cur_shape, cur_xpos, cur_ypos, cur_rot, FALSE)
  END;
END DropProc;


(***************************************************************

                        Actions Utilities

 ***************************************************************)


PROCEDURE BlockCanDown (shape, xpos, ypos, rot: INTEGER): BOOLEAN =
VAR
  y, c: INTEGER;
BEGIN
  (* Find highest non-zero y coordinate for each x *)
  FOR x := 0 TO MROT DO
      c := shapes[shape].forms[rot].highesty[x];
      y := ypos + c;
      IF c # 0 AND y >= 0 THEN
         IF y > MHEIGHT OR grid[xpos+x, y] # NIL THEN 
            RETURN (FALSE)
         END;
      END;
  END;
  RETURN (TRUE)
END BlockCanDown;

PROCEDURE BlockCanLeft (shape, xpos, ypos, rot: INTEGER): BOOLEAN =
VAR
  x, yg, c: INTEGER;
BEGIN  
  (* get the lowest x value for y, in (3-c) *)
  yg := ypos;
  FOR y := 0 TO MROT DO
      c := shapes[shape].forms[rot].lowestx[y];
      x := xpos + c;
      IF c # -2 THEN
         IF  x < 0 OR (yg >= 0 AND grid[x, yg] # NIL) THEN
             RETURN (FALSE)
         END;
      END;
      yg := yg + 1;
  END;
  RETURN (TRUE)
END BlockCanLeft;

PROCEDURE BlockCanRight (shape, xpos, ypos, rot: INTEGER): BOOLEAN =
VAR
  x, yg, c: INTEGER;
BEGIN  
  (* get the lowest x value for y, in (3-c) *)
  yg := ypos;
  FOR y := 0 TO MROT DO
      c := shapes[shape].forms[rot].highestx[y];
      x := xpos + c;
      IF c # 0 AND x >= 0 THEN
         IF x = NWIDTH OR (yg >= 0 AND grid[x, yg] # NIL) THEN
            RETURN (FALSE)
         END;
      END;
      yg := yg + 1;
  END;
  RETURN (TRUE)
END BlockCanRight;

PROCEDURE BlockCanRot (shape, xpos, ypos, rot: INTEGER): BOOLEAN =
VAR
  unitson: WORD;
BEGIN  
  unitson := shapes[shape].forms[rot].unitson;
  FOR y := ypos+MROT TO ypos BY -1 DO
      IF y >= 0 THEN
         IF y > MHEIGHT AND Word.And (16_0000000F, unitson) # 0 THEN 
            RETURN (FALSE) 
         END;
         FOR x := xpos + MROT TO xpos BY -1 DO
             IF Word.And (unitson, 1) # 0 AND (x < 0 OR 
                                               x > MWIDTH OR
                                               grid[x, y] # NIL) THEN
                RETURN (FALSE)
             END;
             unitson := Word.Shift (unitson, -1);
         END;
      ELSE
         unitson := Word.Shift (unitson, -4);
      END;  
  END;
  RETURN (TRUE)
END BlockCanRot;


(*********************************************************************

                        Drawing Procedures

 *********************************************************************)


PROCEDURE RemoveFullLines (starty: INTEGER) =
VAR
  ymax: INTEGER;                             (* y max of search *)
  foundfull: INTEGER;                        (* number of full lines *)
  linefull: ARRAY [0..MHEIGHT] OF BOOLEAN;   (* state of each line *)
BEGIN
  foundfull := 0;
  ymax := MIN (starty + 4, MHEIGHT);
  FOR y := starty TO ymax DO
      linefull[y] := TRUE;
      FOR x := 0 TO MWIDTH DO
          IF grid[x, y] = NIL THEN
             linefull[y] := FALSE;
             EXIT
          END
      END;
      IF linefull[y] THEN 
         INC (foundfull);
         X.XFillRectangle (Xt.Display (canvas), Xt.XtWindow (canvas), erasegc,
                            0, y * bsize, bsize * NWIDTH, bsize)
      END;
  END;
  IF (foundfull = 0) THEN RETURN END;

  (* Computes and display the new score *)

  score := score + (10 * foundfull * foundfull);
  rows  := rows + foundfull;
  level := startlevel + rows DIV 10;
  ShowScoreProc (toplevel, NIL, NIL, NIL);

  (* Wait a bit for the user to see it. *)

  X.XFlush (Xt.Display (toplevel));
  Thread.Pause(1.2d+0);
  
  (* Now change the data. *)

  FOR y := starty TO ymax DO
      IF linefull[y] THEN
         FOR y2 := y TO 1 BY -1 DO
             FOR x := 0 TO MWIDTH DO
                 grid[x, y2] := grid[x, y2 - 1]
             END;
         END;
         FOR x := 0 TO MWIDTH DO
             grid[x, 0] := NIL
         END;
         X.XCopyArea  (Xt.Display (toplevel),
                       Xt.XtWindow (canvas), Xt.XtWindow (canvas), movegc,
                       0,0, bsize * NWIDTH, y * bsize, 0, bsize);
         X.XClearArea (Xt.Display (toplevel), Xt.XtWindow (canvas),
                       0, 0, bsize * NWIDTH, bsize, False);
      END;
  END;
  X.XFlush (Xt.Display (toplevel))
END RemoveFullLines;


PROCEDURE DrawShadow (shape, xpos: INTEGER; <*UNUSED*> ypos: INTEGER;
                      rot: INTEGER) =
VAR
  unitson: WORD := shapes[shape].forms[rot].unitson;
  xmax: INTEGER;
BEGIN  
  X.XClearArea (Xt.Display (shadow), Xt.XtWindow (shadow), 0, 0, 0, 0, False);
  xmax := (xpos+3) * bsize;
  FOR x:= xpos * bsize TO  xmax BY bsize DO
      IF Word.And (unitson, 16_00008888) # 0 THEN
          ShowRectangle (Xt.Display (shadow), Xt.XtWindow (shadow), 
                        shapes[shape].gc, x, 0, bsize, bsize);  
      END;
      unitson := Word.Shift (unitson, 1);
  END;
  X.XFlush (Xt.Display (toplevel))
END DrawShadow;


PROCEDURE ShowNext () =
BEGIN
  X.XClearArea (Xt.Display (nextobj), Xt.XtWindow (nextobj), 0,0,0,0, False);
  ShowShape (nextobj, next_shape, 0, 0, next_rot, FALSE)
END ShowNext;


PROCEDURE ShowRectangle (d: X.DisplayStar; win: X.Drawable;
                         gc: X.GC; x, y: INTEGER; w, h: Xt.Cardinal) =
CONST
  DEEP = 1;
BEGIN
  X.XFillRectangle (d, win, gc, x, y, w, h);
  X.XFillRectangle (d, win, shadegc, x+DEEP, y+DEEP, w-DEEP, h-DEEP)
END ShowRectangle;


PROCEDURE ShowStatLabel (index: INTEGER) =
BEGIN
  SetValueText (stat_labels[index], XtN.string, 
                Fmt.Pad (Fmt.Int (stat_totals[index]), 6) &
                Fmt.Pad (Fmt.Int (stat_counts[index]), 6));
END ShowStatLabel;


PROCEDURE ShowShape (w: Xt.Widget; shape, x, y, rot: INTEGER; clear: BOOLEAN) =
VAR
  d: X.DisplayStar;
  win: X.Drawable;
  unitson: WORD;
  xmax, ymax, xmin, ymin: INTEGER;
BEGIN  
  d       := Xt.Display (w);
  win     := Xt.XtWindow (w);
  unitson := shapes[shape].forms[rot].unitson;
  xmax    := (x+3) * bsize;
  ymax    := (y+3) * bsize;
  ymin    := y * bsize;
  xmin    := x * bsize;

  (* Fill or clear the rectangles *)

  FOR  ycor := ymax TO ymin BY -bsize DO
       IF (ycor >= 0) AND (Word.And (unitson, 16_0000000F) # 0) THEN
          FOR xcor := xmax TO xmin BY -bsize DO
              IF Word.And (unitson, 16_00000001) # 0  THEN
                  IF clear THEN
                    X.XClearArea (d, win, xcor, ycor, bsize, bsize, False)
                  ELSE
                    ShowRectangle (d, win, shapes[shape].gc, xcor, ycor, bsize, bsize);
                    X.XFlush (d)
                  END;
               END;
               unitson := Word.Shift (unitson, -1);
           END;
      ELSE
        unitson := Word.Shift (unitson, -4);
      END;
  END;
END ShowShape;


PROCEDURE DefineShapes () =
VAR
  unitson: WORD;

BEGIN

  WITH s = shapes [0] DO 
    s.forms[0].unitson := 16_0f00; s.forms[0].points := 5;  (*      *)
    s.forms[1].unitson := 16_4444; s.forms[1].points := 8;  (* #### *)
    s.forms[2].unitson := 16_0f00; s.forms[2].points := 5;  (*      *)
    s.forms[3].unitson := 16_4444; s.forms[3].points := 8;  (*      *) END;

  WITH s = shapes [1] DO
    s.forms[0].unitson := 16_cc00; s.forms[0].points := 6;  (* ##   *)
    s.forms[1].unitson := 16_cc00; s.forms[1].points := 6;  (* ##   *)
    s.forms[2].unitson := 16_cc00; s.forms[2].points := 6;  (*      *)
    s.forms[3].unitson := 16_cc00; s.forms[3].points := 6;  (*      *) END;

  WITH s = shapes [2] DO
    s.forms[0].unitson := 16_4e00; s.forms[0].points := 5;  (*  #   *)
    s.forms[1].unitson := 16_4640; s.forms[1].points := 5;  (* ###  *)
    s.forms[2].unitson := 16_0e40; s.forms[2].points := 6;  (*      *)
    s.forms[3].unitson := 16_4c40; s.forms[3].points := 5;  (*      *) END;

  WITH s = shapes [3] DO
    s.forms[0].unitson := 16_c600; s.forms[0].points := 6;  (* ##   *)
    s.forms[1].unitson := 16_4c80; s.forms[1].points := 7;  (*  ##  *)
    s.forms[2].unitson := 16_c600; s.forms[2].points := 6;  (*      *)
    s.forms[3].unitson := 16_4c80; s.forms[3].points := 7;  (*      *) END;

  WITH s = shapes [4] DO
    s.forms[0].unitson := 16_6c00; s.forms[0].points := 6;  (*  ##  *)
    s.forms[1].unitson := 16_8c40; s.forms[1].points := 7;  (* ##   *)
    s.forms[2].unitson := 16_6c00; s.forms[2].points := 6;  (*      *)
    s.forms[3].unitson := 16_8c40; s.forms[3].points := 7;  (*      *) END;

  WITH s = shapes [5] DO
    s.forms[0].unitson := 16_2e00; s.forms[0].points := 6;  (*   #  *)
    s.forms[1].unitson := 16_88c0; s.forms[1].points := 7;  (* ###  *)
    s.forms[2].unitson := 16_e800; s.forms[2].points := 6;  (*      *)
    s.forms[3].unitson := 16_c440; s.forms[3].points := 7;  (*      *) END;

  WITH s = shapes [6] DO
    s.forms[0].unitson := 16_e200; s.forms[0].points := 6;  (* ###  *)
    s.forms[1].unitson := 16_44c0; s.forms[1].points := 7;  (*   #  *)
    s.forms[2].unitson := 16_8e00; s.forms[2].points := 6;  (*      *)
    s.forms[3].unitson := 16_c880; s.forms[3].points := 7;  (*      *) END;


  (* First set the highesty values (could have specified these statically, but the
     human cost is too high.  This is one-shot anyway). *)

  FOR s := 0 TO MSHAPE DO
      FOR r := 0 TO MROT DO
          (* set the highesty values. *)
          unitson := shapes[s].forms[r].unitson;
          FOR x := 0 TO MROT DO
              shapes[s].forms[r].highesty[x] := 0;
              shapes[s].forms[r].highestx[x] := 0;
              shapes[s].forms[r].lowestx[x]  := -2;
          END;
          FOR y := MROT TO 0 BY -1 DO
              FOR x := MROT TO 0 BY -1 DO
                  IF  Word.And (unitson, 1) # 0 THEN
                      IF shapes[s].forms[r].highesty[x] = 0 THEN 
                         shapes[s].forms[r].highesty[x] := y+1 
                      END;
                      IF shapes[s].forms[r].highestx[y] = 0 THEN 
                         shapes[s].forms[r].highestx[y] := x+1 
                      END;
                      shapes[s].forms[r].lowestx[y] := x-1;
                  END;
                  unitson := Word.Shift (unitson, -1);
              END;
          END;
      
      (* Now allocate the colored graphics context *)

      gcval.foreground := shapes[s].foreground;
      gcval.background := shapes[s].background;
      shapes[s].gc := X.XCreateGC (Xt.Display (canvas), Xt.XtWindow (canvas), 
                            X.GCForeground + X.GCBackground, gcval);
      END;
  END;
END DefineShapes;


PROCEDURE UpdateGrid (shape, xpos, ypos, rot: INTEGER) =
VAR
  unitson: WORD;
BEGIN
  unitson  := shapes[shape].forms[rot].unitson;
  FOR y := ypos+3 TO ypos BY -1 DO
      FOR x := xpos+3 TO xpos BY -1 DO
          IF x >= 0 AND y >= 0 THEN
             IF Word.And (unitson, 16_00000001) # 0 THEN
                grid[x, y] := shapes[shape].gc
             END;
          END;
          unitson := Word.Shift (unitson, -1);
      END;
  END;
END UpdateGrid;


(* creates a new (randomly chosen) shape and sets the global variables *)
PROCEDURE CreateShape (countp: BOOLEAN) =
BEGIN
  cur_shape  := next_shape;
  IF countp THEN
     INC (stat_counts[cur_shape]);
     INC (stat_totals[cur_shape])
  END;
  cur_rot    := next_rot;
  next_shape := rand.integer () MOD NSHAPE;
  next_rot   := rand.integer () MOD NROT;
  cur_xpos   := (NWIDTH DIV 2) - 1;
  cur_ypos   := -4
END CreateShape;


(************************************************************************

                        I IN I T I A L I Z A T I O N

 ************************************************************************)


PROCEDURE Initialize () =
VAR
  who: Ctypes.char_star;
BEGIN
  DefineShapes ();
  who := Cstdlib.getenv (TtoS ("TETRIS"));
  IF who = NIL THEN who := Cstdlib.getenv (TtoS ("LOGNAME")) END;
  IF who = NIL THEN who := Cstdlib.getenv (TtoS ("USER")) END;
  IF who = NIL THEN who := Uutmp.getlogin () END;
  user_name := StoT (who);
  ClearGame ();
  ReadHighScores ();
END Initialize;


PROCEDURE ClearGame () =
BEGIN
  score     := startscore;
  rows      := startrows;
  level     := startlevel;
  cur_xpos  := 0;
  cur_ypos  := 0;
  FOR i := 0 TO MWIDTH DO
      FOR j := 0 TO MHEIGHT DO
          grid[i, j] := NIL
      END;
  END;
  FOR i := 0 TO MSHAPE DO
      stat_counts[i] := 0;
      ShowShape (stat_shapes[i], i, 0, 0, 1, FALSE);
      ShowStatLabel (i);
  END;
  CreateShape (FALSE);         (* Set up 1st shape *)
  CreateShape (TRUE);          (* Set up next shape *)
  X.XClearArea (Xt.Display (canvas), Xt.XtWindow (canvas), 0, 0, 0, 0, False);
  SetValueText (game_over, XtN.string, "         ");
  ShowScoreProc (toplevel, NIL, NIL, NIL);
  ShowNext ();
  ShowStatLabel (cur_shape);
  DrawShadow (cur_shape, cur_xpos, cur_ypos, cur_rot )
END ClearGame;


PROCEDURE MakeFrames (top: Xt.Widget) =
VAR
  status, buttons, nextlabel,
  tbar, scores_bt: Xt.Widget;
  stbar, statboxl: Xt.Widget;

BEGIN

  (* the global frame: "Frame" *)

  count := 0;
  SetArgI (args, count, XtN.defaultDistance, bsize);
  frame := CreateWidget ("Frame", Xaw.formWidgetClass, top, args, count);

  (* the left frame: LeftFrame, contains the label, the next object,
     and the buttons *)

  count := 0;
  SetArgA (args, count, XtN.fromHoriz, NIL);
  SetArgI (args, count, XtN.defaultDistance, 2 * bsize);
  left_frame := CreateWidget ("LeftFrame", Xaw.formWidgetClass, frame, args, count);

  count := 0;
  SetArgA (args, count, XtN.fromVert, NIL);
  SetArgT (args, count, XtN.label, "        TETRIS\nX11R4 + Modula-3\n     entertainment");
  tbar := CreateWidget ("TitleBar", Xaw.labelWidgetClass, left_frame, args, count);

  count := 0;
  SetArgA (args, count, XtN.fromVert, tbar);
  SetArgT (args, count, XtN.label, " The Next Shape ");
  nextlabel  := CreateWidget ("NextLabel", Xaw.labelWidgetClass, left_frame,args, count);
  count := 0;
  SetArgA (args, count, XtN.fromVert, nextlabel);
  SetArgI (args, count, XtN.width, (bsize * 4));
  SetArgI (args, count, XtN.height, (bsize * 4));
  SetArgT (args, count, XtN.resizable, "FALSE");
  nextobj := CreateWidget ("NextObject", Xaw.simpleWidgetClass, left_frame, args, count);

  count := 0;
  SetArgA (args, count, XtN.fromVert, nextobj);
  status     := CreateWidget ("Status", Xaw.boxWidgetClass, left_frame, args, count);
  score_item := CreateWidget ("Score", Xaw.asciiTextWidgetClass, status);
  level_item := CreateWidget ("Level", Xaw.asciiTextWidgetClass, status);
  rows_item  := CreateWidget ("Rows",  Xaw.asciiTextWidgetClass, status);
  game_over  := CreateWidget ("Game",  Xaw.asciiTextWidgetClass, status);
  
  count := 0;
  SetArgA (args, count, XtN.fromVert, status);
  buttons    := CreateWidget ("Buttons", Xaw.boxWidgetClass, left_frame, args, count);
  start_bt   := CreateWidget ("Start",   Xaw.commandWidgetClass, buttons);
  pause_bt   := CreateWidget ("Pause",   Xaw.commandWidgetClass, buttons);
  newgame_bt := CreateWidget ("NewGame", Xaw.commandWidgetClass, buttons);
  IF scorefilep THEN
     scores_bt := CreateWidget ("Scores", Xaw.commandWidgetClass, buttons);
  END;
  quit_bt    := CreateWidget ("Quit",    Xaw.commandWidgetClass, buttons);

  (* 
   *  the right frame: RightFrame, contains the canvas and the shadow
   *)

  count := 0;
  SetArgA (args, count, XtN.fromHoriz, left_frame);
  SetArgI (args, count, XtN.defaultDistance, bsize);
  right_frame := CreateWidget ("RightFrame", Xaw.formWidgetClass, frame, args, count);

  count := 0;
  SetArgA (args, count, XtN.fromVert, NIL);
  SetArgI (args, count, XtN.width, (bsize * NWIDTH));
  SetArgI (args, count, XtN.height, (bsize * NHEIGHT));
  SetArgI (args, count, XtN.borderWidth, (bsize DIV 4) + 1);
  SetArgT (args, count, XtN.resizable, "FALSE");
  canvas := CreateWidget ("Canvas", Xaw.simpleWidgetClass, right_frame, args, count);

  count := 0;
  SetArgA (args, count, XtN.fromVert, canvas);
  SetArgI (args, count, XtN.width, (bsize * NWIDTH));
  SetArgI (args, count, XtN.height, bsize);
  SetArgI (args, count, XtN.borderWidth, (bsize DIV 4) + 1);
  SetArgT (args, count, XtN.resizable, "FALSE");
  shadow := CreateWidget ("Shadow", Xaw.simpleWidgetClass, right_frame, args, count);

  (* 
   *  the stats frame: StatFrame, contains the shapes statistics
   *)

  count := 0;
  SetArgA (args, count, XtN.fromHoriz, right_frame);
  SetArgI (args, count, XtN.defaultDistance, bsize);
  stat_frame := CreateWidget ("StatFrame", Xaw.formWidgetClass, frame, args, count);

  count := 0;
  SetArgA (args, count, XtN.fromVert, NIL);
  SetArgT (args, count, XtN.label, "           Shape Statistics            ");
  stbar := CreateWidget ("", Xaw.labelWidgetClass, stat_frame, args, count);

  count := 0;
  SetArgA (args, count, XtN.fromVert, stbar);
  SetArgI (args, count, XtN.vSpace, 13);
  statboxl := CreateWidget (" ", Xaw.boxWidgetClass, stat_frame, args, count);
  FOR i := 0 TO MSHAPE DO
      EVAL CreateWidget ("      " & shapenames[i+1] & "      ", Xaw.labelWidgetClass, statboxl);
      stat_labels[i] := CreateWidget (" ", Xaw.asciiTextWidgetClass, statboxl);
  END;

  count := 0;
  SetArgA (args, count, XtN.fromVert, stbar);
  SetArgA (args, count, XtN.fromHoriz, statboxl);
  stats := CreateWidget ("Shapes", Xaw.boxWidgetClass, stat_frame, args, count);
  FOR i := 0 TO MSHAPE DO
      count := 0;
      SetArgI (args, count, XtN.width, (bsize * 4));
      SetArgI (args, count, XtN.height, (bsize * 4));
      SetArgT (args, count, XtN.resizable, "FALSE");
      stat_shapes[i] := CreateWidget (" ", Xaw.simpleWidgetClass, stats, args, count);
  END;

  (* 
   *  the score frame: contains the results and the Close button
   *)

  IF scorefilep THEN
     score_frame := Xt.CreatePopupShell (TtoS ("ScoreFrame"), 
                                         Xaw.transientShellWidgetClass, top);
     score_panel := CreateWidget ("ScorePanel", Xaw.boxWidgetClass, score_frame);
     FOR j:= 0 TO MHSCORE+2 DO
         count := 0;
         SetArgI (args, count, XtN.width, 500);
         high_score_item[j] := CreateWidget ("", Xaw.asciiTextWidgetClass,
                                              score_panel, args, count);
     END;
     EVAL CreateWidget ("Close", Xaw.commandWidgetClass, score_panel);
  END;


  Xt.InstallAllAccelerators (canvas, top);
  Xt.InstallAllAccelerators (shadow, top);
  Xt.InstallAllAccelerators (nextobj, top);
  Xt.InstallAllAccelerators (frame, top);
  Xt.RealizeWidget (top);
  MapWidget (top);
  
  gcval.foreground := foreground;
  gcval.background := background;
  movegc := X.XCreateGC (Xt.Display (top), Xt.XtWindow(top),
                         X.GCForeground + X.GCBackground, gcval);

  gcval.foreground := background;
  gcval.background := foreground;
  gcval.fill_style := X.FillStippled;
  gcval.stipple    := X.XCreateBitmapFromData (Xt.Display (top), Xt.XtWindow (top),
                                     TtoS("\252\000"), 2, 2);
  erasegc := X.XCreateGC (Xt.Display (top), Xt.XtWindow (top), 
                          X.GCForeground + X.GCBackground + 
                             X.GCStipple +  X.GCFillStyle, gcval);

  gcval.foreground := foreground;
  gcval.background := background;
  gcval.fill_style := X.FillStippled;
  gcval.stipple    := X.XCreateBitmapFromData (Xt.Display (top), Xt.XtWindow (top),
                          TtoS("\223\000"), 2, 2);

  shadegc := X.XCreateGC (Xt.Display (top), Xt.XtWindow(top),
                          X.GCForeground + X.GCBackground +
                             X.GCStipple +  X.GCFillStyle, gcval);
END MakeFrames;


PROCEDURE Syntax (cont: Xt.AppContext; call: TEXT) =
BEGIN
    Xt.DestroyApplicationContext (cont);
    Wr.PutText (Stdio.stdout, "Usage: " & call &
                              " [ -noscore] [ -score ] [ -boxsize [<n>]\n");
    Cstdlib.exit (1);
END Syntax;


(*****************************************************************

                           M A I N

 *****************************************************************)


BEGIN
  FOR i := 0 TO LAST (fallbacktext) DO
      fallbacklist[i] := TtoS (fallbacktext[i])
  END;
  fallbacklist[LAST (fallbacktext)+1] := NIL;

  count := 0;
  SetOption (options, count, "-score",  "*useScoreFile",Xrm.optionNoArg, "1");
  SetOption (options, count, "-noscore","*useScoreFile",Xrm.optionNoArg, "0");
  SetOption (options, count, "-boxsize","*boxSize",     Xrm.optionSepArg,"16");
  SetOption (options, count, "-iscore", "*startScore",  Xrm.optionSepArg, "0");
  SetOption (options, count, "-rows",   "*startRows",   Xrm.optionSepArg, "0");
  SetOption (options, count, "-level",  "*startLevel",  Xrm.optionSepArg, "0");

  
  VAR
    name := TtoS ("Tetris");
    argc : Xt.Cardinal := RTLinker.info.argc;
    argv : X.Argv := RTLinker.info.argv;
  BEGIN
    toplevel := Xt.AppInitialize (context, name, options, count, argc, argv,
                                fallbacklist);
    IF argc # 1 THEN Syntax (context, Params.Get (0)) END;
  END;

  count := 0;
  SetResource (reslist, count, "foreground", "Foreground", XtR.Pixel,
               BYTESIZE (resources.foreground),
               ADR (resources.foreground) - ADR (resources^),
               XtR.String, "XtDefaultForeground");
  SetResource (reslist, count, "background", "Background", XtR.Pixel,
               BYTESIZE (resources.background),
               ADR (resources.background) - ADR (resources^),
               XtR.String, "XtDefaultBackground");
  SetResource (reslist, count, "boxSize", "BoxSize", XtR.Dimension,
               BYTESIZE (resources.boxsize),
               ADR (resources.boxsize) - ADR (resources^),
               XtR.String, "16");
  SetResource (reslist, count, "useScoreFile", "Boolean", XtR.Int,
               BYTESIZE (resources.scorefilep),
               ADR (resources.scorefilep) - ADR (resources^),
               XtR.String, "1");
  SetResource (reslist, count, "startScore", "StartScore", XtR.Int,
               BYTESIZE (resources.startscore),
               ADR (resources.startscore) - ADR (resources^),
               XtR.String, "0");
  SetResource (reslist, count, "startRows",  "StartRows",  XtR.Int,
               BYTESIZE (resources.startrows),
               ADR (resources.startrows) - ADR (resources^),
               XtR.String, "0");
  SetResource (reslist, count, "startLevel", "StartLevel", XtR.Int,
               BYTESIZE (resources.startlevel),
               ADR (resources.startlevel) - ADR (resources^),
               XtR.String, "0");

  Xt.GetApplicationResources (toplevel, LOOPHOLE (resources, Xt.Pointer),
                              reslist, count);
  foreground   := resources.foreground;
  background   := resources.background;
  bsize        := MAX (resources.boxsize, 4);
  startscore   := resources.startscore;
  startrows    := resources.startrows;
  startlevel   := resources.startlevel;
  scorefilep   := (resources.scorefilep # 0);

  count := 0;
  SetAction (actions, count, "Refresh", RefreshProc);
  SetAction (actions, count, "ShowScore", ShowScoreProc);
  SetAction (actions, count, "Quit", QuitProc);
  SetAction (actions, count, "Close", CloseProc);
  SetAction (actions, count, "EndGame", EndGame);
  SetAction (actions, count, "NewGame", RestartProc);
  SetAction (actions, count, "Start", StartProc);
  SetAction (actions, count, "Pause", PauseProc);
  SetAction (actions, count, "MoveLeft", LeftProc);
  SetAction (actions, count, "MoveRight", RightProc);
  SetAction (actions, count, "RotateCW", ClockProc);
  SetAction (actions, count, "RotateCCW", AntiProc);
  SetAction (actions, count, "Drop", DropProc);
  SetAction (actions, count, "Scores", ShowHighScores);

  Xt.AppAddActions (context, actions, count);

  count := 0;
  SetResource (reslist, count, "foreground", "Foreground", XtR.Pixel,
               BYTESIZE (resources.foreground),
               ADR (resources.foreground) - ADR (resources^),
               XtR.String, "XtDefaultForeground");
  SetResource (reslist, count, "background", "Background", XtR.Pixel,
               BYTESIZE (resources.background),
               ADR (resources.background) - ADR (resources^),
               XtR.String, "XtDefaultBackground");
  FOR i:= 0 TO MSHAPE
      DO  Xt.GetSubresources (toplevel, LOOPHOLE (resources, Xt.Pointer),
                              TtoS (shapenames[i+1]), TtoS (shapenames[0]),
                              reslist, count, NIL, 0);
          shapes[i].foreground := resources.foreground;
          shapes[i].background := resources.background;
          stat_totals[i] := 0;
      END;

  MakeFrames (toplevel);

  Initialize (); 
  
  Xt.AppMainLoop (context);


END Main.

