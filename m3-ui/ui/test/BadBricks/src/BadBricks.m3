(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Apr 30 14:59:58 PDT 1993 by mjordan  *)
(*      modified on Mon Feb 24 13:47:56 PST 1992 by muller   *)
(*      modified on Thu Jan 23 16:54:39 PST 1992 by kalsow   *)
(*      modified on Tue Nov 19  0:50:54 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:53:59 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE BadBricks EXPORTS Main;
IMPORT AnchorBtnVBT, Axis, BorderedVBT, ButtonVBT, BtnVBTClass, 
  Fmt, Font, HighlightVBT, HVSplit, MenuBtnVBT, PaintOp, Palette,
  Pixmap, Random, ScrnPixmap, ScreenType, Split, Stdio, 
  TextureVBT, TextVBT, Trestle, VBT, VBTClass, Wr, TrestleComm, Thread;

CONST 
  XSize = 10;
  YSize = 30;
  BrickSizeV = 16;
  BrickSizeH = 48;
  SafeZone = 3;

TYPE
  Difficulty = {Easy, Normal, Hard, Desperate, Ridiculous, Absurd};
  RefDifficulty = OBJECT difficulty: Difficulty END;
  Range = {Short, Long};
CONST
  DifficultyProbability = 
    ARRAY Difficulty OF INTEGER {15, 20, 25, 30, 40, 50};
  DifficultyName = 
    ARRAY Difficulty OF TEXT 
      {"Easy", "Normal", "Hard", "Desperate", "Ridiculous", "Absurd"};

TYPE
  State = INTEGER; (* >= 0 => number of good brick neighbors *)

CONST
  NoBrickState = -3;
  UnknownState = -2;
  OKState = -1;

TYPE
  Position = 
    RECORD x,y: INTEGER END;

  Brick = 
    ButtonVBT.T OBJECT
      wall: Wall;
      p: Position;
      good: BOOLEAN := FALSE;
      icon: TextVBT.T;
      border: BorderedVBT.T;
      shown := FALSE;
      state: State;
    METHODS
      EnumerateNeighbors(no: NeighborEnumerator; range: Range) := 
        EnumerateNeighbors;
      Show() := Show;
      ShowAndFlood() := ShowAndFlood;
      EndGameShow() := EndGameShow;
      HighlightOn() := HighlightOn;
      HighlightOff() := HighlightOff;
    OVERRIDES
      shape := BrickShape;
      mouse := BrickMouse;
      newShape := BrickNewShape;
    END;

  NeighborEnumerator = OBJECT
    METHODS
      proc(neighbor: Brick);
    END;
    
VAR 
  brickPaintOp := 
(*    PaintOp.FromRGB(0.808590, 0.329671, 0.060822, *)
(*    PaintOp.FromRGB(0.907576, 0.272571, 0.062211, *)
    PaintOp.FromRGB(0.970077, 0.291340, 0.066498, 
      mode:=PaintOp.Mode.Accurate, bw := PaintOp.BW.UseFg);
  markPaintOp := 
    PaintOp.FromRGB(1.0, 1.0, 0.0,
      mode:=PaintOp.Mode.Accurate);
  darkLinesPaintOp := 
    PaintOp.Pair(PaintOp.Bg,
      PaintOp.FromRGB(0.2, 0.2, 0.2,
      mode:=PaintOp.Mode.Accurate, bw := PaintOp.BW.UseFg));
  darkLinesPaintOpForOK := 
    PaintOp.FromRGB(0.2, 0.2, 0.2,
      mode:=PaintOp.Mode.Accurate, bw := PaintOp.BW.UseFg);
  lightLinesPaintOp := 
    PaintOp.Pair(PaintOp.Bg,
      PaintOp.FromRGB(0.7, 0.7, 0.7,
        mode:=PaintOp.Mode.Accurate, bw := PaintOp.BW.UseFg));
  highlightLinesPaintOp := 
    PaintOp.FromRGB(0.0, 1.0, 1.0,
      mode:=PaintOp.Mode.Accurate);
  concretePaintOp := 
    PaintOp.FromRGB(0.8, 0.8, 0.8,
      mode:=PaintOp.Mode.Accurate);
  brickTextPaintOp := 
    PaintOp.FromRGB(0.0, 0.0, 0.0,
      mode:=PaintOp.Mode.Accurate, bw := PaintOp.BW.UseBg);
  brickColorQuad :=
    PaintOp.MakeColorQuad(brickPaintOp, brickTextPaintOp);
  markColorQuad :=
    PaintOp.MakeColorQuad(markPaintOp, PaintOp.Fg);
  concreteColorQuad :=
    PaintOp.MakeColorQuad(concretePaintOp, PaintOp.Fg);
  msgColorQuad :=
    PaintOp.MakeColorQuad(concretePaintOp, PaintOp.Fg);
  borderTexture := TwoTone(Pixmap.Gray);

PROCEDURE TwoTone(pm: Pixmap.T): Pixmap.T =
(* Return the pixmap which is "pm" on a black-and-white display,
   and "Pixmap.Solid" otherwise. *)
   BEGIN
    RETURN Palette.FromPixmapClosure(NEW(TTClosure, pm := pm))
   END TwoTone;

TYPE TTClosure = Palette.PixmapClosure OBJECT
    pm: Pixmap.T
  OVERRIDES
    apply := TTApply
  END;

PROCEDURE TTApply(cl: TTClosure; st: ScreenType.T): ScrnPixmap.T =
  BEGIN
    IF st.depth = 1 THEN 
      RETURN Palette.ResolvePixmap(st, cl.pm)
    ELSE
      RETURN Palette.ResolvePixmap(st, Pixmap.Solid)
    END
  END TTApply;

PROCEDURE NewBrick(wall: Wall; x,y: CARDINAL): Brick =
  VAR brick: Brick; icon: TextVBT.T; border: BorderedVBT.T;
  BEGIN
    icon := 
      TextVBT.New("", bgFg:=brickColorQuad);
    border := BorderedVBT.New(icon, op:=darkLinesPaintOp, 
      txt := borderTexture);
    brick :=
      NEW(Brick, wall:=wall, p:=Position{x:=x, y:=y}, state := UnknownState,
	icon:=icon, border:=border,
	pre:=BrickHighlightOn, 
	cancel:=BrickHighlightOff, post:=BrickHighlightOff);
    EVAL ButtonVBT.T.init(brick, border, BrickAction);
    RETURN brick;
  END NewBrick;

PROCEDURE NewNoBrick(wall: Wall): Brick =
  VAR icon: TextVBT.T;
  BEGIN
    icon := TextVBT.New("");
    RETURN
      NEW(Brick, wall:=wall, p:=Position{x:=0, y:=0}, state := NoBrickState,
	icon:=icon, border:=BorderedVBT.New(icon), shown:=TRUE);
  END NewNoBrick;

TYPE
  NeighborCounter = NeighborEnumerator OBJECT 
    cnt: INTEGER; 
  END;
  
VAR
  highlightOn := NEW(NeighborEnumerator, proc := HighlightOnProc);
  highlightOff := NEW(NeighborEnumerator, proc := HighlightOffProc);
  neighborCountGood := 
    NEW(NeighborCounter, proc := NeighborCountGood);

PROCEDURE HighlightOnProc(<*UNUSED*>enm: NeighborEnumerator; brick: Brick) =
  BEGIN brick.HighlightOn() END HighlightOnProc;

PROCEDURE HighlightOffProc(<*UNUSED*>enm: NeighborEnumerator; brick: Brick) =
  BEGIN brick.HighlightOff() END HighlightOffProc;

PROCEDURE NeighborCountGood(enm: NeighborCounter; brick: Brick) =
  BEGIN 
    IF brick.good THEN INC(enm.cnt) END;
  END NeighborCountGood;
  
PROCEDURE EnumerateNeighbors(
  self: Brick; 
  no: NeighborEnumerator; 
  range: Range) =
  VAR wall: Wall; p: Position;
  BEGIN
    wall := self.wall;
    p := self.p;
    no.proc(wall.BrickAt(WestOf(p)));
    no.proc(wall.BrickAt(NorthWestOf(p)));
    no.proc(wall.BrickAt(NorthEastOf(p)));
    no.proc(wall.BrickAt(EastOf(p)));
    no.proc(wall.BrickAt(SouthEastOf(p)));
    no.proc(wall.BrickAt(SouthWestOf(p)));
    IF range = Range.Long THEN
      no.proc(wall.BrickAt(WestOf(WestOf(p))));
      no.proc(wall.BrickAt(WestOf(NorthWestOf(p))));
      no.proc(wall.BrickAt(NorthWestOf(NorthWestOf(p))));
      no.proc(wall.BrickAt(NorthWestOf(NorthEastOf(p))));
      no.proc(wall.BrickAt(NorthEastOf(NorthEastOf(p))));
      no.proc(wall.BrickAt(NorthEastOf(EastOf(p))));
      no.proc(wall.BrickAt(EastOf(EastOf(p))));
      no.proc(wall.BrickAt(EastOf(SouthEastOf(p))));
      no.proc(wall.BrickAt(SouthEastOf(SouthEastOf(p))));
      no.proc(wall.BrickAt(SouthEastOf(SouthWestOf(p))));
      no.proc(wall.BrickAt(SouthWestOf(SouthWestOf(p))));
      no.proc(wall.BrickAt(SouthWestOf(WestOf(p))));
   END;
  END EnumerateNeighbors;

PROCEDURE BrickHighlightOn(self: Brick) RAISES {} =
  VAR 
    range: Range; 
    bad, ok, unknown, good: INTEGER;
  BEGIN
    IF NOT self.wall.gameOver AND self.shown THEN
      self.EnumerateNeighbors(highlightOn, self.wall.range);
      IF self.wall.range = Range.Long THEN
        neighborCountGood.cnt := 0;
        self.EnumerateNeighbors(neighborCountGood, Range.Long);
        TextVBT.Put(self.icon, Fmt.Int(neighborCountGood.cnt));
      END;
      
      range := self.wall.range;
      bad := NeighborBadCount(self, range);
      ok := NeighborOKCount(self, range);
      IF range = Range.Short THEN
        unknown := 6 - (bad + ok);
        good := self.state;
      ELSE
        unknown := 18 - (bad + ok);
        neighborCountGood.cnt := 0;
        self.EnumerateNeighbors(neighborCountGood, Range.Long);
        good := neighborCountGood.cnt
      END;
      self.wall.GameStatus(good - ok, unknown);
    END;
  END BrickHighlightOn;
  
PROCEDURE BrickHighlightOff(self: Brick) RAISES {} =
  BEGIN
    IF self.shown THEN
      self.EnumerateNeighbors(highlightOff, self.wall.range);
      IF self.wall.range = Range.Long THEN
        neighborCountGood.cnt := 0;
        self.EnumerateNeighbors(neighborCountGood, Range.Short);
        TextVBT.Put(self.icon, Fmt.Int(neighborCountGood.cnt));
      END;
    END;
  END BrickHighlightOff;

PROCEDURE HighlightOn(self: Brick) RAISES {} =
  BEGIN
    BorderedVBT.SetColor(self.border, highlightLinesPaintOp, borderTexture);
  END HighlightOn;

PROCEDURE HighlightOff(self: Brick) RAISES {} =
  BEGIN
    IF self.shown THEN
      BorderedVBT.SetColor(self.border, lightLinesPaintOp, borderTexture);
   ELSIF self.state = OKState THEN
       BorderedVBT.SetColor(self.border, darkLinesPaintOpForOK, borderTexture);
   ELSE
       BorderedVBT.SetColor(self.border, darkLinesPaintOp, borderTexture);
   END;
  END HighlightOff;

VAR
  neighborBadCount := NEW(NeighborCounter, proc := BadCount);
  neighborOKCount := NEW(NeighborCounter, proc := OKCount);
  
PROCEDURE BadCount(enm: NeighborCounter; brick: Brick )  =
BEGIN
  IF (brick.state = NoBrickState) OR (brick.state >= 0) THEN
    INC(enm.cnt);
  END;
END BadCount;

PROCEDURE OKCount(enm: NeighborCounter; brick: Brick ) =
BEGIN
  IF brick.state = OKState THEN
    INC(enm.cnt);
  END;
END OKCount;

PROCEDURE NeighborBadCount( brick: Brick; range: Range ): INTEGER RAISES {} =
  BEGIN
    neighborBadCount.cnt := 0;
    brick.EnumerateNeighbors(neighborBadCount, range);
    RETURN neighborBadCount.cnt
  END NeighborBadCount;

PROCEDURE NeighborOKCount(self: Brick; range: Range): INTEGER =
  BEGIN
    neighborOKCount.cnt := 0;
    self.EnumerateNeighbors(neighborOKCount, range);
    RETURN neighborOKCount.cnt
  END NeighborOKCount;

VAR
  markBad := NEW(NeighborEnumerator, proc := MarkBad);
  markGood := NEW(NeighborEnumerator, proc := MarkGood);
  
PROCEDURE MarkGood(<*UNUSED*>enm: NeighborEnumerator; brick: Brick ) RAISES {} =
BEGIN
  IF brick.state = UnknownState THEN
    ToggleMarking(brick);
  END;
END MarkGood;

PROCEDURE MarkBad(<*UNUSED*>enm: NeighborEnumerator; brick: Brick )  =
BEGIN
  IF brick.state = UnknownState THEN
    IF brick.good THEN
      brick.wall.GameLost();
    ELSE
      brick.ShowAndFlood();
    END;
  END;
END MarkBad;

PROCEDURE AutoBrick( brick: Brick; range: Range ) RAISES {} =
VAR
  bad, good, ok, unknown: INTEGER;
  p: Position;
BEGIN
  bad := NeighborBadCount(brick, range);
  ok := NeighborOKCount(brick, range);
  IF range = Range.Short THEN
    unknown := 6 - (bad + ok);
    good := brick.state;
  ELSE
    unknown := 18 - (bad + ok);
    neighborCountGood.cnt := 0;
    brick.EnumerateNeighbors(neighborCountGood, Range.Long);
    good := neighborCountGood.cnt
  END;
  IF unknown = 0 THEN RETURN END;
  p := brick.p;
  IF good <= ok THEN
     brick.EnumerateNeighbors(markBad, range);
  ELSIF unknown = good - ok THEN
     brick.EnumerateNeighbors(markGood, range);
  END;
END AutoBrick;

PROCEDURE BrickAction(self: ButtonVBT.T; READONLY cd: VBT.MouseRec) RAISES {} =
  VAR brick: Brick;
  BEGIN
    brick := NARROW(self, Brick);
    IF brick.wall.gameOver THEN RETURN END;
    IF (cd.whatChanged = VBT.Modifier.MouseR) 
	OR (VBT.Modifier.Shift IN cd.modifiers) THEN
      ToggleMarking(brick);
    ELSIF brick.state # OKState THEN
      IF brick.good THEN brick.wall.GameLost();
      ELSE
        IF brick.state = UnknownState THEN
          brick.ShowAndFlood();
        ELSE
          IF (cd.whatChanged = VBT.Modifier.MouseL) OR 
             (brick.wall.difficulty < Difficulty.Hard) THEN
	    AutoBrick( brick, Range.Short);
	  ELSE
	    AutoBrick( brick, Range.Long);
	  END;
	END;
        brick.wall.GameStatus(0, 0);
        IF brick.wall.badBricks = 0 THEN brick.wall.GameWon() END;
      END;
    END;
  END BrickAction;

PROCEDURE ToggleMarking(brick: Brick) =
  BEGIN
    IF brick.state = UnknownState THEN
      brick.state := OKState;
      TextVBT.Put(brick.icon, "ok");
       BorderedVBT.SetColor(brick.border, darkLinesPaintOpForOK, 
         borderTexture);
      TextVBT.SetFont(brick.icon, Font.BuiltIn, markColorQuad);
    ELSIF brick.state = OKState THEN
      brick.state := UnknownState;
       BorderedVBT.SetColor(brick.border, darkLinesPaintOp, borderTexture);
      TextVBT.Put(brick.icon, "");
      TextVBT.SetFont(brick.icon, Font.BuiltIn, brickColorQuad);
    END;
  END ToggleMarking;

PROCEDURE BrickShape(<*UNUSED*>self: VBT.T; ax: Axis.T; <*UNUSED*>n: CARDINAL): VBT.SizeRange =
  VAR range: VBT.SizeRange;
  BEGIN
    CASE ax OF
    | Axis.T.Hor => 
        range.lo:=BrickSizeH; range.hi:=range.lo+1; range.pref:=range.lo;
    | Axis.T.Ver => 
        range.lo:=BrickSizeV; range.hi:=range.lo+1; range.pref:=range.lo;
    END;
    RETURN range;
  END BrickShape;

PROCEDURE BrickMouse(v: Brick; READONLY cd: VBT.MouseRec) RAISES {} =
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      IF (v.wall.difficulty < Difficulty.Desperate) OR 
         (cd.whatChanged = VBT.Modifier.MouseL) THEN
        v.wall.range := Range.Short;
      ELSIF cd.whatChanged = VBT.Modifier.MouseM THEN
        v.wall.range := Range.Long;
      END;
    END;
    
    ButtonVBT.T.mouse(v, cd);
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      v.ready := TRUE;
      v.pre();
      VBT.SetCage(v, VBT.InsideCage)
    ELSE
      IF (cd.clickType = VBT.ClickType.LastUp) AND NOT cd.cp.gone AND v.ready
      THEN
        v.action(v, cd);
        v.post();
      ELSIF v.ready THEN
        v.cancel();
      END;
      v.ready := FALSE
    END
  END BrickMouse;

PROCEDURE BrickNewShape(<*UNUSED*>self: VBT.T; <*UNUSED*>ch: VBT.T) RAISES {} =
  BEGIN
  END BrickNewShape;
  
PROCEDURE WestOf(p: Position): Position =
  BEGIN
    RETURN Position{x:=p.x-1, y:=p.y};
  END WestOf;

PROCEDURE NorthWestOf(p: Position): Position =
  BEGIN
    RETURN Position{x:=p.x-1+(p.y MOD 2), y:=p.y-1};
  END NorthWestOf;

PROCEDURE NorthEastOf(p: Position): Position =
  BEGIN
    RETURN Position{x:=p.x+(p.y MOD 2), y:=p.y-1};
  END NorthEastOf;

PROCEDURE EastOf(p: Position): Position =
  BEGIN
    RETURN Position{x:=p.x+1, y:=p.y};
  END EastOf;

PROCEDURE SouthEastOf(p: Position): Position =
  BEGIN
    RETURN Position{x:=p.x+(p.y MOD 2), y:=p.y+1};
  END SouthEastOf;

PROCEDURE SouthWestOf(p: Position): Position =
  BEGIN
    RETURN Position{x:=p.x-1+(p.y MOD 2), y:=p.y+1};
  END SouthWestOf;

PROCEDURE BrickAt(self: Wall; p: Position): Brick =
  BEGIN
    IF (p.x<0) OR (p.x>=self.xSize) OR
       (p.y<0) OR (p.y>=self.ySize) THEN
      RETURN self.noBrick;
    ELSE
      RETURN self.brick^[p.x,p.y];
    END;
  END BrickAt;

PROCEDURE GameStatus(self: Wall; good, unknown: INTEGER) =
  BEGIN
    IF good = unknown THEN
      TextVBT.Put(self.msgArea, Fmt.F("%s Game:  %s bad bricks left",
        DifficultyName[self.difficulty], Fmt.Int(self.badBricks)));
    ELSE
      TextVBT.Put(self.msgArea, Fmt.F(
      "%s Game:  %s out of %s unknown neighbors are good;   %s bad bricks left",
        DifficultyName[self.difficulty], Fmt.Int(good), Fmt.Int(unknown), 
        Fmt.Int(self.badBricks)));
    END;
  END GameStatus;

PROCEDURE Show(self: Brick) =
  BEGIN
    IF self.shown THEN RETURN END;
    IF NOT self.good THEN
      neighborCountGood.cnt := 0;
      self.EnumerateNeighbors(neighborCountGood, Range.Short);
      self.state := neighborCountGood.cnt;
      TextVBT.Put(self.icon, Fmt.Int(self.state));
      TextVBT.SetFont(self.icon, Font.BuiltIn, concreteColorQuad);
      BorderedVBT.SetColor(self.border, lightLinesPaintOp, borderTexture);
      self.shown := TRUE;
      DEC(self.wall.badBricks);
    END;
  END Show;

PROCEDURE EndGameShow(self: Brick) =
  BEGIN
    IF self.shown THEN RETURN END;
    IF NOT self.good THEN
      neighborCountGood.cnt := 0;
      self.EnumerateNeighbors(neighborCountGood, Range.Short);
      self.state := neighborCountGood.cnt;
      TextVBT.Put(self.icon, Fmt.Int(self.state));
      BorderedVBT.SetColor(self.border, lightLinesPaintOp, borderTexture);
      self.shown := TRUE;
      DEC(self.wall.badBricks);
    END;
  END EndGameShow;

PROCEDURE ShowAndFlood(self: Brick) =
  VAR wall: Wall; p: Position;
  BEGIN
    IF self.shown THEN RETURN END;
    self.Show();
    IF self.state = 0 THEN
      wall := self.wall;
      p := self.p;
      wall.BrickAt(WestOf(p)).ShowAndFlood();
      wall.BrickAt(NorthWestOf(p)).ShowAndFlood();
      wall.BrickAt(NorthEastOf(p)).ShowAndFlood();
      wall.BrickAt(EastOf(p)).ShowAndFlood();
      wall.BrickAt(SouthEastOf(p)).ShowAndFlood();
      wall.BrickAt(SouthWestOf(p)).ShowAndFlood();
    END;
  END ShowAndFlood;

TYPE
  BrickSpace =
    TextureVBT.T OBJECT
    OVERRIDES
      shape := BrickSpaceShape;
    END;

PROCEDURE NewBrickSpace(): BrickSpace =
  VAR brickSpace: BrickSpace;
  BEGIN
    brickSpace := NEW(BrickSpace);
    EVAL TextureVBT.T.init(brickSpace, op:=concretePaintOp);
    RETURN brickSpace;
  END NewBrickSpace;

PROCEDURE BrickSpaceShape(<*UNUSED*>self: VBT.T; ax: Axis.T; <*UNUSED*>n: CARDINAL): VBT.SizeRange =
  VAR range: VBT.SizeRange;
  BEGIN
    CASE ax OF
    | Axis.T.Hor => 
        range.lo:=BrickSizeH DIV 2; range.hi:=range.lo+1; range.pref:=range.lo;
    | Axis.T.Ver => 
        range.lo:=BrickSizeV; range.hi:=range.lo+1; range.pref:=range.lo;
    END;
    RETURN range;
  END BrickSpaceShape;

TYPE
  Wall = 
    OBJECT
      brick: REF ARRAY OF ARRAY OF Brick;
      noBrick: Brick;
      xSize,ySize: CARDINAL;
      badBricks: CARDINAL;
      wallVBT: VBT.T;
      range: Range;
      msgArea: TextVBT.T;
      difficulty: Difficulty;
      gameOver: BOOLEAN := FALSE;
    METHODS
      BrickAt(p: Position): Brick := BrickAt;
      StartGame(difficulty: Difficulty) := StartGame;
      GameStatus(good, unknown: INTEGER) := GameStatus;
      GameLost() := GameLost;
      GameWon() := GameWon;
    END;

PROCEDURE NewWall(xSize,ySize: CARDINAL): Wall =
  VAR bricks: REF ARRAY OF ARRAY OF Brick; rowVBT, colVBT: HVSplit.T;
    wall: Wall;
  BEGIN
    wall := NEW(Wall, brick:=NIL, xSize:=xSize, ySize:=ySize,
		msgArea:=TextVBT.New("", bgFg:=msgColorQuad));
    bricks := NEW(REF ARRAY OF ARRAY OF Brick, xSize, ySize);
    FOR x:=0 TO xSize-1 DO
      FOR y:=0 TO ySize-1 DO
        bricks[x,y] := NewBrick(wall, x, y);
      END;
    END;
    wall.brick := bricks;
    wall.noBrick := NewNoBrick(wall);
    colVBT := HVSplit.New(Axis.T.Ver, adjustable := FALSE);
    Split.AddChild(colVBT, wall.msgArea);
    FOR y:=0 TO ySize-1 DO
      rowVBT := HVSplit.New(Axis.T.Hor, adjustable := FALSE);
      IF (y MOD 2)=1 THEN
	Split.AddChild(rowVBT, NewBrickSpace());
      END;
      FOR x:=0 TO xSize-1 DO
	Split.AddChild(rowVBT, bricks[x,y]);
      END;
      Split.AddChild(rowVBT, TextureVBT.New(op:=concretePaintOp));
      Split.AddChild(colVBT, rowVBT);
    END;
    Split.AddChild(colVBT, TextureVBT.New(op:=concretePaintOp));
    wall.wallVBT := BorderedVBT.New(HighlightVBT.New(colVBT));
    RETURN wall;
  END NewWall;

PROCEDURE StartGame(self: Wall; difficulty: Difficulty) =
  VAR n,i,rx,ry: INTEGER; rand: Random.T; brick: Brick;
  BEGIN
    self.difficulty := difficulty;
    self.gameOver := FALSE;
    IF difficulty >= Difficulty.Desperate THEN
      self.range := Range.Long;
    ELSE
      self.range := Range.Short;
    END;
    TextVBT.Put(self.msgArea, 
     "ClickLeft: remove bad bricks. " & 
     "ClickRight or ShiftClickLeft: mark/unmark bricks.");
    FOR y:=0 TO self.ySize-1 DO
      FOR x:=0 TO self.xSize-1 DO
	brick := self.brick^[x,y];
	brick.good := FALSE;
	TextVBT.Put(brick.icon, "");
	brick.state := UnknownState;
        TextVBT.SetFont(brick.icon, Font.BuiltIn, brickColorQuad);
        BorderedVBT.SetColor(brick.border, darkLinesPaintOp, borderTexture);
        brick.shown := FALSE;
      END;
    END;
    self.badBricks := self.xSize * self.ySize;
    IF (self.xSize>0) AND (self.ySize>0) THEN
      n := ROUND(FLOAT(XSize*YSize*DifficultyProbability[difficulty])/100.0);
      rand := NEW(Random.Default).init();
      i:=0;
      LOOP
        IF i=n THEN EXIT END;
	rx := rand.integer(0, self.xSize-1);
	ry := rand.integer(0, self.ySize-1);
	IF ((rx>=SafeZone) OR (ry>=SafeZone)) AND (NOT self.brick^[rx,ry].good) THEN
          self.brick^[rx,ry].good := TRUE;
	  DEC(self.badBricks);
	  INC(i); 
	END;
      END;
      self.brick^[0,0].ShowAndFlood();
    END;
  END StartGame;

PROCEDURE GameLost(self: Wall) =
  BEGIN
    FOR x:=0 TO self.xSize-1 DO
      FOR y:=0 TO self.ySize-1 DO
	self.brick^[x,y].EndGameShow();
      END;
    END;
    TextVBT.Put(self.msgArea, "OOPS! That was a perfectly good brick!");
    self.gameOver := TRUE;
  END GameLost;

PROCEDURE GameWon(self: Wall) =
  BEGIN
    TextVBT.Put(self.msgArea, 
      "NO BAD BRICKS LEFT! Skateboarding is now safe.");
    self.gameOver := TRUE;
  END GameWon;

VAR 
  wall0: Wall;
  main, menuBar: VBT.T;
  sensorMenuTitle: TextVBT.T;

PROCEDURE DoGame(b: ButtonVBT.T; <*UNUSED*>READONLY cd: VBT.MouseRec) =
  BEGIN
    wall0.StartGame(
      NARROW(VBT.GetProp(b, TYPECODE(RefDifficulty)), RefDifficulty)
	.difficulty);
    IF wall0.difficulty > Difficulty.Hard THEN
      TextVBT.Put(sensorMenuTitle, "  Sensor");
    ELSE
      TextVBT.Put(sensorMenuTitle, "");
    END; 
  END DoGame;

PROCEDURE DoShortRange(<*UNUSED*>b: ButtonVBT.T; <*UNUSED*>READONLY cd: VBT.MouseRec) =
  BEGIN
    wall0.range := Range.Short;
  END DoShortRange;

PROCEDURE DoLongRange(<*UNUSED*>b: ButtonVBT.T; <*UNUSED*>READONLY cd: VBT.MouseRec) =
  BEGIN
    IF wall0.difficulty > Difficulty.Hard THEN
      wall0.range := Range.Long;
    END;
  END DoLongRange;

PROCEDURE QuitGame(<*UNUSED*>b: ButtonVBT.T; <*UNUSED*>READONLY cd: VBT.MouseRec) =
  BEGIN
    Trestle.Delete(main);
  END QuitGame;

PROCEDURE GameMenu(): HVSplit.T =
  VAR menu: HVSplit.T;
  BEGIN
    menu := HVSplit.New(Axis.T.Ver, adjustable := FALSE);
    FOR difficulty:=FIRST(Difficulty) TO LAST(Difficulty) DO
      Split.AddChild(menu,
        MenuBtnVBT.TextItem(DifficultyName[difficulty], DoGame, 
	  NEW(RefDifficulty, difficulty:=difficulty)));
    END;
    Split.AddChild(menu,
      MenuBtnVBT.TextItem("Quit", QuitGame));
    RETURN menu;
  END GameMenu;

PROCEDURE SensorMenu(): HVSplit.T =
  VAR menu: HVSplit.T;
  BEGIN
    menu := HVSplit.New(Axis.T.Ver, adjustable := FALSE);
    Split.AddChild(menu,
      MenuBtnVBT.TextItem("Short range", DoShortRange));
    Split.AddChild(menu,
      MenuBtnVBT.TextItem("Long range", DoLongRange));
    RETURN menu;
  END SensorMenu;

PROCEDURE Deadlock () =
  VAR mu := NEW (MUTEX);
  BEGIN
    Thread.Pause (10.0d0);
    LOCK mu DO LOCK mu DO END END;
  END Deadlock;

  <*FATAL Wr.Failure, Thread.Alerted*>
BEGIN
  wall0           := NewWall(XSize, YSize);
  sensorMenuTitle := TextVBT.New("");
  menuBar :=
    ButtonVBT.MenuBar(AnchorBtnVBT.New(
                        TextVBT.New("  Game"), BorderedVBT.New(GameMenu())));
    (* main := ZSplit.New(HVSplit.Cons(Axis.T.Ver, menuBar,
       HighlightVBT.New(wall0.wallVBT))); *)
  main := HVSplit.Cons(Axis.T.Ver, menuBar, HighlightVBT.New(wall0.wallVBT),
    adjustable := FALSE);
  wall0.StartGame(Difficulty.Normal);
(***** Deadlock();*****)
  TRY
    Trestle.Install(main);
    Trestle.AwaitDelete(main)
  EXCEPT
    TrestleComm.Failure =>
      Wr.PutText(Stdio.stderr, "Can't connect to Trestle Server\n")
  END
END BadBricks.
