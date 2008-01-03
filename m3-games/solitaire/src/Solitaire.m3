(* Copyright (C) 1991, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Tue Jan 31 16:54:02 PST 1995 by kalsow *)
(*      modified on Fri Apr 17 22:06:38 1992 by msm *)
(*      modified on Sat Feb 22 00:20:14 1992 by goldberg@parc.xerox.com *)


MODULE Solitaire EXPORTS Main;

IMPORT AnchorBtnVBT, Axis, BorderedVBT, ButtonVBT, Card,
  Filter, Fmt, Font, HVSplit, Latin1Key, MenuBtnVBT, MouseSplit, PackSplit, 
  PaintOp, Params, Point, Rd, RigidVBT, Solve, Solve2, Split, Stdio, 
  TSplit, Text, TextVBT, TextureVBT, Thread, Trestle, TrestleComm, VBT, Wr, 
  ZSplit, KeyboardKey, Process, Time;
IMPORT RTProcess, IO;

(* IMPORT RTHeapStats; *)
(* IMPORT RTutils, RTCollector; *)

FROM Card IMPORT Width, Height, Overlap, Value, Suit, Real, Family;

CONST
  Gutter = 6; (*16*)
  Gap    = 3; (*8*)

TYPE
  State = REF RECORD
                deck  : Card.StandardDeck;
                pile  : ARRAY [Family.Spades .. Family.Clubs] OF Card.T;
                talon : ARRAY [0 .. 3] OF Card.T;
                layout: ARRAY [0 .. 9] OF Card.T;
                consec, wins, losses, consecWins, consecLosses: INTEGER;
                neverWon                                      : BOOLEAN;
              END;

VAR
  helpDialog, scoreDialog: VBT.T;
  state                  : State;
  buttonFont := Font.FromName(
                  ARRAY OF
                    TEXT{
                    "-*-helvetica-bold-r-*-*-*-120-*-*-*-*-iso8859-1",
                    "-*-menu-medium-r-*-*-*-120-*-*-*-*-iso8859-1",
                    "-*-itc souvenir-demi-r-*-*-*-120-*-*-*-*-iso8859-1",
                    "-*-times-bold-r-*-*-*-120-*-*-*-*-iso8859-1"});
  textFont := Font.FromName(
                ARRAY OF
                  TEXT{
                  "-*-new century schoolbook-medium-r-*-*-*-120-*-*-*-*-iso8859-1",
                  "-*-itc souvenir-demi-r-*-*-*-120-*-*-*-*-iso8859-1",
                  "-*-times-medium-r-*-*-*-120-*-*-*-*-iso8859-1",
                  "-*-helvetica-medium-r-*-*-*-120-*-*-*-*-iso8859-1"});
  msgFont := Font.FromName(
               ARRAY OF
                 TEXT{"-*-helvetica-medium-o-*-*-*-140-*-*-*-*-iso8859-1",
                      "-*-times-medium-o-*-*-*-120-*-*-*-*-iso8859-1"});

PROCEDURE Play (VAR (* out*) a, b: Card.T): BOOLEAN =

  PROCEDURE Playable (c: Card.T): BOOLEAN =
    VAR v: Value;
    BEGIN
      c := c.below;
      IF NOT Real(c) THEN RETURN FALSE END;
      v := c.value;
      DEC(v);
      IF state.pile[c.family].below.value = v THEN
        a := c;
        b := state.pile[c.family].below;
        RETURN TRUE
      ELSE
        RETURN FALSE
      END;
    END Playable;

  VAR won: BOOLEAN;
  BEGIN
    FOR i := 0 TO LAST(state.layout) DO
      IF Playable(state.layout[i]) THEN RETURN TRUE END
    END;
    FOR i := 0 TO LAST(state.talon) DO
      IF Playable(state.talon[i]) THEN RETURN TRUE END
    END;
    won := TRUE;
    FOR s := Family.Spades TO Family.Clubs DO
      IF state.pile[s].below.value # Value.King THEN won := FALSE END
    END;
    IF won AND state.neverWon THEN
      INC(state.wins);
      state.consec := MAX(state.consec, 0) + 1;
      state.consecWins := MAX(state.consecWins, state.consec);
      state.neverWon := FALSE
    END;
    RETURN FALSE
  END Play;

PROCEDURE Obvious (a: Card.T): Card.T =
  VAR
    v: Value;
    s: Suit;
  BEGIN
    IF a.family # Family.Any THEN
      IF (a.value = Value.King) AND (a.below.value # Value.Max) THEN
        FOR i := 0 TO LAST(state.layout) DO
          IF NOT Real(state.layout[i].below) THEN
            RETURN state.layout[i]
          END
        END
      ELSE
        v := a.value;
        s := a.family;
        INC(v);
        FOR i := 0 TO LAST(state.deck) DO
          IF (state.deck[i].family = s) AND (state.deck[i].value = v) THEN
            RETURN state.deck[i]
          END
        END;
      END
    END;
    RETURN NIL
  END Obvious;

PROCEDURE Stupid (a: Card.T): Card.T =
  VAR
    v: Value;
    s: Suit;
  BEGIN
    IF Real(a) THEN
      v := a.value;
      s := a.family;
      DEC(v);
      FOR i := 0 TO LAST(state.deck) DO
        IF (state.deck[i].family = s) AND (state.deck[i].value = v) THEN
          RETURN state.deck[i]
        END
      END
    END;
    RETURN NIL
  END Stupid;

PROCEDURE Trivial (a: Card.T): Card.T =
  BEGIN
    IF Real(a) AND (a.above.value # Value.Talon) THEN
      FOR i := 0 TO LAST(state.talon) DO
        IF NOT Real(state.talon[i].below) THEN RETURN state.talon[i] END
      END
    END;
    RETURN NIL
  END Trivial;

PROCEDURE Attachable (a, b: Card.T): BOOLEAN =
  VAR
    v : Value;
    ap: Card.T;
    tc: INTEGER;
  BEGIN
    IF NOT Real(a) THEN RETURN FALSE END;
    ap := a.above;
    v := a.value;
    tc := 0;
    FOR i := 0 TO LAST(state.talon) DO
      IF state.talon[i].above = state.talon[i] THEN INC(tc) END
    END;
    WHILE Real(ap) AND (ap # a) DO
      IF ap.family # a.family THEN RETURN FALSE END;
      DEC(v);
      IF v # ap.value THEN RETURN FALSE END;
      DEC(tc);
      ap := ap.above
    END;
    IF ap.value = Value.Min THEN RETURN FALSE END;
    IF tc < 0 THEN RETURN FALSE END;
    IF Real(b.above) THEN RETURN FALSE END;
    IF (b.above.value = Value.Talon) AND (b # b.above) THEN
      RETURN FALSE
    END;
    IF (b.family # a.family) AND (b.family # Family.Any) THEN
      RETURN FALSE
    END;
    IF b.value = Value.Talon THEN RETURN NOT Real(a.above) END;
    v := a.value;
    IF b.above.value = Value.Min THEN DEC(v) ELSE INC(v) END;
    RETURN v = b.value
  END Attachable;

PROCEDURE NewDeck (parent: ZSplit.T; state: State) =
  VAR p: Point.T;
  BEGIN
    Card.attachable := Attachable;
    Card.play := Play;
    Card.obvious := Obvious;
    Card.trivial := Trivial;
    Card.stupid := Stupid;
    Card.InitializeStandardDeck(state.deck, parent);
    p.h := Gutter;
    p.v := Gutter;
    FOR st := Family.Spades TO Family.Clubs DO
      state.pile[st] := Card.New(Value.Min, st, p, parent);
      IF st = Family.Hearts THEN
        INC(p.h, 7 * (Width + Gap))
      ELSE
        INC(p.h, Width + Gap)
      END
    END;
    p.h := Gutter + 3 * (Width + Gap);
    p.v := Gutter;
    FOR i := 0 TO LAST(state.talon) DO
      state.talon[i] := Card.New(Value.Talon, Family.Any, p, parent);
      INC(p.h, Width + Gap)
    END;
    p.h := Gutter;
    INC(p.v, Height + Gap);
    FOR i := 0 TO LAST(state.layout) DO
      state.layout[i] := Card.New(Value.Max, Family.Any, p, parent);
      INC(p.h, Width + Gap)
    END
  END NewDeck;

TYPE MyBg = TextureVBT.T OBJECT OVERRIDES shape := Shape END;

PROCEDURE Shape ( <*UNUSED*>ch: MyBg; ax: Axis.T;  <*UNUSED*>n: CARDINAL):
  VBT.SizeRange =
  CONST
    Wid = 2 * Gutter + 9 * Gap + 10 * Width;
    Hei = 2 * Gutter + Gap + 2 * Height + 10 * Overlap;
  BEGIN
    IF ax = Axis.T.Hor THEN
      RETURN
        VBT.SizeRange{lo := Wid, pref := Wid, hi := VBT.DefaultShape.hi}
    ELSE
      RETURN VBT.SizeRange{lo := 0, pref := Hei, hi := VBT.DefaultShape.hi}
    END
  END Shape;

PROCEDURE NewGame (VAR (* out*) state: State; fromStdin: BOOLEAN): VBT.T =
  VAR
    txt           := NEW(MyBg);
    res: ZSplit.T;
  BEGIN
    LOCK VBT.mu DO
      EVAL TextureVBT.T.init(txt, Card.felt.op, Card.felt.txt);
      res := ZSplit.New(txt);
      state := NEW(State);
      state.consec := 0;
      state.wins := 0;
      state.losses := 0;
      state.consecWins := 0;
      state.consecLosses := 0;
      NewDeck(res, state);
      IF fromStdin THEN
        StdinLayout(res, state)
      ELSE
        NewLayout(res, state)
      END;
    END;
    RETURN res;
  END NewGame;

PROCEDURE NewLayout ( <*UNUSED*>v: VBT.T; state: State) =
  <*FATAL Card.BadDeal*>
  VAR
    j   : INTEGER;
    a, b: Card.T;
  BEGIN
    Card.Shuffle(state.deck);
    FOR i := 0 TO LAST(state.deck) DO Card.Detach(state.deck[i]) END;
    Card.Attach(state.deck[0], state.talon[1]);
    Card.Attach(state.deck[1], state.talon[2]);
    j := 0;
    FOR i := 2 TO LAST(state.deck) DO
      Card.Attach(state.deck[i], state.layout[j].below);
      IF j < LAST(state.layout) THEN INC(j) ELSE j := 0 END
    END;
    WHILE Play(a, b) DO Card.Attach(a, b) END;
    state.neverWon := TRUE;
    Card.StartUndoLog()
  END NewLayout;

VAR
  game, board, menu, txtMsg: VBT.T;
  abortButton              : TSplit.T;
  solver                   : Thread.T := NIL;
  solveMutex                          := NEW(MUTEX);
  verbose, solving         : BOOLEAN;
  fromStdin                : BOOLEAN;
  display                  : TEXT := NIL;
  

PROCEDURE TrackingOn ( <*UNUSED*>         button: ButtonVBT.T;  <*UNUSED*>
                                 READONLY cd    : VBT.MouseRec            ) =
  BEGIN
    Card.EnableTracking(TRUE);
  END TrackingOn;

PROCEDURE TrackingOff ( <*UNUSED*>         button: ButtonVBT.T;  <*UNUSED*>
                                  READONLY cd    : VBT.MouseRec            ) =
  BEGIN
    Card.EnableTracking(FALSE);
  END TrackingOff;

PROCEDURE DoNewLayout ( <*UNUSED*>         button: ButtonVBT.T;  <*UNUSED*>
                                  READONLY cd    : VBT.MouseRec            ) =
  BEGIN
    RemoveHelp();
    IF state.neverWon THEN
      INC(state.losses);
      state.consec := MIN(state.consec, 0) - 1;
      state.consecLosses := MAX(state.consecLosses, -state.consec)
    END;
    NewLayout(board, state)
  END DoNewLayout;

PROCEDURE DoUndo ( <*UNUSED*>         button: ButtonVBT.T;  <*UNUSED*>
                             READONLY cd    : VBT.MouseRec            ) =
  BEGIN
    RemoveHelp();
    EVAL Card.Undo()
  END DoUndo;

PROCEDURE DoHint ( <*UNUSED*>button: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
(****
RTutils.Heap (suppressZeros := TRUE,
              presentation := RTutils.HeapPresentation.ByNumber,
              window := 10);
RTCollector.Disable(); RTCollector.Enable(); (* force a full collection *)
RTCollector.Disable(); RTCollector.Enable();
****)
    DoSolve (cd, 0);
  END DoHint;

PROCEDURE DoHint2 ( <*UNUSED*>button: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    DoSolve (cd, 1);
  END DoHint2;

PROCEDURE DoSolve (READONLY cd: VBT.MouseRec;  which_solver: INTEGER) =
  <*FATAL Split.NotAChild*>
  VAR
    howHard: [0 .. 2];
    msg    : TEXT;
  BEGIN
    CASE cd.whatChanged OF
    | VBT.Modifier.MouseL =>
        howHard := 0;
        msg := "checking for a win...";
    | VBT.Modifier.MouseR =>
        howHard := 2;
        msg := "checking for a win...";
    | VBT.Modifier.MouseM =>
        howHard := 1;
        msg := "checking hard for a win...";
    ELSE
      RETURN
    END;
    (* don't solve again, if already solving *)
    IF solving THEN RETURN; END;
    TSplit.SetCurrent(abortButton, Split.Succ(abortButton, NIL));
    RemoveHelp();
    TextVBT.Put(txtMsg, msg);   (* XXX: when remove this msg? *)
    solver := Thread.Fork(NEW(SolveClosure, stackSize := 100000,
                              howHard := howHard, solver := which_solver));
  END DoSolve;

TYPE
  SolveClosure = Thread.SizedClosure OBJECT
    howHard : [0 .. 2];
    solver  : [0..1];
  OVERRIDES
    apply := SolveIt;
  END;

PROCEDURE DoAbort ( <*UNUSED*>         button: ButtonVBT.T;  <*UNUSED*>
                             READONLY cd    : VBT.MouseRec            ) =
  BEGIN
    RemoveHelp();
    IF solver # NIL THEN
    Thread.Alert(solver) END;
  END DoAbort;

PROCEDURE SolveIt (self: SolveClosure): REFANY =
  <*FATAL Split.NotAChild*>
  PROCEDURE ConvertCard (card: Card.T): Solve.CardType =
    VAR res: Solve.CardType;
    BEGIN
      IF NOT Card.Real(card) THEN
        RETURN (Solve.noCard)
      ELSE
        res.suit := VAL(ORD(card.family) - 1, Solve.Suit);
        res.val := ORD(card.value);
        RETURN (res);
      END;
    END ConvertCard;

  VAR
    txt      : TEXT;
    j        := Family.Any;
    layout   : Solve.Layout;
    why      : Solve.WhyStop;
    stateCard: Card.T;
    card     : Solve.CardType;
    lst, nxt : Solve.CardList;
    depth    : CARDINAL;
    breadth  : CARDINAL;
    total    : CARDINAL;
    start    : Time.T;
    elapsed  : Time.T;
  BEGIN
    Thread.Acquire(solveMutex);
    TRY
      solving := TRUE;
      FOR i := 1 TO 4 DO
        INC(j);
        card := ConvertCard(state.pile[j].below);
        IF card # Solve.noCard THEN layout.fnd[i] := card; END;
      END;
      FOR i := 1 TO 4 DO
        layout.tal[i] := ConvertCard(state.talon[i - 1].below);
      END;
      FOR i := 1 TO 10 DO
        stateCard := state.layout[i - 1].below;
        WHILE Card.Real(stateCard) DO stateCard := stateCard.below; END;
        stateCard := stateCard.above;
        card := ConvertCard(stateCard);
        nxt := NIL;
        lst := NIL;
        WHILE Card.Real(stateCard) DO
          lst := NEW(Solve.CardList);
          lst.card := card;
          lst.nxt := nxt;
          nxt := lst;
          stateCard := stateCard.above;
          card := ConvertCard(stateCard);
        END;
        layout.tab[i] := lst;
      END;

      CASE self.howHard OF
      | 0, 2 =>
          (* solves about 87% of games, average time of 25 secs/game *)
          depth   := 2000;
          breadth := 500;
          total   := 100000;
      | 1 =>
          (* for unsolved in previous, solves about 46%, 73 secs/game *)
          depth   := 80000;
          breadth := 2000;
          total   := 100000;
      END;

      start := Time.Now ();
      IF (self.solver = 0) THEN
        txt := Solve.NextMove(layout, why, depth, breadth, total,
                              verbose := verbose, callback := PrintCount);
      ELSE
        txt := Solve2.NextMove(layout, why, depth, breadth, total,
                              verbose := verbose, callback := PrintCount);
      END;
      elapsed := Time.Now () - start;

      IF self.howHard = 2 AND why = Solve.WhyStop.Solution THEN
        txt := "Game is winnable"
      END;                      (* if *)

      IF (elapsed > 0.5d0) THEN
        txt := Fmt.F ("%s (%s sec.)", txt,
                      Fmt.LongReal (elapsed, Fmt.Style.Fix, prec := 2));
      END;

      TextVBT.Put(txtMsg, txt); (* XXX: when remove this msg? *)
      RETURN NIL;
    FINALLY
      solving := FALSE;
      Thread.Release(solveMutex);
      LOCK VBT.mu DO TSplit.SetCurrent(abortButton, NIL); solver := NIL END
    END;
  END SolveIt;

PROCEDURE PrintCount (cnt: CARDINAL) =
  VAR
    txt: TEXT;
    n  : INTEGER;
  BEGIN
    txt := TextVBT.Get(txtMsg);
    IF txt = NIL OR Text.Length(txt) = 0 THEN
      TextVBT.Put(txtMsg, Fmt.F("(%s moves examined)", Fmt.Int(cnt)))
    ELSIF Text.GetChar(txt, Text.Length(txt) - 1) = ')' THEN
      n := Text.FindCharR(txt, '(');
      TextVBT.Put(txtMsg, Fmt.F("%s(%s moves examined)",
                                Text.Sub(txt, 0, n), Fmt.Int(cnt)));
    ELSE
      TextVBT.Put(
        txtMsg, Fmt.F("%s (%s moves examined)", txt, Fmt.Int(cnt)));
    END;
  END PrintCount;

PROCEDURE DoRedo ( <*UNUSED*>         button: ButtonVBT.T;  <*UNUSED*>
                             READONLY cd    : VBT.MouseRec            ) =
  BEGIN
    RemoveHelp();
    EVAL Card.Redo(TRUE)
  END DoRedo;

PROCEDURE DoReset ( <*UNUSED*>         button: ButtonVBT.T;  <*UNUSED*>
                              READONLY cd    : VBT.MouseRec            ) =
  BEGIN
    RemoveHelp();
    WHILE Card.Undo() DO END
  END DoReset;

PROCEDURE DoExit ( <*UNUSED*>         button: ButtonVBT.T;  <*UNUSED*>
                             READONLY cd    : VBT.MouseRec            ) =
  BEGIN
(* RTHeapStats.ReportReachable (); *)
    Trestle.Delete(game)
  END DoExit;

PROCEDURE RemoveHelp () =
  <*FATAL Split.NotAChild*>
  BEGIN
    TextVBT.Put(txtMsg, "");
    TSplit.SetCurrent(board, Split.Succ(board, NIL));
  END RemoveHelp;

PROCEDURE AddText (v: VBT.T; t: TEXT) =
  <*FATAL Split.NotAChild*>
  VAR
    index0, index1, index2: INTEGER;
    word                  : TEXT;
  BEGIN
    index0 := 0;
    REPEAT
      index1 := Text.FindChar(t, ' ', index0);
      IF index1 = -1 THEN
        index2 := Text.Length(t)
      ELSE
        index2 := index1;
      END;
      word := Text.Sub(t, index0, index2 - index0);
      index0 := index1 + 1;
      Split.Insert(v, Split.Pred(v, NIL),
                   TextVBT.New(word, 0.5, 0.0, 0.0, 0.0, fnt := textFont))
    UNTIL index1 = -1
  END AddText;

TYPE Para = PackSplit.T OBJECT OVERRIDES shape := ParaShape END;

PROCEDURE ParaShape (v: Para; ax: Axis.T; n: CARDINAL): VBT.SizeRange
  RAISES {} =
  VAR sh := PackSplit.T.shape(v, ax, n);
  BEGIN
    IF ax # PackSplit.AxisOf(v) THEN
      sh.hi := sh.lo + 1;
      sh.pref := sh.lo
    END;
    RETURN sh
  END ParaShape;

PROCEDURE Paragraph (READONLY t: ARRAY OF TEXT): PackSplit.T =
  VAR res := NEW(Para);
  BEGIN
    EVAL PackSplit.T.init(res, hgap := 1.2, vgap := 0.5);
    FOR i := FIRST(t) TO LAST(t) DO
      IF t[i] # NIL THEN AddText(res, t[i]) END
    END;
    RETURN res
  END Paragraph;

TYPE AT = ARRAY OF TEXT;

PROCEDURE CreateHelp ( <*UNUSED*>z: ZSplit.T) =
  CONST
    Para1  = AT{Text1};
    Text1  = "  This solitaire is better known as Sea Haven Towers.";
    Para3  = AT{Text2, Text3, Text4, Text5, Text6, Text7};
    Text2  = "  The top row contains the four foundations,";
    Text3  = "which are built up in suit from Ace to King.";
    Text4  = "It also contains the four talons, where any single card";
    Text5  = "may be placed. The lower part of the board contains";
    Text6  = "the rest of the deck. Cards there are played";
    Text7  = "descending in suit on the topmost card in a stack.";
    Para5  = AT{Text8, Text9, Text10, Text11};
    Text8  = "  You can only move one card at a time.";
    Text9  = "As a convenience, if there is space, cards will be moved";
    Text10 = "to the talon and back, allowing you to move several cards";
    Text11 = "at a time.";
    Para7  = AT{Text12, Text13, Text14, Text15, Text16, Text16a, Text17};
    Text12 = "  You move a card by pointing to it with the";
    Text13 = "mouse, pressing any button, and dragging the card to";
    Text14 = "its destination. As a shortcut, you can move it to";
    Text15 = "the next higher card in suit by left-clicking.";
    Text16 = "You can move it to the talon by middle-clicking.";
    Text16a = "The right button shows you which card to move onto this one.";
    Text17 = "Cards move to the foundations automatically.";
    Para9 = AT{Text18, Text19, Text20, Text21, Text22, Text23};
    Text18 = "  You can ask for assistance by pressing the 'hint' button.";
    Text19 = "If you press using the left mouse-button, solitaire will";
    Text20 = "try to supply a hint.  If you use the middle button,";
    Text21 = "solitaire will try really hard to supply a hint. The right";
    Text22 = "button causes solitaire to determine only if the current";
    Text23 = "position is solvable or not.";
    Para11 = AT{Text24, Text25};
    Text24 = "  You can invoke any command except 'hint' by typing the";
    Text25 = "capitalized letter in its name.";
    Para13  = AT{Text26};
    Text26 = "  Click on this text to resume play.";
  VAR p1, p3, p5, p7, p9, p11, p13: VBT.T;
  BEGIN
    p1 := Paragraph(Para1);
    p3 := Paragraph(Para3);
    p5 := Paragraph(Para5);
    p7 := Paragraph(Para7);
    p9 := Paragraph(Para9);
    p11 := Paragraph(Para11);
    p13 := Paragraph(Para13);
    helpDialog := HelpSplitCons(ARRAY OF VBT.T{p1, p3, p5, p7, p9, p11, p13});
    Split.AddChild(board, helpDialog)
  END CreateHelp;

PROCEDURE Spacer (): VBT.T =
  CONST
    shape = RigidVBT.Shape{
              RigidVBT.SizeRange{lo := 0.0, pref := 0.0, hi := 1000.0},
              RigidVBT.SizeRange{2.0, 2.0, 2.0}};
  BEGIN
    RETURN RigidVBT.New(TextureVBT.New(op := PaintOp.Bg), shape)
  END Spacer;

PROCEDURE HelpSplitCons (READONLY a: ARRAY OF VBT.T): HelpVBT =
  VAR
    hv, hv2 := HVSplit.New(Axis.T.Ver, adjustable := FALSE);
    res     := NEW(HelpVBT);
  BEGIN
    IF 0 < LAST(a) THEN Split.AddChild(hv, a[0]) END;
    FOR i := 1 TO LAST(a) DO Split.AddChild(hv, Spacer(), a[i]) END;
    Split.AddChild(
      hv2, TextureVBT.New(op := PaintOp.Bg),
      BorderedVBT.New(BorderedVBT.New(hv, 6.0, op := PaintOp.Bg)),
      TextureVBT.New(op := PaintOp.Bg));
    EVAL BorderedVBT.T.init(res, hv2, 5.0, op := PaintOp.Bg);
    RETURN res
  END HelpSplitCons;

TYPE HelpVBT = BorderedVBT.T OBJECT OVERRIDES mouse := HelpMouse END;

PROCEDURE HelpMouse ( <*UNUSED*>         v : HelpVBT;     <*UNUSED*>
                                READONLY cd: VBT.MouseRec           ) =
  BEGIN
    RemoveHelp()
  END HelpMouse;

PROCEDURE DoHelp (v: ButtonVBT.T;  <*UNUSED*>READONLY cd: VBT.MouseRec) =
  <*FATAL Split.NotAChild*>
  BEGIN
    RemoveHelp();
    IF helpDialog = NIL THEN
      CreateHelp(VBT.GetProp(v, TYPECODE(ZSplit.T)))
    END;
    TSplit.SetCurrent(board, helpDialog)
  END DoHelp;

PROCEDURE CreateScore ( <*UNUSED*>z: ZSplit.T) =
  CONST
    Text1 = "  Scoring summary:";
    Text2 = "Wins:";
    Text3 = "Losses:";
    Text4 = "Winning streak:";
    Text5 = "Losing streak:";
    Text6 = "Longest winning streak:";
    Text7 = "Longest losing streak:";
    Text8 = "The current game has been won. ";
    Text9 = "Click on this text to resume play.";
  VAR p1, p2, p3, p4, p5, p6, p7: VBT.T;
  BEGIN
    p1 := Paragraph(AT{Text1});
    p2 := Paragraph(AT{Text2, Fmt.Int(state.wins)});
    p3 := Paragraph(AT{Text3, Fmt.Int(state.losses)});
    IF state.consec >= 0 THEN
      p4 := Paragraph(AT{Text4, Fmt.Int(state.consec)})
    ELSE
      p4 := Paragraph(AT{Text5, Fmt.Int(-state.consec)})
    END;
    p5 := Paragraph(AT{Text6, Fmt.Int(state.consecWins)});
    p6 := Paragraph(AT{Text7, Fmt.Int(state.consecLosses)});
    IF state.neverWon THEN
      p7 := Paragraph(AT{"  ", Text9})
    ELSE
      p7 := Paragraph(AT{Text8, Text9})
    END;
    scoreDialog :=
      HelpSplitCons(ARRAY OF VBT.T{p1, p2, p3, p4, p5, p6, p7});
    Split.AddChild(board, scoreDialog)
  END CreateScore;

PROCEDURE DoScore (v: ButtonVBT.T;  <*UNUSED*>READONLY cd: VBT.MouseRec) =
  <*FATAL Split.NotAChild *>
  BEGIN
    RemoveHelp();
    IF scoreDialog # NIL THEN
      Split.Delete(VBT.Parent(scoreDialog), scoreDialog);
      VBT.Discard(scoreDialog);
      scoreDialog := NIL
    END;
    CreateScore(VBT.GetProp(v, TYPECODE(ZSplit.T)));
    TSplit.SetCurrent(board, scoreDialog)
  END DoScore;

PROCEDURE TBorder (v: VBT.T; op := PaintOp.Fg): VBT.T =
  BEGIN
    RETURN BorderedVBT.New(v, 0.3, op := op)
  END TBorder;

PROCEDURE TButton (name: Text.T; proc: ButtonVBT.Proc): ButtonVBT.T =
  BEGIN
    RETURN
      ButtonVBT.New(TBorder(TextVBT.New(name, fnt := buttonFont)), proc)
  END TButton;

PROCEDURE MButton (name: TEXT; action: ButtonVBT.Proc; ref: REFANY := NIL):
  MenuBtnVBT.T =
  VAR ti := TextVBT.New(name, 0.0, 0.5, 3.0, 0.5, fnt := buttonFont);
  BEGIN
    RETURN MenuBtnVBT.New(ti, action, ref)
  END MButton;

TYPE
  KeyGrab = Filter.T OBJECT
              hasFocus := FALSE;
              mbutton: ButtonVBT.T
            OVERRIDES
              misc  := KeyMisc;
              mouse := KeyMouse;
              key   := KeyKey
            END;

PROCEDURE KeyKey (v: KeyGrab; READONLY cd: VBT.KeyRec) =
  VAR
    cdM: VBT.MouseRec;
    b                 := v.mbutton;
    what := cd.whatChanged;
  BEGIN
    IF cd.wentDown THEN
      cdM.time := cd.time;
      cdM.modifiers := cd.modifiers;
      cdM.whatChanged := VBT.Modifier.Mouse4;
      IF what >= Latin1Key.a AND what <= Latin1Key.z THEN
        what := what + (Latin1Key.A - Latin1Key.a)
      END;
      IF what = Latin1Key.U THEN
        DoUndo(b, cdM)
      ELSIF what = Latin1Key.R THEN
        DoRedo(b, cdM)
      ELSIF what = Latin1Key.N THEN
        DoNewLayout(b, cdM)
      ELSIF what = Latin1Key.Q THEN
        DoExit(b, cdM)
      ELSIF what = Latin1Key.H THEN
        DoHelp(b, cdM)
      ELSIF what = Latin1Key.S THEN
        DoScore(b, cdM)
      ELSIF what = Latin1Key.E THEN
        DoReset(b, cdM)
      ELSIF what = Latin1Key.O THEN
        TrackingOn(b, cdM)
      ELSIF what = Latin1Key.F THEN
        TrackingOff(b, cdM)
      ELSIF what = Latin1Key.A THEN
        DoAbort(b, cdM)
      ELSIF what >= Latin1Key.space AND what <= Latin1Key.ydiaeresis OR
        what = KeyboardKey.Return OR what = KeyboardKey.BackSpace OR
        what = KeyboardKey.Tab OR what = KeyboardKey.Linefeed OR
	what = KeyboardKey.Escape OR what = KeyboardKey.Delete THEN
	RemoveHelp()
      END
    END
  END KeyKey;

PROCEDURE KeyMisc (v: KeyGrab; READONLY cd: VBT.MiscRec) =
  BEGIN
    IF cd.selection = VBT.KBFocus THEN
      IF cd.type = VBT.Lost THEN
        v.hasFocus := FALSE
      ELSIF cd.type = VBT.TakeSelection AND NOT v.hasFocus THEN
        TakeFocus(v, cd.time)
      END
    END;
    Filter.T.misc(v, cd)
  END KeyMisc;

PROCEDURE KeyMouse (v: KeyGrab; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF NOT v.hasFocus AND cd.clickType = VBT.ClickType.FirstDown THEN
      TakeFocus(v, cd.time)
    END;
    MouseSplit.Mouse(v, cd)
  END KeyMouse;

PROCEDURE KeyGrabNew (v: VBT.T; b: ButtonVBT.T := NIL): KeyGrab =
  VAR res := NEW(KeyGrab);
  BEGIN
    EVAL Filter.T.init(res, v);
    res.mbutton := b;
    RETURN res
  END KeyGrabNew;

PROCEDURE TakeFocus (v: KeyGrab; time: VBT.TimeStamp) =
  BEGIN
    TRY
      VBT.Acquire(v, VBT.KBFocus, time);
      v.hasFocus := TRUE
    EXCEPT
      VBT.Error =>
    END
  END TakeFocus;

PROCEDURE GetC (rd: Rd.T): CHAR =
  <*FATAL Rd.Failure, Rd.EndOfFile*>
  BEGIN
    IF Rd.EOF (rd)
      THEN RETURN '\000';
      ELSE RETURN Rd.GetChar (rd);
    END;
  END GetC;

PROCEDURE ScanCard (rd: Rd.T): [0 .. 51] =
  VAR
    val: CARDINAL;
    ch := GetC (rd);
  BEGIN
    (* skip white space *)
    WHILE (ch = ' ') OR (ch = '\n') OR (ch = '\t') DO ch := GetC (rd); END;

    IF (ch < '1') OR ('9' < ch) THEN BadInput () END;
    val := ORD (ch) - ORD ('0');

    ch := GetC (rd);
    IF ('0' <= ch) AND (ch <= '3') THEN
      val := 10 * val + ORD(ch) - ORD('0');
      ch := GetC (rd);
    END;
    IF (val < 1) OR (val > 13) THEN BadInput () END;

    CASE ch OF
    | 'S' => RETURN ((val - 1) * 4 + 0);
    | 'H' => RETURN ((val - 1) * 4 + 1);
    | 'D' => RETURN ((val - 1) * 4 + 2);
    | 'C' => RETURN ((val - 1) * 4 + 3);
    ELSE     BadInput (); RETURN 0;
    END;
  END ScanCard;

PROCEDURE BadInput () =
  BEGIN
    Wr.PutText(Stdio.stderr,
      "Need to provide 52 cards on stdin.\n"
    & "Each card is of the form n[D,S,D,H], 1 <= n <= 13\n");
    Trestle.Delete(game);
  END BadInput;

PROCEDURE StdinLayout ( <*UNUSED*>v: VBT.T; state: State) =
  <*FATAL Card.BadDeal*>
  VAR
    j   : INTEGER;
    a, b: Card.T;
    tmp : ARRAY [0 .. 51] OF Card.T;
  BEGIN
    FOR i := 0 TO 51 DO
      j := ScanCard(Stdio.stdin);
      tmp[i] := state.deck[j];
    END;

    FOR i := 0 TO 51 DO state.deck[i] := tmp[i]; END;

    FOR i := 0 TO LAST(state.deck) DO Card.Detach(state.deck[i]) END;
    Card.Attach(state.deck[0], state.talon[1]);
    Card.Attach(state.deck[1], state.talon[2]);
    j := 0;
    FOR i := 2 TO LAST(state.deck) DO
      Card.Attach(state.deck[i], state.layout[j].below);
      IF j < LAST(state.layout) THEN INC(j) ELSE j := 0 END
    END;
    WHILE Play(a, b) DO Card.Attach(a, b) END;
    state.neverWon := TRUE;
    Card.StartUndoLog()
  END StdinLayout;

<* FATAL Wr.Failure, Thread.Alerted *>

PROCEDURE GetParams () =
  VAR i := 1;  p: TEXT;
  BEGIN
    WHILE i < Params.Count DO
      p := Params.Get (i);
      IF Text.Equal (p, "-v") THEN
        verbose := TRUE;
      ELSIF Text.Equal (p, "-f") THEN
        fromStdin := TRUE;
      ELSIF Text.Equal (p, "-display") THEN
        INC (i);
        IF (i >= Params.Count) THEN BadDisplay () END;
        display := Params.Get (i);
      END;
      INC (i);
    END;
  END GetParams;

PROCEDURE BadDisplay () =
  BEGIN
    Wr.PutText(Stdio.stderr, "solitaire: bad -display parameter\n");
    Process.Exit (1);
  END BadDisplay;

PROCEDURE Main () =
  <* FATAL Split.NotAChild *>
  BEGIN
    Card.EnableHighlight(TRUE, 600);
    GetParams ();
    state := NIL;
    board := TSplit.Cons(NewGame(state, fromStdin));
    txtMsg := TextVBT.New("", fnt := msgFont);
    menu := BorderedVBT.New(
              HVSplit.Cons(
                Axis.T.Ver, MButton("Score", DoScore, board),
                MButton("rEset", DoReset), MButton("Help", DoHelp, board),
                MButton("New game", DoNewLayout),
                MButton("track On", TrackingOn, board),
                MButton("track oFf", TrackingOff, board),
                MButton("Quit", DoExit)));
    abortButton := TSplit.Cons(TButton("Abort", DoAbort), NIL);
    TSplit.SetCurrent(abortButton, NIL);
    game :=
      KeyGrabNew(
        HVSplit.Cons(
          Axis.T.Ver,
          ButtonVBT.MenuBar(
            TBorder(AnchorBtnVBT.New(
                      TextVBT.New("Control", fnt := buttonFont), menu,
                      99999), op := PaintOp.Bg),
            TButton("Undo", DoUndo),
            TButton("Redo", DoRedo),
            TButton("hint", DoHint),
            TButton("hint2", DoHint2),
            abortButton,
            txtMsg),
          board),
        MButton("Bogus", DoExit, board));
    TRY
      IF (display # NIL) THEN
        Trestle.Install(game, trsl := Trestle.Connect(display))
      ELSE
        Trestle.Install(game)
      END;
      Trestle.AwaitDelete(game)
    EXCEPT
      TrestleComm.Failure =>
        Wr.PutText(
          Stdio.stderr, "solitaire: couldn't contact window system\n")
    END
  END Main;

VAR old_handler: RTProcess.InterruptHandler := NIL;

PROCEDURE CtrlC () =
  BEGIN
    IO.Put ("\r\n**Control-C**\r\n");
    IF (old_handler # NIL) THEN old_handler (); END;
  END CtrlC;

BEGIN
  old_handler := RTProcess.OnInterrupt (CtrlC);
  Main();
END Solitaire.
