(* Copyright (C) 1992, Xerox Corporation.                      *)
(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the files COPYRIGHT and COPYRIGHT.extras for details.   *)
(*                                                             *)
(* Last Modified On Tue Jan 31 10:30:09 PST 1995 By kalsow     *)
(* Created on Sat Feb 22 00:20:14 PST 1992 By goldberg@parc.xerox.com *)

MODULE Solve;

IMPORT RefIntTbl, Word, TreeSeq;
IMPORT Thread, Wr, Stdio, Fmt;

TYPE Key = REFANY;

TYPE
  HashLayout = RECORD
                 hash  : INTEGER;
                 layout: Layout;
               END;

PROCEDURE Put (msg: TEXT; flush := FALSE) =
  <* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    Wr.PutText(Stdio.stderr, msg);
    IF flush THEN Wr.Flush(Stdio.stderr); END;
  END Put;

(* return card on 'top' of CardList *)
PROCEDURE Top (lst: CardList): CardType =
  VAR prev: CardList;
  BEGIN
    WHILE lst # NIL DO prev := lst; lst := lst.nxt; END;
    RETURN (prev.card);
  END Top;

(* move king into place. Know that positions >start are nil *)
PROCEDURE MoveKing (VAR tab: Tableau; start: CARDINAL) =
  VAR
    j   : INTEGER;
    card: CardType;
    save: CardList;
  BEGIN
    save := tab[start];
    card := save.card;
    j := 1;
    FOR i := start - 1 TO 1 BY -1 DO
      IF tab[i] # NIL AND Less(Top(tab[i]), card) THEN
        j := i + 1;
        EXIT;
      END;
    END;
    FOR i := start - 1 TO j BY -1 DO tab[i + 1] := tab[i]; END;
    tab[j] := save;
  END MoveKing;

PROCEDURE CollapseTableau (VAR tab: Tableau; start: CARDINAL) =
  BEGIN
    FOR i := start TO 9 DO tab[i] := tab[i + 1]; END;
    tab[10] := NIL;
  END CollapseTableau;

(* compute the new layout resulting from the move loc -> loc1 *)
PROCEDURE NewLayout (READONLY layout: Layout;
                     READONLY loc, loc1: Location): Layout =
  VAR
    res    : Layout;
    card   : CardType;
    save   : CardList;
    sort              := FALSE;
    empty             := FALSE;
    newKing           := FALSE;
  BEGIN
    res := layout;
    (* remove card from loc *)
    CASE loc.grp OF             <* NOWARN *>
    | Group.Tableau =>
        card := res.tab[loc.where].card;
        res.tab[loc.where] := res.tab[loc.where].nxt;
        empty := res.tab[loc.where] = NIL;
    | Group.Talon =>
        card := res.tal[loc.where];
        res.tal[loc.where] := noCard;
        sort := TRUE;
    END;

    (* add card to loc1 *)
    CASE loc1.grp OF
    | Group.Tableau =>
        save := res.tab[loc1.where];
        res.tab[loc1.where] := NEW(CardList);
        res.tab[loc1.where].card := card;
        res.tab[loc1.where].nxt := save;
        newKing := card.val = 13;
    | Group.Talon => res.tal[loc1.where] := card; sort := TRUE;
    | Group.Foundation => res.fnd[loc1.where] := card;
    END;
    (* deal with non-uniqueness of layouts by sorting *)
    IF sort THEN SortTalon(res.tal); END;
    IF empty THEN CollapseTableau(res.tab, loc.where); END;
    IF newKing THEN MoveKing(res.tab, loc1.where); END;
    RETURN (res);
  END NewLayout;

(* add layout to the tree *)
PROCEDURE AddToTree (tree: Tree;  READONLY layout: Layout) =
  VAR
    arr  : REF ARRAY OF Tree;
    n    : INTEGER;
    tree1: Tree;
  BEGIN
    arr := tree.children;
    IF arr # NIL THEN n := NUMBER(arr^); ELSE n := 0; END;
    tree.children := NEW(REF ARRAY OF Tree, n + 1);
    IF n > 0 THEN SUBARRAY(tree.children^, 0, n) := arr^; END;
    tree1 := NEW(Tree);
    tree1.layout := layout;
    tree1.level := tree.level + 1;
    tree.children[n] := tree1;
  END AddToTree;

(* return the card at 'loc' in 'layout' *)
PROCEDURE GetCard (READONLY layout: Layout;
                   READONLY loc: Location): CardType =
  VAR
    lst : CardList;
    card: CardType;
  BEGIN
    CASE loc.grp OF
    | Group.Foundation => card := layout.fnd[loc.where];
    | Group.Talon => card := layout.tal[loc.where];
    | Group.Tableau =>
        lst := layout.tab[loc.where];
        IF lst # NIL THEN
          card := layout.tab[loc.where].card;
        ELSE
          card := noCard;
        END;
    END;
    RETURN (card);
  END GetCard;

(* true if card1 could fit below card2 *)
PROCEDURE Below (READONLY card1, card2: CardType): BOOLEAN =
  BEGIN
    RETURN (card1.suit = card2.suit AND card2.val = card1.val + 1);
  END Below;

(*
 * Returns true if the card at 'loc' has a place to move to.
 * Uses this move to generate a new layout, which is returned in newLayout.
 * If fndOnly set, then only return moves to foundation.
 *
 * The only case in which there are two spots to move to is if the Talon
 * and Tableau are both possible.  FindSpot returns Talon first.  Then
 * a child of this move will contain the other possibility, namely the
 * move to the Tableau.
 *)
PROCEDURE FindSpot (READONLY     layout   : Layout;
                    READONLY     loc      : Location;
                                 fndOnly  : BOOLEAN    := FALSE;
                    VAR (* out*) newLayout: Layout               ): BOOLEAN
  RAISES {Stop} =
  VAR
    card: CardType;
    loc1: Location;
  BEGIN
    card := GetCard(layout, loc);
    (* If card goes on foundation, put it there immediately *)
    FOR i := 1 TO 4 DO
      IF Below(layout.fnd[i], card) THEN
        loc1.grp := Group.Foundation;
        loc1.where := i;
        newLayout := NewLayout(layout, loc, loc1);
        IF NOT AlreadySeen(newLayout) THEN RETURN TRUE; END;
      END;
    END;
    IF fndOnly THEN RETURN FALSE; END;

    (* Don't move a single king to talon if tableau has an open spot. Use
       fact that CollapseTableau moves empty tableau slots to the end. *)
    IF card.val = 13 AND loc.grp = Group.Tableau
         AND layout.tab[loc.where].nxt = NIL AND layout.tab[10] = NIL THEN
      RETURN FALSE;
    END;

    IF loc.grp # Group.Talon THEN
      FOR i := 1 TO 4 DO
        IF layout.tal[i].val = 0 THEN
          loc1.grp := Group.Talon;
          loc1.where := i;
          newLayout := NewLayout(layout, loc, loc1);
          IF NOT AlreadySeen(newLayout) THEN RETURN TRUE; END;
        END;
      END;
    END;

    FOR i := 1 TO 10 DO
      IF (layout.tab[i] # NIL AND Below(card, layout.tab[i].card))
           OR (layout.tab[i] = NIL AND card.val = 13) THEN
        loc1.grp := Group.Tableau;
        loc1.where := i;
        newLayout := NewLayout(layout, loc, loc1);
        IF NOT AlreadySeen(newLayout) THEN RETURN TRUE; END;
      END;
    END;
    RETURN FALSE;
  END FindSpot;

PROCEDURE NumFnd (READONLY layout: Layout): CARDINAL =
  VAR fndSize: CARDINAL := 0;
  BEGIN
    FOR i := 1 TO 4 DO INC(fndSize, layout.fnd[i].val); END;
    RETURN (fndSize);
  END NumFnd;

PROCEDURE Report (READONLY layout: Layout; level: CARDINAL) =
  VAR fndSize: CARDINAL := 0;
  BEGIN
    fndSize := NumFnd(layout);
    IF verbose AND fndSize = 52 THEN
      Put(
        Fmt.F("Win with %s moves.  (%s (%s) layouts, %s htable entries)\n",
              Fmt.Int(level), Fmt.Int(numLayouts), Fmt.Int(numLayouts1),
              Fmt.Int(sizeHTable)), flush := TRUE);
    END;
  END Report;

PROCEDURE ComputeMove (READONLY     layout1, layout2: Layout;
                       VAR (* out*) card            : CardType;
                       VAR (* out*) srcGrp, dstGrp  : Group     ) =
  VAR
    src, dst: BOOLEAN  := FALSE;
    ln1, ln2: CARDINAL := 0;
  BEGIN
    card := noCard;
    srcGrp := Group.Tableau;
    dstGrp := Group.Tableau;
    FOR i := 1 TO 4 DO
      IF layout1.fnd[i] # layout2.fnd[i] THEN
        dst := TRUE;
        dstGrp := Group.Foundation;
        IF Below(layout1.fnd[i], layout2.fnd[i]) THEN
          card := layout2.fnd[i];
        ELSE
          card := layout1.fnd[i];
        END;
      END;
    END;

    FOR i := 1 TO 4 DO
      IF layout1.tal[i] # noCard THEN INC(ln1); END;
      IF layout2.tal[i] # noCard THEN INC(ln2); END;
    END;
    IF ln1 # ln2 THEN
      FOR i := 1 TO 4 DO
        IF layout1.tal[i] # layout2.tal[i] THEN
          IF ln1 > ln2 THEN
            src := TRUE;
            srcGrp := Group.Talon;
            card := layout1.tal[i];
          ELSE
            <* ASSERT NOT dst *>
            dst := TRUE;
            dstGrp := Group.Talon;
            card := layout2.tal[i];
          END;
          EXIT;
        END;
      END;
    END;
    IF src OR dst THEN
      RETURN                    (* srcGrp, dstGrp initialized to Tableau *)
    END;

    ln1 := 0;
    ln2 := 0;
    FOR i := 1 TO 10 DO
      IF layout1.tab[i] # NIL THEN INC(ln1); END;
      IF layout2.tab[i] # NIL THEN INC(ln2); END;
    END;

    PROCEDURE MyBelow (READONLY card1: CardType; card2: CardList): BOOLEAN =
      BEGIN
        IF card2 = NIL THEN
          RETURN (card1.val = 13)
        ELSE
          RETURN (Below(card1, card2.card));
        END;
      END MyBelow;
    BEGIN
      FOR i := 1 TO ln1 DO
        IF NOT MyBelow(layout1.tab[i].card, layout1.tab[i].nxt) THEN
          WITH crd = layout1.tab[i].card DO
            FOR j := 1 TO ln2 DO
              WITH crdlist = layout2.tab[j] DO
                IF crdlist.card = crd AND MyBelow(crd, crdlist.nxt) THEN
                  card := crd;
                  RETURN;
                END;
              END;
            END;
          END;
        END;
      END;
    END;
    <* ASSERT FALSE *>
  END ComputeMove;

(*
 * RecordResult records the winning moves, storing them in resultArr
 * It returns the first move from node of level 0 to node of level 1
 * if known, NIL otherwise.
 *)
VAR
  resultArr: REF ARRAY OF Tree := NIL;
  (* only need Layout, but Tree is a Ref, and Layout is not *)
  resultInd   : CARDINAL;
  accumulating           := FALSE;
PROCEDURE RecordResult (tree: Tree): TEXT =
  VAR
    card    : CardType;
    src, dst: Group;
    n       : INTEGER;
  BEGIN
    IF NOT accumulating THEN
      accumulating := TRUE;
      resultArr := NEW(REF ARRAY OF Tree, tree.level + 1);
      resultInd := 0;
    END;
    resultArr[resultInd] := tree;
    INC(resultInd);
    IF tree.level = 0 THEN
      IF veryVerbose THEN
        FOR i := NUMBER(resultArr^) - 1 TO NUMBER(resultArr^) - 20 BY -1 DO
          ComputeMove(
            resultArr[i].layout, resultArr[i - 1].layout, card, src, dst);
          Put(Fmt.F("%s: %s -> %s\n", FmtCard(card), FmtGroup(src),
                    FmtGroup(dst)));
        END;
      END;
      n := NUMBER(resultArr^) - 1;
      ComputeMove(
        resultArr[n].layout, resultArr[n - 1].layout, card, src, dst);
      accumulating := FALSE;
      resultInd := NUMBER(resultArr^);
      RETURN (Fmt.F("%s: %s -> %s", FmtCard(card), FmtGroup(src),
                    FmtGroup(dst)));
    END;
    RETURN "";
  END RecordResult;

CONST suitNames = ARRAY Suit OF TEXT{"Spade", "Heart", "Diamond", "Club"};

PROCEDURE FmtGroup (grp: Group): TEXT =
  BEGIN
    CASE grp OF
    | Group.Foundation => RETURN ("Foundation");
    | Group.Tableau => RETURN ("Tableau");
    | Group.Talon => RETURN ("Talon");
    END;
  END FmtGroup;

PROCEDURE FmtCard (READONLY card: CardType): TEXT =
  VAR val: TEXT;
  BEGIN
    CASE card.val OF
    | 0 => RETURN ("");
    | 1 .. 10 => val := Fmt.Int(card.val);
    | 11 => val := "Jack";
    | 12 => val := "Queen";
    | 13 => val := "King";
    END;
    RETURN (Fmt.F("%s of %ss ", val, suitNames[card.suit]));
  END FmtCard;

PROCEDURE NumChildren (tree: Tree): CARDINAL =
  BEGIN
    IF tree.children = NIL THEN
      RETURN 0
    ELSE
      RETURN (NUMBER(tree.children^));
    END;
  END NumChildren;

(* do depth-first search of each position on queue *)
PROCEDURE GenerateDepth (queue: TreeSeq.T; VAR (* out*) solution: Tree):
  WhyStop =
  (* Recursively generate tree of all possible layouts. Returns true if
     found solution *)
  PROCEDURE Generate (tree: Tree; level: CARDINAL): BOOLEAN RAISES {Stop} =
    BEGIN
      (* if there is a move to foundation, generate only that move *)
      IF NOT FindChildren(tree, fndOnly := TRUE) THEN
        EVAL FindChildren(tree, fndOnly := FALSE);
      END;
      IF tree.children = NIL THEN
        Report(tree.layout, level);
        IF NumFnd(tree.layout) = 52 THEN
          EVAL RecordResult(tree);
          RETURN TRUE;
        ELSE
          IF level = 0 AND verboseDepth THEN
            Put("No moves (hence no winning move)\n", flush := TRUE);
          END;
          RETURN FALSE;
        END;
      END;

      (* now, call Generate on children *)
      FOR i := 0 TO NumChildren(tree) - 1 DO
        IF Generate(tree.children[i], level + 1) THEN
          EVAL RecordResult(tree);
          RETURN TRUE;
        END;
      END;
      tree.children := NIL;     (* so can garbage collect *)
      IF level = 0 AND (verboseDepth OR verboseNoWin) THEN
        verboseNoWin := FALSE;
        Put(
          Fmt.F(
            "  No winning move on this branch.  (%s layouts, %s htable entries)\n",
            Fmt.Int(numLayouts), Fmt.Int(sizeHTable)), flush := TRUE);
      END;
      RETURN FALSE;
    END Generate;

  PROCEDURE Cnt (tree: Tree) =
    BEGIN
      INC(cnt);
      WITH n = NumFnd(tree.layout) DO
        max := MAX(max, n);
        min := MIN(min, n);
      END;
    END Cnt;

  PROCEDURE CntI (tree: Tree) =
    BEGIN
      IF NumFnd(tree.layout) = i THEN INC(cnt); END
    END CntI;

  PROCEDURE ProbeDepth (tree: Tree) RAISES {Stop} =
    BEGIN
      IF NumFnd(tree.layout) = i THEN
        IF verboseDepth THEN
          INC(cnt);
          Put(Fmt.F(
                "starting a depth first search:  %s cards on foundation\n",
                Fmt.Int(i)), flush := TRUE);
          IF cnt > 5 THEN
            verboseDepth := FALSE;
            Put("...\n", flush := TRUE);
          END;
        END;
        numLayouts1 := 0;
        TRY
          IF Generate(tree, 0) THEN
            solution := tree;
            RAISE Stop(WhyStop.Solution);
          END;
        EXCEPT
          Stop (arg) =>
            CASE (arg) OF
            | WhyStop.GiveUp => good := FALSE;
            | WhyStop.Exhausted => RAISE Stop(WhyStop.Exhausted);
            | WhyStop.Aborted => RAISE Stop(WhyStop.Aborted);
            | WhyStop.Solution => RAISE Stop(WhyStop.Solution);
            | WhyStop.NoSolution =>     <* ASSERT FALSE *>
            END;
        END;
      END;
    END ProbeDepth;

  VAR
    cnt, max, min: CARDINAL;
    good                    := TRUE;
    i            : INTEGER;     (* global so can pass to CntI *)
  BEGIN
    cnt := 0;
    max := 0;
    min := LAST(INTEGER);
    (* queue.map(Cnt) *)
    FOR i := 0 TO queue.size () - 1 DO Cnt (queue.get (i)); END;
    IF veryVerbose THEN
      Put(Fmt.F("%s leaf nodes: ", Fmt.Int(cnt)));
      i := min;
      WHILE i <= max DO
        cnt := 0;
        (* queue.map(CntI); *)
        FOR i := 0 TO queue.size () - 1 DO CntI (queue.get (i)); END;
        Put(Fmt.F("%s with %s, ", Fmt.Int(cnt), Fmt.Int(i)));
        INC(i);
      END;
      Put("\n", flush := TRUE);
    END;
    i := max;
    WHILE i >= min DO
      verboseDepth := veryVerbose;
      verboseGiveUp := veryVerbose;
      verboseNoWin := veryVerbose;
      cnt := 0;
      TRY
        (* queue.map(ProbeDepth); *)
        FOR i := 0 TO queue.size () - 1 DO ProbeDepth (queue.get (i)); END;
      EXCEPT
        Stop (why) =>
          CASE (why) OF
          | WhyStop.Exhausted => RETURN (WhyStop.Exhausted);
          | WhyStop.Aborted => RETURN (WhyStop.Aborted);
          | WhyStop.Solution => RETURN (WhyStop.Solution);
          | WhyStop.GiveUp, WhyStop.NoSolution => <* ASSERT FALSE *>
          END;
      END;
      DEC(i);
    END;
    IF good THEN
      IF verbose THEN
        Put(Fmt.F("No winning move.     (%s layouts, %s htable entries)\n",
                  Fmt.Int(numLayouts), Fmt.Int(sizeHTable)), flush := TRUE);
      END;
      RETURN WhyStop.NoSolution;
    ELSE
      IF verbose THEN
        Put(
          Fmt.F(
            "Give Up. No win after %s layouts generated.  (%s htable entries)\n",
            Fmt.Int(numLayouts), Fmt.Int(sizeHTable)), flush := TRUE);
      END;
      RETURN WhyStop.GiveUp;
    END;
  END GenerateDepth;
(*
 * Identify children, add them to tree.  If fndonly, then only look for
 * moves to foundation, and if found, only add a single child.
 * The return value only meaningful when fndOnly is TRUE,
 * in which case returns TRUE if added a child.
 *)
PROCEDURE FindChildren (tree: Tree; fndOnly: BOOLEAN): BOOLEAN
  RAISES {Stop} =
  VAR
    loc      : Location;
    newLayout: Layout;
  BEGIN
    loc.grp := Group.Tableau;
    FOR i := 1 TO 10 DO
      IF tree.layout.tab[i] # NIL THEN
        loc.where := i;
        IF FindSpot(tree.layout, loc, fndOnly, newLayout) THEN
          AddToTree(tree, newLayout);
          IF fndOnly THEN RETURN TRUE END;
        END;
      END;
    END;

    loc.grp := Group.Talon;
    FOR i := 1 TO 4 DO
      IF tree.layout.tal[i].val > 0 THEN
        loc.where := i;
        IF FindSpot(tree.layout, loc, fndOnly, newLayout) THEN
          AddToTree(tree, newLayout);
          IF fndOnly THEN RETURN TRUE END;
        END;
      END;
    END;
    RETURN FALSE;
  END FindChildren;

(*
 * Print path in tree, from root to leaf, but don't print leaf.
 * Returns move that goes from root to next node.
 *)
PROCEDURE PrintTree (root, leaf: Tree; VAR (* out*) txt: TEXT): BOOLEAN =
  BEGIN
    IF root = leaf THEN RETURN TRUE; END;
    IF root.level >= leaf.level THEN RETURN FALSE; END;
    FOR i := 0 TO NumChildren(root) - 1 DO
      IF PrintTree(root.children[i], leaf, txt) THEN
        txt := RecordResult(root);
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END PrintTree;

(*
 * Generate tree of all possible layouts in level order, then
 * call GenerateDepth on each leaf.  Return txt for MsgVbt.
 *)
PROCEDURE GenerateBreadth (tree: Tree; VAR whyStop: WhyStop): TEXT =
  VAR
    queue    := NEW(TreeSeq.T).init();
    root     : Tree;
    res      := "game done!";
    curLevel := 0;
    branch   : Tree;     (* branch of depth tree that leads to solution *)
  BEGIN
    root := tree;
    queue.addhi(tree);
    WHILE queue.size() > 0 DO
      tree := queue.remlo();
      IF tree.level # curLevel THEN
        curLevel := tree.level;
        IF numLayouts > cutOver THEN
          actualCut := numLayouts;
          queue.addhi(tree);
          depthLim := userDepthLim;
	  whyStop := GenerateDepth(queue, branch);
          CASE whyStop OF
          | WhyStop.Solution => EVAL PrintTree(root, branch, res);
          | WhyStop.NoSolution => res := "Game is not winnable";
          | WhyStop.Aborted => res := "Aborted";
          | WhyStop.GiveUp, WhyStop.Exhausted =>
              res :=
                "Couldn't find winning move.  Middle click Hint for deep search";
          END;
          RETURN res;
        END;
        IF veryVerbose THEN
          Put(
            Fmt.F("    level %s, %s layouts examined, htable size at %s\n",
                  Fmt.Pad(Fmt.Int(curLevel), 3),
                  Fmt.Pad(Fmt.Int(numLayouts), 6), Fmt.Int(sizeHTable)),
            flush := TRUE);
        END;
      END;

      TRY
        IF NOT FindChildren(tree, fndOnly := TRUE) THEN
          EVAL FindChildren(tree, fndOnly := FALSE);
        END;
      EXCEPT
        Stop (why) =>
          IF why = WhyStop.Aborted THEN
	    whyStop := why;
            RETURN ("Aborted")
          ELSE                  <* ASSERT FALSE *>
          END;
      END;
      IF tree.children = NIL THEN
        Report(tree.layout, tree.level);
        IF NumFnd(tree.layout) = 52 THEN
          EVAL PrintTree(root, tree, res);
	  whyStop := WhyStop.Solution;
          RETURN res;
        END;
      END;
      FOR i := 0 TO NumChildren(tree) - 1 DO
        queue.addhi(tree.children[i]);
      END;
    END;                        (* WHILE *)
    IF verbose THEN
      Put(Fmt.F("No winning move.     (%s layouts, %s htable entries)\n",
                Fmt.Int(numLayouts), Fmt.Int(sizeHTable)), flush := TRUE);
    END;
    whyStop := WhyStop.NoSolution;
    RETURN ("Game is not winnable");
  END GenerateBreadth;

TYPE
  LayoutTable = RefIntTbl.Default OBJECT OVERRIDES
    keyEqual := EqualProc;
    keyHash  := HashProc;
  END;

VAR tbl: LayoutTable;

PROCEDURE HashProc (<*UNUSED*>tbl: LayoutTable;  READONLY key: Key): Word.T =
  CONST konst = 9;
  VAR
    h        : REF HashLayout;
    sum, sum1                 := 0;
    ans      : INTEGER;
    shft                      := 0;
  BEGIN
    INC(hashCnt);
    h := key;
    WITH x = h.layout DO
      FOR i := 1 TO 4 DO
        sum := Word.Plus (Word.Times (konst, sum), x.fnd[i].val);
      END;
      FOR i := 1 TO 4 DO
        sum1 := Word.Plus (sum1, Word.Shift(ORD(x.tal[i].suit), shft));
        INC(shft, 2);
        sum := Word.Plus (Word.Times (konst, sum), x.tal[i].val);
      END;
      FOR i := 1 TO 10 DO
        WITH lst = x.tab[i] DO
          IF lst # NIL THEN
            sum1 := Word.Plus (sum1, Word.Shift(ORD(lst.card.suit), shft));
            INC(shft, 2);
            sum := Word.Plus (Word.Times (konst, sum), lst.card.val);
          ELSE
            sum := Word.Times (konst, sum); (* EXIT *)
          END;
        END;
      END;
    END;
    ans := Word.Plus (sum, sum1);
    h.hash := ans;
    RETURN (ans);
  END HashProc;

PROCEDURE EqualProc (<*UNUSED*>tbl: LayoutTable; READONLY a, b: Key): BOOLEAN =
  VAR h1, h2: REF HashLayout;
  BEGIN
    INC(eqCnt);
    h1 := a;
    h2 := b;
    IF h1.hash # h2.hash THEN INC(fastCnt); RETURN FALSE; END;
    RETURN (EqualLayout(h1.layout, h2.layout));
  END EqualProc;

PROCEDURE EqualLayout (READONLY x, y: Layout): BOOLEAN =
  VAR lst1, lst2: CardList;
  BEGIN
    IF x.fnd = y.fnd AND x.tal = y.tal THEN
      FOR i := 1 TO 10 DO
        lst1 := x.tab[i];
        lst2 := y.tab[i];
        WHILE lst1 # NIL DO
          IF lst2 = NIL OR lst1.card # lst2.card THEN RETURN FALSE END;
          lst1 := lst1.nxt;
          lst2 := lst2.nxt;
        END;
        IF lst2 # NIL THEN RETURN FALSE END;
      END;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END EqualLayout;

EXCEPTION Stop(WhyStop);

VAR
  numLayouts1: CARDINAL;
  numLayouts : CARDINAL;
  sizeHTable : CARDINAL;
PROCEDURE AlreadySeen (READONLY layout: Layout): BOOLEAN RAISES {Stop} =
  VAR hit: BOOLEAN;
  BEGIN
    INC(numLayouts);
    INC(numLayouts1);
    IF numLayouts MOD 512 = 0 THEN
      IF Thread.TestAlert() THEN
        RAISE Stop(WhyStop.Aborted);
      END;
      IF callback # NIL THEN callback(numLayouts); END;
    END;
    IF veryVerbose AND numLayouts MOD 8192 = 0 THEN
      Put(Fmt.F("    %s layouts examined, htable size at %s\n",
                Fmt.Pad(Fmt.Int(numLayouts), 6), Fmt.Int(sizeHTable)),
          flush := TRUE);
    END;

    IF numLayouts1 >= depthLim THEN
      IF verboseGiveUp THEN
        verboseGiveUp := FALSE;
        Put(
          Fmt.F(
            "  Give up in this subtree after %s layouts.  (%s htable entries)\n",
            Fmt.Int(numLayouts1), Fmt.Int(sizeHTable)), flush := TRUE);
      END;
      RAISE Stop(WhyStop.GiveUp);
    END;

    IF numLayouts >= lim THEN
      IF verbose THEN
        Put(
          Fmt.F(
            "Give up. No win after %s layouts generated.  (%s htable entries)\n",
            Fmt.Int(numLayouts), Fmt.Int(sizeHTable)), flush := TRUE);
      END;
      RAISE Stop(WhyStop.Exhausted);
    END;

    layoutPool.layout := layout;
    hit := tbl.put(layoutPool, 0);

    IF hit THEN
      hit := hit;
    ELSE
      INC(sizeHTable);
      layoutPool := NEW(REF HashLayout);
    END;
    RETURN hit;
  END AlreadySeen;

PROCEDURE NextMove (<*NOWARN*> layout : Layout;
                    VAR whyStop   : WhyStop;
                    depth, breadth: CARDINAL;
                    total         : CARDINAL;
                    vbose         : BOOLEAN;
                    callbck       : Callback  ): TEXT =
  <* FATAL Stop *>
  PROCEDURE Initialize () =
    BEGIN
      depthLim := LAST(INTEGER);
      tbl := NEW (LayoutTable).init (20000);
      numLayouts1 := 0;
      numLayouts := 0;
      sizeHTable := 0;
      eqCnt := 0;
      hashCnt := 0;
      fastCnt := 0;
      EVAL AlreadySeen(layout);
      tree := NEW(Tree);
      tree.layout := layout;
      tree.level := 0;
    END Initialize;
  VAR
    txt     : TEXT;
    tree    : Tree;
    card    : CardType;
    src, dst: Group;
  BEGIN
    Sort(layout);
    IF resultArr # NIL THEN
      FOR i := resultInd - 1 TO MAX(resultInd - 20, 1) BY -1 DO
        IF EqualLayout(layout, resultArr[i].layout) THEN
          resultInd := i + 1;
          ComputeMove(
            resultArr[i].layout, resultArr[i - 1].layout, card, src, dst);
	  whyStop := WhyStop.Solution;
          RETURN Fmt.F("%s: %s -> %s", FmtCard(card), FmtGroup(src),
                       FmtGroup(dst));
        END;
      END;
    END;
    (* make args global *)
    lim := total;
    userDepthLim := depth;
    cutOver := breadth;
    callback := callbck;
    verbose := vbose;

    Initialize();
    txt := GenerateBreadth(tree, whyStop);
    IF veryVerbose THEN Put(txt & "\n", flush := TRUE); END;

    (* give the garbage collector a chance... *)
    EVAL tbl.init (0);
    tbl := NIL;

    RETURN (txt);
  END NextMove;

(*--------------------------------------------------------------- sorting ---*)

PROCEDURE Sort (VAR layout: Layout) =
  BEGIN
    SortTableau (layout.tab);
    SortTalon (layout.tal);
  END Sort;

PROCEDURE SortTableau (VAR x: Tableau) =
  (* simple insertion sort *)
  VAR j: INTEGER;  key: CardList;
  BEGIN
    FOR i := FIRST (x) + 1 TO LAST (x) DO
      key := x[i];
      j := i - 1;
      WHILE (j >= FIRST (x)) AND CompareCardList (key, x[j]) < 0 DO
        x[j+1] := x[j];
        DEC (j);
      END;
      x[j+1] := key;
    END;
  END SortTableau;

PROCEDURE CompareCardList (x1, x2: CardList): [-1 .. 1] =
  (* nocard is high *)
  BEGIN
    IF    x1 = NIL THEN RETURN 1
    ELSIF x2 = NIL THEN RETURN -1
    ELSIF Less(Top(x1), Top(x2)) THEN RETURN -1
    ELSE RETURN 1;
    END;
  END CompareCardList;

PROCEDURE SortTalon (VAR x: Talon) =
  (* simple insertion sort *)
  VAR j: INTEGER;  key: CardType;
  BEGIN
    FOR i := FIRST (x) + 1 TO LAST (x) DO
      key := x[i];
      j := i - 1;
      WHILE (j >= FIRST (x)) AND CompareCard (key, x[j]) < 0 DO
        x[j+1] := x[j];
        DEC (j);
      END;
      x[j+1] := key;
    END;
  END SortTalon;

PROCEDURE CompareCard (READONLY c1, c2: CardType): [-1 .. 1] =
  (* nocard is high *)
  BEGIN
    IF    c1 = noCard THEN RETURN 1
    ELSIF c2 = noCard THEN RETURN -1
    ELSIF Less(c1, c2) THEN RETURN -1
    ELSE RETURN 1;
    END;
  END CompareCard;

PROCEDURE Less (READONLY card1, card2: CardType): BOOLEAN =
  BEGIN
    RETURN (card1.suit < card2.suit)
        OR (card1.suit = card2.suit AND card1.val < card2.val);
  END Less;

(*------------------------------------------------------------------ main ---*)

VAR
  cutOver      : CARDINAL;
  actualCut    : INTEGER;
  userDepthLim : CARDINAL;
  lim          : CARDINAL;
  verbose      : BOOLEAN;
  veryVerbose  : BOOLEAN        := FALSE;
  verboseGiveUp: BOOLEAN;
  verboseNoWin : BOOLEAN;
  verboseDepth : BOOLEAN;
  layoutPool   : REF HashLayout;
  eqCnt        : CARDINAL;
  hashCnt      : CARDINAL;
  fastCnt      : CARDINAL;
  depthLim     : CARDINAL;
  callback     : Callback;
BEGIN
  layoutPool := NEW(REF HashLayout);
END Solve.
