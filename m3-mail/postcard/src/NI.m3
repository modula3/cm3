(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Postcard - a user interface for mail and news *)
(* UI for accessing the "ni" program *)

(* Last modified on Thu Apr 21 16:21:34 PDT 1994 by birrell   *)

MODULE NI EXPORTS NI;

(* Imports. PLEASE KEEP IN ALPHABETICAL ORDER! *)
IMPORT
  BorderedVBT,
  Closure,
  Filter, FilterClass, Fmt,
    FormsVBT, FVTypes,
  MailUtilities, MiscUtils,
  OSUtils,
  PaintOp, Pixmap, Process,
  Rd, Rect, Rsrc,
  Stdio, Split,
  Text, TextList, TextPort, TextRd,
    TextWr, Thread, Time,
  UnixMail,
  VBT, VBTClass,
  Wr;

<*FATAL Wr.Failure*>
<*FATAL Thread.Alerted*>
<*FATAL Rd.Failure*>
<*FATAL FormsVBT.Error*>
<*FATAL FormsVBT.Unimplemented*>


(* *)
(* Types and constants *)
(* *)

TYPE OpClass = {
      (* syntactic shape of NI expressions *)
      Const,     (* true, false *)
      UnaryText, (* "from ...", "contains ...", and so on *)
      UnaryExpr, (* "not expr" *)
      BinaryExpr (* "expr and expr", "expr or expr" *)
    };

TYPE Op = {
      (* expression constructors *)
      After, And, Before, CC, Discussion, Earliest,
      False, From, FromOrToOrCC, Group, InReplyTo,
      Keyword, Latest, Not, MessageID, PathName, On,
      References, Subject, To, True,
      Id, ToOrCC, Or,
      ThisDay, ThisWeek, ThisMonth, ThisYear, Word,
      OpenParen,
      (* symbols for the text parser *)
      CloseParen,
      EOF
    };

REVEAL T = Public BRANDED OBJECT
    fv: FormsVBT.T;
    cl: Closure.T;
    structured := TRUE;
    filter: UnixMail.NI := NIL;
  OVERRIDES
    init := Init;
    reset := Reset;
    flip := Flip;
    count := Count;
    browse := Browse;
    showConversation := ShowConversation;
  END;

TYPE Expr = REF ExprRec;

TYPE Popup = FormsVBT.T OBJECT (* pop-up menu and anchor for keywords *)
    expr: Expr;
  END;

TYPE ExprRec = RECORD
    t: T;              (* global state containing the expression *)
    pack: VBT.T;       (* the pack split containing the expression *)
    parent: Expr;    (* the expression containing the expression, or NIL *)
    op: Op;
    keyword: VBT.T;    (* pack split child containing the keyword and pop-up *)
    popUp: Popup;    (* sub-form containing pop-up menu and its anchor *)
    nextFree: Expr;  (* free list *)
    opClass: OpClass;
  (*! OpClass.UnaryText: !*)
    strOpen: VBT.T;
    textData: TEXT;  (* text arg when not on-screen *)
    textArg: VBT.T;    (* text arg on-screen; this is child of the pack *)
    textArgTypein: FVTypes.FVTypeIn; (* text arg on-screen  *) (* JRM *)
    strClose: VBT.T;
  (*! OpClass.UnaryExpr: !*)
    unaryArg: Expr;
  (*! OpClass.BinaryExpr: !*)
    openParen: VBT.T;
    leftArg: Expr;
    rightArg: Expr;
    closeParen: VBT.T;
  END;

TYPE NIFilter = Filter.T OBJECT
    expr: Expr;
  OVERRIDES
    mouse := NIPutMouseCode;
    position := NIPutPosition;
  END;

CONST (* some FormsVBT symbol names used in multiple places *)
  Predicate = "Predicate";
  NIWrap = "PredicateWrap";


(* *)
(* Global variables *)
(* *)

VAR

(* Constant after initialization: *)
  NIItemTemplate: TEXT;    (* SX for the pop-up anchor and menu *)
  wrap: VBT.T;             (* the rim around the pack-split *)
  wrapOp: PaintOp.T;       (* the rim's colors *)

(* VBT.mu *)
  highlightedExpr: Expr;   (* if non-NIL, expression that has highlight *)
  mouseTracker: VBT.T;     (* if non-NIL, pop-up anchor that got firstDown *)
  mouseRanOut: BOOLEAN;    (* while mouseTracker#NIL, TRUE if mouse left it *)

(* niLock: protects free list and creator threads *)
  niLock := NEW(Thread.Mutex);
  freeExpr: Expr;          (* free list of Expr's *)
  niCreator: Thread.T;     (* thread creating an NI pop-up *)
  niCreator2: Thread.T;    (* thread creating an NI pop-up *)


(* *)
(* Subroutines *)
(* *)

PROCEDURE RowBounds(pack: Split.T; child: VBT.T;
                    VAR top, bottom: INTEGER) =
    (* Assigns to (top,bottom) the (min,max) of the (top,bottom) edges of the
       siblings of child in pack that are in the same row as child. *)
    (* LL = VBT.mu *)
    VAR this: VBT.T; domain := VBT.Domain(child); r: Rect.T; prev: INTEGER;
    <* FATAL Split.NotAChild *>
  BEGIN
    top := domain.north; bottom := domain.south;
    this := child;
    prev := domain.west;
    LOOP
      this := Split.Pred(pack, this);
      IF this = NIL THEN EXIT (* no more siblings *) END;
      r := VBT.Domain(this);
      IF r.east > prev THEN EXIT (* different row *) END;
      prev := r.east;
      top := MIN(top, r.north);
      bottom := MAX(bottom, r.south);
    END;
    this := child;
    prev := domain.east;
    LOOP
      this := Split.Succ(pack, this);
      IF this = NIL THEN EXIT (* no more siblings *) END;
      r := VBT.Domain(this);
      IF r.west < prev THEN EXIT (* different row *) END;
      prev := r.west;
      top := MIN(top, r.north);
      bottom := MAX(bottom, r.south);
    END;
  END RowBounds;

PROCEDURE HighlightExpr(expr: Expr; show: BOOLEAN) =
    (* Highlight or un-highlight expression *)
    (* LL = VBT.mu *)
    (* First, compute bounds, then outline contained area *)
  VAR
    op: PaintOp.T; (* PaintOp.Fg, or wrap's background, depending on "show" *)
    firstLeft: INTEGER;      (* left edge of first VBT *)
    lastRight: INTEGER;      (* right edge of last VBT *)
    top: INTEGER;            (* MIN(top edges of VBTs in row of first VBT) *)
    topInset: INTEGER;       (* MAX(bottom edges of VBTs in row of first VBT) *)
    bottomInset: INTEGER;    (* MIN(top edges of VBTs in row of last VBT) *)
    bottom: INTEGER;         (* MAX(bottom edges of VBTs in row of last VBT) *)
    left := LAST(INTEGER);   (* MIN(left edges of VBTs in expr) *)
    right := FIRST(INTEGER); (* MAX(right edges of VBTs in expr) *)
    hPos, vPos: INTEGER;     (* current position for "LineTo" *)
  CONST SemiWidth = 1;
  PROCEDURE LineTo(h, v: INTEGER) =
    (* Stroke the path (hPos,vPos) -> (h,v) with a line of width 2 * SemiWidth,
       with rectangular caps *)
    BEGIN
      VBT.PaintTint(wrap,
        Rect.FromEdges(MIN(hPos,h)-SemiWidth,
                       MAX(hPos,h)+SemiWidth,
                       MIN(vPos,v)-SemiWidth,
                       MAX(vPos,v)+SemiWidth), op);
      hPos := h; vPos := v;
    END LineTo;
  PROCEDURE ComputeBounds(v: VBT.T; first, last: BOOLEAN) =
    VAR r := VBT.Domain(v);
    BEGIN
      r := VBT.Domain(v);
      IF Rect.IsEmpty(r) THEN
        IF first THEN firstLeft := left; top := 0; topInset := 0 END;
        IF last THEN
          lastRight := right;
          bottomInset := 100000;
          bottom := 100000;
        END;
      ELSE
        IF first THEN
          firstLeft := r.west;
          RowBounds(expr.pack, v, top, topInset);
        END;
        IF last THEN
          lastRight := r.east;
          RowBounds(expr.pack, v, bottomInset, bottom);
        END;
        left := MIN(left, r.west);
        right := MAX(right, r.east);
      END;
    END ComputeBounds;
  PROCEDURE BoundsOfExpr(sub: Expr; first, last: BOOLEAN) =
    BEGIN
      CASE sub.opClass OF
        | OpClass.Const =>
            ComputeBounds(sub.keyword, first, last);
        | OpClass.UnaryText =>
            ComputeBounds(sub.keyword, first, FALSE);
            ComputeBounds(sub.strOpen, FALSE, FALSE);
            ComputeBounds(sub.textArg, FALSE, FALSE);
            ComputeBounds(sub.strClose, FALSE, last);
        | OpClass.UnaryExpr =>
            ComputeBounds(sub.keyword, first, FALSE);
            BoundsOfExpr(sub.unaryArg, FALSE, last);
        | OpClass.BinaryExpr =>
            ComputeBounds(sub.openParen, first, FALSE);
            BoundsOfExpr(sub.leftArg, FALSE, FALSE);
            ComputeBounds(sub.keyword, FALSE, FALSE);
            BoundsOfExpr(sub.rightArg, FALSE, FALSE);
            ComputeBounds(sub.closeParen, FALSE, last);
        ELSE
      END;
    END BoundsOfExpr;
  BEGIN
    BoundsOfExpr(expr, TRUE, TRUE);
    IF show THEN op := PaintOp.Fg ELSE op := wrapOp END;
    hPos := firstLeft-SemiWidth;
    vPos := top-SemiWidth;
    (* top of first*)        LineTo(right+SemiWidth, vPos);
    (* right *)              LineTo(hPos, bottomInset-SemiWidth);
    (* bottom of others *)   LineTo(lastRight+SemiWidth, vPos);
    (* right edge of last *) LineTo(hPos, bottom+SemiWidth);
    (* bottom of last *)     LineTo(left-SemiWidth, vPos);
    (* left *)               LineTo(hPos, topInset+SemiWidth);
    (* top of others *)      LineTo(firstLeft-SemiWidth, vPos);
    (* left edge of first *) LineTo(hPos, top-SemiWidth);
  END HighlightExpr;

PROCEDURE NIPutMouseCode(self: NIFilter; READONLY cd: VBT.MouseRec) =
    (* For filter around the pop-up anchors *)
    (* LL = VBT.mu *)
  BEGIN
    Filter.T.mouse(self, cd);
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      mouseTracker := self;
    ELSIF mouseTracker = self THEN
      mouseTracker := NIL;
      IF mouseRanOut AND (highlightedExpr # NIL) THEN
        HighlightExpr(highlightedExpr, FALSE);
        highlightedExpr := NIL;
      END;
    END;
  END NIPutMouseCode;

PROCEDURE NIPutPosition(self: NIFilter; READONLY cd: VBT.PositionRec) =
    (* For filter around the pop-up anchors *)
    (* LL = VBT.mu *)
  BEGIN
    Filter.T.position(self, cd);
    IF cd.cp.gone THEN
      IF highlightedExpr = self.expr THEN
        IF mouseTracker # NIL THEN
          mouseRanOut := TRUE;
        ELSE
          HighlightExpr(highlightedExpr, FALSE);
          highlightedExpr := NIL;
        END;
      END;
    ELSE
      IF highlightedExpr # NIL THEN HighlightExpr(highlightedExpr, FALSE) END;
      highlightedExpr := self.expr;
      HighlightExpr(highlightedExpr, TRUE);
      mouseRanOut := FALSE;
    END;
  END NIPutPosition;

PROCEDURE CreateExprHandler(popUp: VBT.T; expr: Expr): VBT.T =
    (* Returns a filter with "popUp" as child, highlighting "expr" while
       the mouse is down *)
    (* LL = VBT.mu *)
  BEGIN
    RETURN NEW(NIFilter, expr := expr).init(popUp)
  END CreateExprHandler;

PROCEDURE FreeExpr(expr: Expr) =
    (* Place non-NIL components on free list *)
    (* LL = VBT.mu *)
  BEGIN
    <* ASSERT expr.popUp # NIL *>
    CASE expr.opClass OF
      | OpClass.Const =>
      | OpClass.UnaryText =>
      | OpClass.UnaryExpr =>
          IF expr.unaryArg # NIL THEN FreeExpr(expr.unaryArg) END;
      | OpClass.BinaryExpr =>
          (* free right-to-left, so that things go on the free list
             in the same order that left-to-right construction would take them
             off it. *)
          IF expr.rightArg # NIL THEN FreeExpr(expr.rightArg) END;
          IF expr.leftArg # NIL THEN FreeExpr(expr.leftArg) END;
      ELSE
    END;
    LOCK niLock DO expr.nextFree := freeExpr; freeExpr := expr END;
  END FreeExpr;

TYPE CreatePopupForkee = Thread.Closure OBJECT
      path: Rsrc.Path;
    OVERRIDES
      apply := CreatePopup
    END;

PROCEDURE CreatePopup(self: CreatePopupForkee): REFANY =
    (* Forked procedure for creating pop-up sub-form *)
    (* LL = 0 *)
    VAR popUp: FormsVBT.T;
  BEGIN
    Thread.Pause(0.250d0); (* Let user interaction finish first *)
    popUp := NEW(Popup).init(NIItemTemplate, TRUE, self.path);
    FormsVBT.AttachProc(popUp, "NIFrom", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NIGroup",DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NISubject", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NIToOrCC", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NIWord", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NIOn", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NIBefore", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NIAfter", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NILastWeek", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NILastMonth", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NILastYear", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NINot", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NIExpAndTrue", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NIExpOrFalse", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NIUnaryArg", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NILeftArg", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NIRightArg", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NIaORb", DoPopupAction, NIL);
    FormsVBT.AttachProc(popUp, "NIaANDb", DoPopupAction, NIL);
    RETURN popUp
  END CreatePopup;

PROCEDURE MakePopup(expr: Expr) =
    (* Create and initialize the pop-up menu sub-form *)
    (* LL = niLock *)
  PROCEDURE JoinCreator(VAR creator: Thread.T): FormsVBT.T =
    VAR newForm: FormsVBT.T;
    BEGIN
      <* ASSERT creator # NIL *>
      newForm := NARROW(Thread.Join(creator), FormsVBT.T);
      creator := Thread.Fork(NEW(CreatePopupForkee, path := expr.t.cl.path));
      RETURN newForm
    END JoinCreator;
  BEGIN
      (* Some creations are op + constant. Use niCreator for op, and use
         niCreator2 for the const; thereby we should rarely wait for
         a form to be created *)
    IF expr.opClass = OpClass.Const THEN
      expr.popUp := JoinCreator(niCreator2);
    ELSE
      expr.popUp := JoinCreator(niCreator);
    END;
    expr.popUp.expr := expr;
  END MakePopup;

PROCEDURE MakeExpr(t: T; op: Op; opKeyword: TEXT;
                     left, right: Expr; forDisplay: BOOLEAN): Expr =
    (* create a raw expression; sub-expressions "left" and "right" *)
    (* For unary op, arg is "right" *)
    (* Iff "forDisplay", use the free list and allocate pop-up *)
    (* No VBT's are created - this is done later, in "replace" *)
    (* .textData and .parent are left NIL *)
    (* Children's parent fields are updated appropriately *)
    (* LL < niLock *)
  VAR opClass: OpClass;
  VAR new, prev: Expr;
  BEGIN
    CASE op OF
      | Op.False, Op.ThisDay, Op.ThisMonth, Op.ThisWeek,
        Op.ThisYear, Op.True =>
          opClass := OpClass.Const;
      | Op.After, Op.Before, Op.CC, Op.From,
        Op.FromOrToOrCC, Op.Group, Op.Id, Op.InReplyTo,
        Op.Keyword, Op.MessageID, Op.On, Op.PathName,
        Op.References, Op.Subject, Op.To, Op.ToOrCC,
        Op.Word =>
          opClass := OpClass.UnaryText;
      | Op.Discussion, Op.Earliest, Op.Latest, Op.Not =>
          opClass := OpClass.UnaryExpr;
      | Op.And, Op.Or =>
          opClass := OpClass.BinaryExpr;
    ELSE
      <* ASSERT FALSE *>
    END;
      (* get one from the free list if possible *)
    LOCK niLock DO
      new := freeExpr;
      prev := NIL;
      LOOP
        IF ( NOT forDisplay) OR (new = NIL) THEN
          new :=  NEW(Expr, t := t, pack := FormsVBT.GetVBT(t.fv, Predicate),
                      keyword := NIL, opClass := opClass);
          CASE opClass OF
            | OpClass.UnaryText =>
                new.strOpen := NIL;
                new.textData := "";
                new.textArg := NIL;
                new.textArgTypein := NIL;
                new.strClose := NIL;
            | OpClass.BinaryExpr =>
                new.openParen := NIL;
                new.closeParen := NIL;
            ELSE
          END;
          EXIT
        END;
        IF new.op = op THEN
          IF prev = NIL THEN
            freeExpr := new.nextFree;
          ELSE
            prev.nextFree := new.nextFree;
          END;
          EXIT
        END;
        prev := new;
        new := new.nextFree;
      END;
      new.op := op;
      IF forDisplay THEN
        IF new.popUp = NIL THEN MakePopup(new) END;
        FormsVBT.PutText(new.popUp, "Keyword", opKeyword);
      END;
    END;
    new.parent := NIL;
    new.nextFree := NIL;
    CASE opClass OF
      | OpClass.Const =>
      | OpClass.UnaryText =>
      | OpClass.UnaryExpr =>
          new.unaryArg := right;
          new.unaryArg.parent := new;
      | OpClass.BinaryExpr =>
          new.leftArg := left;
          new.leftArg.parent := new;
          new.rightArg := right;
          new.rightArg.parent := new;
    END;
    RETURN new
  END MakeExpr;

PROCEDURE RemoveExpr(expr: Expr) =
    (* Remove expression from the screen *)
    (* LL = VBT.mu *)
  <*FATAL Split.NotAChild*>
  BEGIN
    IF highlightedExpr = expr THEN
      HighlightExpr(highlightedExpr, FALSE);
      highlightedExpr := NIL;
    END;
    Split.Delete(expr.pack, expr.keyword);
    CASE expr.opClass OF
      | OpClass.Const =>
      | OpClass.UnaryText =>
          Split.Delete(expr.pack, expr.strOpen);
          expr.textData := TextPort.GetText(expr.textArgTypein);
          Split.Delete(expr.pack, expr.textArg);
          Split.Delete(expr.pack, expr.strClose);
      | OpClass.UnaryExpr => RemoveExpr(expr.unaryArg);
      | OpClass.BinaryExpr =>
          Split.Delete(expr.pack, expr.openParen);
          RemoveExpr(expr.leftArg);
          RemoveExpr(expr.rightArg);
          Split.Delete(expr.pack, expr.closeParen);
      ELSE
    END;
  END RemoveExpr;

VAR lastUniqueSymbol: INTEGER;

PROCEDURE UniqueSymbol(): TEXT =
    (* LL = VBT.mu *)
  BEGIN
    INC(lastUniqueSymbol);
    RETURN "UniqueNI" & Fmt.Int(lastUniqueSymbol)
  END UniqueSymbol;

PROCEDURE InsertExpr(t: T; new: Expr; pred: VBT.T; ticks: INTEGER) =
    (* Place "new" on the screen, after "pred" *)
    (* LL = VBT.mu *)
  VAR curPos: INTEGER;
  VAR tp: TextPort.T;
  PROCEDURE Position(pack, this: VBT.T): INTEGER =
    VAR ch: VBT.T;
    VAR i: INTEGER;
    <*FATAL Split.NotAChild*>
    BEGIN
      ch := NIL;
      i := 0;
      LOOP
        ch := Split.Succ(pack, ch);
        IF (ch = NIL) OR (ch = this) THEN EXIT END;
        INC(i);
      END;
      RETURN i
    END Position;
  PROCEDURE InsertVBT(newCh: VBT.T) =
    <*FATAL Split.NotAChild*>
    BEGIN
      Split.Insert(new.pack, pred, newCh);
      t.cl.SetFonts(newCh);
      pred := newCh;
      INC(curPos);
    END InsertVBT;
  PROCEDURE InsertSx(sx: TEXT): VBT.T =
    <*FATAL Split.NotAChild*>
    BEGIN
      t.cl.SetFonts(FormsVBT.Insert(t.fv, Predicate, sx, curPos));
      INC(curPos);
      pred := Split.Succ(new.pack, pred);
      RETURN pred
    END InsertSx;
  PROCEDURE SetNIColor(mySymbol: TEXT) =
    BEGIN
      FormsVBT.PutColorProperty(t.fv, mySymbol, "BgColor",
          FormsVBT.GetColorProperty(t.fv, NIWrap, "BgColor"));
    END SetNIColor;
  PROCEDURE Insert(expr: Expr) =
    VAR mySymbol: TEXT;
    BEGIN
      CASE expr.opClass OF
        | OpClass.BinaryExpr =>
            IF expr.openParen = NIL THEN
              mySymbol := UniqueSymbol();
              expr.openParen := InsertSx("(Text %" & mySymbol & " \"(\" )");
              SetNIColor(mySymbol);
            ELSE
              InsertVBT(expr.openParen);
            END;
            Insert(expr.leftArg);
        ELSE
      END;
      <* ASSERT expr.popUp # NIL *>
      IF expr.keyword = NIL THEN
        mySymbol := UniqueSymbol();
        expr.keyword := InsertSx("(Generic %" & mySymbol & " )");
        FormsVBT.PutGeneric(t.fv, mySymbol,
                            CreateExprHandler(expr.popUp, expr));
      ELSE
        InsertVBT(expr.keyword);
      END;
      CASE expr.opClass OF
        | OpClass.Const =>
        | OpClass.UnaryText =>
            IF expr.strOpen = NIL THEN
              mySymbol := UniqueSymbol();
              expr.strOpen := InsertSx("(Text  %" & mySymbol & " \"(\" )");
              SetNIColor(mySymbol);
            ELSE
              InsertVBT(expr.strOpen);
            END;
            IF expr.textArg = NIL THEN
              mySymbol := UniqueSymbol();
              expr.textArg := InsertSx(
                "(Shape (Width 150)" &
                   "(TypeIn %" & mySymbol & " (ShadowSize 0)))");
              SetNIColor(mySymbol);
              FormsVBT.AttachProc(t.fv, mySymbol, InvokeNIBrowse, t);
              expr.textArgTypein := FormsVBT.GetVBT(t.fv, mySymbol);
            ELSE
              InsertVBT(expr.textArg);
            END;
            TextPort.SetText(expr.textArgTypein, expr.textData);
            IF expr.strClose = NIL THEN
              mySymbol := UniqueSymbol();
              expr.strClose := InsertSx("(Text %" & mySymbol & " \")\" )");
              SetNIColor(mySymbol);
            ELSE
              InsertVBT(expr.strClose);
            END;
        | OpClass.UnaryExpr => Insert(expr.unaryArg);
        | OpClass.BinaryExpr =>
            Insert(expr.rightArg);
            IF expr.closeParen = NIL THEN
              mySymbol := UniqueSymbol();
              expr.closeParen := InsertSx("(Text %" & mySymbol & " \")\" )");
              SetNIColor(mySymbol);
            ELSE
              InsertVBT(expr.closeParen);
            END;
        ELSE
      END;
      CASE expr.opClass OF (* customize the menu *)
        | OpClass.Const, OpClass.UnaryText =>
            FormsVBT.PutInteger(expr.popUp, "NITSplit1", 0);
        | OpClass.UnaryExpr =>
            FormsVBT.PutInteger(expr.popUp, "NITSplit1", 1);
        | OpClass.BinaryExpr =>
            FormsVBT.PutInteger(expr.popUp, "NITSplit1", 2);
            IF expr.op = Op.And THEN
              FormsVBT.PutInteger(expr.popUp, "NITSplit2", 0);
            ELSE
              FormsVBT.PutInteger(expr.popUp, "NITSplit2", 1);
            END;
      END;
    END Insert;
  BEGIN
    IF pred = NIL THEN
      curPos := 0
    ELSE
      curPos := 1 + Position(new.pack, pred)
    END;
      (**** Install on screen, allocating VBT's as needed *)
    Insert(new);
      (**** Grab keybard focus if appropriate *)
    IF new.opClass = OpClass.UnaryText THEN
      tp := new.textArgTypein;
      IF TextPort.TryFocus(tp, ticks) THEN
        TextPort.Select(tp, ticks, 0, LAST(CARDINAL),
                         TextPort.SelectionType.Primary, TRUE);
      END;
    END;
    IF new.parent = NIL THEN VBT.PutProp(new.pack, new) END;
  END InsertExpr;

PROCEDURE GetExpr(ni: FormsVBT.T): Expr =
    (* LL = VBT.mu *)
  BEGIN
    RETURN
    NARROW(VBT.GetProp(FormsVBT.GetVBT(ni, Predicate), TYPECODE(Expr)),
            Expr);
  END GetExpr;

TYPE PrintDest = {
    NI, (* => use NI keywords *)
    Human (* => use keywords from the pop-up anchor *)
  };

PROCEDURE PrintExpr(expr: Expr; dest: PrintDest): TEXT =
    (* LL = VBT.mu *)
  VAR wr: Wr.T;
  PROCEDURE Print(expr: Expr) =
    VAR t: Time.T;
    VAR fields: OSUtils.CalendarTime;
    BEGIN
      CASE expr.opClass OF
        | OpClass.BinaryExpr =>
            Wr.PutText(wr, "(");
            Print(expr.leftArg);
      ELSE
      END;
      IF dest = PrintDest.NI THEN
        CASE expr.op OF
          | Op.After => Wr.PutText(wr, "after");
          | Op.Before => Wr.PutText(wr, "before");
          | Op.CC => Wr.PutText(wr, "cc");
          | Op.Discussion => Wr.PutText(wr, "discussion(");
          | Op.Earliest => Wr.PutText(wr, "earliest(");
          | Op.From => Wr.PutText(wr, "from");
          | Op.Group => Wr.PutText(wr, "group");
          | Op.InReplyTo => Wr.PutText(wr, "inreplyto");
          | Op.Keyword => Wr.PutText(wr, "keyword");
          | Op.Latest => Wr.PutText(wr, "latest(");
          | Op.MessageID => Wr.PutText(wr, "messageid");
          | Op.PathName => Wr.PutText(wr, "pathname");
          | Op.On => Wr.PutText(wr, "on");
          | Op.References => Wr.PutText(wr, "references");
          | Op.Subject => Wr.PutText(wr, "subject");
          | Op.To => Wr.PutText(wr, "to");
          | Op.Word => Wr.PutText(wr, "word");
          | Op.True => Wr.PutText(wr, "All");
          | Op.False => Wr.PutText(wr, "!All");
          | Op.FromOrToOrCC => Wr.PutText(wr, "From_to_cc");
          | Op.Id => Wr.PutText(wr, "Id");
          | Op.ToOrCC => Wr.PutText(wr, "To_cc");
          | Op.Not => Wr.PutText(wr, "!(");
          | Op.And => Wr.PutText(wr, "&");
          | Op.Or => Wr.PutText(wr, "|");
          | Op.ThisDay, Op.ThisWeek, Op.ThisMonth,
            Op.ThisYear =>
              t := Time.Now();
                (* All days and weeks are the same length ... *)
              CASE expr.op OF
                | Op.ThisDay => t := t - FLOAT(24 * 60 * 60, LONGREAL);
                | Op.ThisWeek => t := t - FLOAT(7 * 24 * 60 * 60, LONGREAL);
                ELSE
              END;
              OSUtils.FromTimeLocal(t, fields);
                (* ... but months and years vary in length *)
              CASE expr.op OF
                | Op.ThisMonth =>
                    IF fields.month = 0 THEN
                      DEC(fields.year);
                      fields.month := 11;
                    ELSE
                      DEC(fields.month);
                    END;
                | Op.ThisYear => DEC(fields.year);
                ELSE
              END;
              t := OSUtils.ToTime(fields);
              Wr.PutText(wr, "after(" & Fmt.Int(fields.day) & " " &
                         Text.Sub(OSUtils.TimeLocalToText(t), 4, 3) &
                         " " & Fmt.Int(fields.year) & ")");
        ELSE
          <* ASSERT FALSE *>
        END;
      ELSE
        <* ASSERT expr.popUp # NIL *>
        Wr.PutText(wr, " " & FormsVBT.GetText(expr.popUp, "Keyword") & " ");
      END;
      CASE expr.opClass OF
        | OpClass.Const =>
        | OpClass.UnaryText =>
            IF expr.textArg = NIL THEN
              Wr.PutText(wr, "(" & expr.textData & ")");
            ELSE
              Wr.PutText(wr, "(" &
                         TextPort.GetText(expr.textArgTypein) & ")");
            END;
        | OpClass.UnaryExpr =>
            Print(expr.unaryArg);
            IF dest = PrintDest.NI THEN Wr.PutText(wr, ")") END;
        | OpClass.BinaryExpr =>
            Print(expr.rightArg);
            Wr.PutText(wr, ")");
      END;
    END Print;
  BEGIN
    wr := TextWr.New();
    Print(expr);
    RETURN TextWr.ToText(wr);
  END PrintExpr;

PROCEDURE ParseExpr(t: T; rd: Rd.T;
                    forDisplay: BOOLEAN): Expr RAISES {Error} =
    (* Parse expression; result is non NIL iff parse succeeded. *)
    (* Iff "forDisplay", allocate pop-up and use expr free list. *)
    (* Should mirror exactly the parsing done by the "ni" program itself *)
    (* LL < niLock *)
  VAR this: Op;
  VAR symText: TEXT;
  VAR parsedExpr: Expr; (* result *)
  PROCEDURE StringAndSymbol(t: TEXT): TEXT =
    BEGIN
      IF this = Op.EOF THEN
        RETURN "Syntax error: " & t & " end of expression"
      ELSE
        RETURN "Syntax error: " & t & " \"" & symText & "\""
      END
    END StringAndSymbol;
  PROCEDURE NonWhiteChar(rd: Rd.T;
                         white: SET OF CHAR := MailUtilities.DefaultWS): CHAR
                         RAISES { Rd.EndOfFile } =
    (* Returns the first non-whitespace character, consuming it from rd *)
    VAR c: CHAR;
    BEGIN
      REPEAT c := Rd.GetChar(rd) UNTIL NOT (c IN white);
      RETURN c
    END NonWhiteChar;
  PROCEDURE TerminatedText(rd: Rd.T; term: SET OF CHAR): TEXT =
    (* Returns a text from the reader, up to but excluding the first char in
       "term".   Leaves the terminating character available in the reader. 
       Terminates without complaint at end of file. *)
    VAR wr: Wr.T; c: CHAR;
        <*FATAL Rd.EndOfFile*>
    BEGIN
      wr := TextWr.New();
      LOOP 
        IF Rd.EOF(rd) THEN EXIT END;
        c := Rd.GetChar(rd);
        IF c IN term THEN Rd.UnGetChar(rd); EXIT END;
        Wr.PutChar(wr, c);
      END;
      RETURN TextWr.ToText(wr);
    END TerminatedText;
  PROCEDURE NextSym() RAISES {Error} =
    VAR c: CHAR;
    CONST EndOfSym = SET OF CHAR { '(', ')', '!', '|', '&', '"', '\''} +
                     MailUtilities.DefaultWS;
    <*FATAL Rd.EndOfFile*>
    BEGIN
      TRY c := NonWhiteChar(rd);
      EXCEPT Rd.EndOfFile => this := Op.EOF; symText := "bug"; RETURN
      END;
      IF c = '(' THEN
        this := Op.OpenParen;
        symText := "(";
      ELSIF c = ')' THEN
        this := Op.CloseParen;
        symText := ")";
      ELSIF c = '!' THEN
        this := Op.Not;
        symText := "!";
      ELSIF c = '|' THEN
        this := Op.Or;
        symText := "|";
        IF  NOT Rd.EOF(rd) THEN
          c := Rd.GetChar(rd);
          IF c = '|' THEN symText := "||";  ELSE Rd.UnGetChar(rd);  END;
        END;
      ELSIF c = '&' THEN
        this := Op.And;
        symText := "&";
        IF  NOT Rd.EOF(rd) THEN
          c := Rd.GetChar(rd);
          IF c = '&' THEN symText := "&&";  ELSE Rd.UnGetChar(rd);  END;
        END;
      ELSE
        Rd.UnGetChar(rd);
        symText := TerminatedText(rd, EndOfSym);
        IF MiscUtils.Equal(symText, "after", TRUE) THEN
          this := Op.After
        ELSIF MiscUtils.Equal(symText, "before", TRUE) THEN
          this := Op.Before
        ELSIF MiscUtils.Equal(symText, "cc", TRUE) THEN
          this := Op.CC
        ELSIF MiscUtils.Equal(symText, "discussion", TRUE) THEN
          this := Op.Discussion
        ELSIF MiscUtils.Equal(symText, "earliest", TRUE) THEN
          this := Op.Earliest
        ELSIF MiscUtils.Equal(symText, "from", TRUE) THEN
          this := Op.From
        ELSIF MiscUtils.Equal(symText, "group", TRUE) THEN
          this := Op.Group
        ELSIF MiscUtils.Equal(symText, "inreplyto", TRUE) THEN
          this := Op.InReplyTo
        ELSIF MiscUtils.Equal(symText, "in_reply_to", TRUE) THEN
          this := Op.InReplyTo
        ELSIF MiscUtils.Equal(symText, "keyword", TRUE) THEN
          this := Op.Keyword
        ELSIF MiscUtils.Equal(symText, "latest", TRUE) THEN
          this := Op.Latest
        ELSIF MiscUtils.Equal(symText, "messageid", TRUE) THEN
          this := Op.MessageID
        ELSIF MiscUtils.Equal(symText, "message_id", TRUE) THEN
          this := Op.MessageID
        ELSIF MiscUtils.Equal(symText, "pathname", TRUE) THEN
          this := Op.PathName
        ELSIF MiscUtils.Equal(symText, "on", TRUE) THEN
          this := Op.On
        ELSIF MiscUtils.Equal(symText, "date", TRUE) THEN
          this := Op.On
        ELSIF MiscUtils.Equal(symText, "references", TRUE) THEN
          this := Op.References
        ELSIF MiscUtils.Equal(symText, "subject", TRUE) THEN
          this := Op.Subject
        ELSIF MiscUtils.Equal(symText, "to", TRUE) THEN
          this := Op.To
        ELSIF MiscUtils.Equal(symText, "word", TRUE) THEN
          this := Op.Word
        ELSIF MiscUtils.Equal(symText, "all", TRUE) THEN
          this := Op.True
        ELSIF MiscUtils.Equal(symText, "true", TRUE) THEN
          this := Op.True
        ELSIF MiscUtils.Equal(symText, "false", TRUE) THEN
          this := Op.False
        ELSIF MiscUtils.Equal(symText, "from_to_cc", TRUE) THEN
          this := Op.FromOrToOrCC
        ELSIF MiscUtils.Equal(symText, "id", TRUE) THEN
          this := Op.Id
        ELSIF MiscUtils.Equal(symText, "to_cc", TRUE) THEN
          this := Op.ToOrCC
        ELSIF MiscUtils.Equal(symText, "not", TRUE) THEN
          this := Op.Not
        ELSIF MiscUtils.Equal(symText, "or", TRUE) THEN
          this := Op.Or
        ELSIF MiscUtils.Equal(symText, "and", TRUE) THEN
          this := Op.And
        ELSIF MiscUtils.Equal(symText, "in_the_last_day", TRUE) THEN
          this := Op.ThisDay
        ELSIF MiscUtils.Equal(symText, "in_the_last_week", TRUE) THEN
          this := Op.ThisWeek
        ELSIF MiscUtils.Equal(symText, "in_the_last_month", TRUE) THEN
          this := Op.ThisMonth
        ELSIF MiscUtils.Equal(symText, "in_the_last_year", TRUE) THEN
          this := Op.ThisYear
        ELSE
          RAISE Error(StringAndSymbol("unknown symbol"))
        END;
      END;
    END NextSym;
  PROCEDURE String(): TEXT RAISES {Error} =
    VAR str: TEXT;
    VAR c: CHAR;
    BEGIN
      TRY
        c := NonWhiteChar(rd);
      EXCEPT
        | Rd.EndOfFile =>
            RAISE Error(StringAndSymbol("expected a string arg after"));
      END;
      CASE c OF
        | '(' => str := TerminatedText(rd, SET OF CHAR{'\n', ')'});
        | '\"' => str := TerminatedText(rd, SET OF CHAR{'\n', '"'});
        | '\'' => str := TerminatedText(rd, SET OF CHAR{'\n', '\''});
        ELSE
          RAISE Error(StringAndSymbol("expected a string arg after"));
      END;
      TRY
        c := Rd.GetChar(rd);
        IF c = '\n' THEN
          RAISE Error("Syntax error: newline isn\'t allowed inside a string arg");
        END;
      EXCEPT Rd.EndOfFile =>
        RAISE Error("Syntax error: unexpected end of expression inside a string arg");
      END;
      NextSym();
      RETURN str
    END String;
  PROCEDURE Unary(): Expr RAISES {Error} =
    VAR kw: TEXT;
    VAR expr: Expr;
    VAR thisCopy: Op;
    BEGIN
      thisCopy := this;
      kw := symText;
      CASE thisCopy OF
        | Op.True, Op.False, Op.ThisDay, Op.ThisWeek,
      Op.ThisMonth, Op.ThisYear =>
            expr := MakeExpr(t, thisCopy, kw,
                               NIL, NIL, forDisplay);
            NextSym();
        | Op.After, Op.Before, Op.CC, Op.From,
      Op.Group, Op.InReplyTo, Op.Keyword,
      Op.MessageID, Op.PathName, Op.On, Op.References,
      Op.Subject, Op.To, Op.Word, Op.FromOrToOrCC,
      Op.Id, Op.ToOrCC =>
            expr := MakeExpr(t, thisCopy, kw, NIL, NIL, forDisplay);
            expr.textData := String();
        | Op.Not, Op.Discussion, Op.Earliest,
      Op.Latest =>
            NextSym();
            expr := MakeExpr(t, thisCopy, kw, NIL, Unary(), forDisplay);
        | Op.OpenParen =>
            NextSym();
            expr := Binary(Op.Or);
            IF this # Op.CloseParen THEN
              IF this = Op.EOF THEN
                RAISE Error("Syntax error: missing \")\"");
              ELSE
                RAISE Error(StringAndSymbol("expected \")\" before"));
              END;
            END;
            NextSym();
        ELSE RAISE Error(StringAndSymbol("unexpected"));
      END;
      RETURN expr
    END Unary;
  PROCEDURE Binary(self: Op): Expr RAISES {Error} =
    VAR left, right: Expr;
    VAR kw: TEXT;
    BEGIN
      IF self = Op.Or THEN
        left := Binary(Op.And)
      ELSE
        left := Unary()
      END;
      IF this = self THEN
        kw := symText;
        NextSym();
        right := Binary(self);
        RETURN MakeExpr(t, self, kw, left, right, forDisplay)
      ELSE
        RETURN left
      END;
    END Binary;
  BEGIN
    NextSym();
    parsedExpr := Binary(Op.Or);
    IF this # Op.EOF THEN
      RAISE Error(StringAndSymbol("unexpected"));
    END;
    RETURN parsedExpr
  END ParseExpr;


(* *)
(* Actions *)
(* *)

PROCEDURE EnsureClosed(t: T) =
  (* LL = t *)
  (* Close down the NI sub-process, firmly *)
  BEGIN
    IF t.filter # NIL THEN
      t.filter.kill();
      t.filter := NIL;
    END;
  END EnsureClosed;

TYPE NICloserForkee = Thread.Closure OBJECT
      t: T;
    OVERRIDES
      apply := NICloser
    END;

PROCEDURE NICloser(self: NICloserForkee): REFANY =
    (* Close down the NI sub-process after a delay *)
    (* LL = 0 *)
  BEGIN
    Thread.Pause(60.0d0);
    LOCK self.t DO EnsureClosed(self.t) END;
    RETURN NIL
  END NICloser;

PROCEDURE EnsureOpen(t: T) RAISES {UnixMail.Error} =
    (* Make sure the ni sub-process is running *)
    (* LL = t *)
  BEGIN
    IF t.filter = NIL THEN
      t.filter := UnixMail.StartNI();
      EVAL Thread.Fork(NEW(NICloserForkee, t := t));
    END;
  END EnsureOpen;

PROCEDURE InvokeNIBrowse(<*UNUSED*> fv: FormsVBT.T;
                         <*UNUSED*> name: TEXT;
                         arg: REFANY;
                         ticks: VBT.TimeStamp) =
    (* CR operation from type-ins *)
    (* LL = VBT.mu *)
  BEGIN
    NARROW(arg, T).cl.invokeNIBrowse(ticks);
  END InvokeNIBrowse;

PROCEDURE DoPopupAction(fv: FormsVBT.T;
                        name: TEXT;
                        <*UNUSED*> arg: REFANY;
                        ticks: VBT.TimeStamp) =
    (* Update operation from pop-up menu in expression keyword *)
    (* "fv" is the keyword sub-form, not the main form *)
    (* LL = VBT.mu *)
  VAR old := NARROW(fv, Popup).expr;
  VAR t := old.t;
  VAR op: Op;
  VAR opKeyword: TEXT;
  VAR toBeFreed, new, left, right, parent: Expr;
  VAR pred: VBT.T;
  BEGIN
      (**** Extract interesting stuff from old expression before deleting it *)
    parent := old.parent;
    <*FATAL Split.NotAChild*> BEGIN
      CASE old.opClass OF
        | OpClass.Const, OpClass.UnaryText,
          OpClass.UnaryExpr =>
           pred := Split.Pred(old.pack, old.keyword);
        | OpClass.BinaryExpr =>
           pred := Split.Pred(old.pack, old.openParen);
      END;
    END;
      (* Remove old expression from the screen *)
    RemoveExpr(old);
      (****   A) events that return old sub-expressions *)
    IF MiscUtils.Equal(name, "NIUnaryArg", TRUE) THEN
      <* ASSERT old.opClass = OpClass.UnaryExpr *>
      new := old.unaryArg;
      old.unaryArg := NIL;
      FreeExpr(old);
    ELSIF MiscUtils.Equal(name, "NILeftArg", TRUE) THEN
      <* ASSERT old.opClass = OpClass.BinaryExpr *>
      new := old.leftArg;
      old.leftArg := NIL;
      FreeExpr(old);
    ELSIF MiscUtils.Equal(name, "NIRightArg", TRUE) THEN
      <* ASSERT old.opClass = OpClass.BinaryExpr *>
      new := old.rightArg;
      old.rightArg := NIL;
      FreeExpr(old);
    ELSE
        (****   B) events that construct new expr's; set "left", "right". *)
      toBeFreed := old;
      IF MiscUtils.Equal(name, "NIFrom", TRUE) THEN
        op := Op.From;
        opKeyword := "from";
      ELSIF MiscUtils.Equal(name, "NIGroup", TRUE) THEN
        op := Op.Group;
        opKeyword := "group";
      ELSIF MiscUtils.Equal(name, "NISubject", TRUE) THEN
        op := Op.Subject;
        opKeyword := "subject";
      ELSIF MiscUtils.Equal(name, "NIToOrCC", TRUE) THEN
        op := Op.ToOrCC;
        opKeyword := "to_cc";
      ELSIF MiscUtils.Equal(name, "NIWord", TRUE) THEN
        op := Op.Word;
        opKeyword := "word";
      ELSIF MiscUtils.Equal(name, "NIOn", TRUE) THEN
        op := Op.On;
        opKeyword := "date";
      ELSIF MiscUtils.Equal(name, "NIBefore", TRUE) THEN
        op := Op.Before;
        opKeyword := "before";
      ELSIF MiscUtils.Equal(name, "NIAfter", TRUE) THEN
        op := Op.After;
        opKeyword := "after";
      ELSIF MiscUtils.Equal(name, "NILastWeek", TRUE) THEN
        op := Op.ThisWeek;
        opKeyword := "in_the_last_week";
      ELSIF MiscUtils.Equal(name, "NILastMonth", TRUE) THEN
        op := Op.ThisMonth;
        opKeyword := "in_the_last_month";
      ELSIF MiscUtils.Equal(name, "NILastYear", TRUE) THEN
        op := Op.ThisYear;
        opKeyword := "in_the_last_year";
      ELSIF MiscUtils.Equal(name, "NINot", TRUE) THEN
        op := Op.Not;
        opKeyword := "not";
        right := old;
        toBeFreed := NIL;
      ELSIF MiscUtils.Equal(name, "NIExpAndTrue", TRUE) THEN
        op := Op.And;
        opKeyword := "and";
        left := old;
        toBeFreed := NIL;
        right := MakeExpr(t, Op.True, "true", NIL, NIL, TRUE);
      ELSIF MiscUtils.Equal(name, "NIExpOrFalse", TRUE) THEN
        op := Op.Or;
        opKeyword := "or";
        left := old;
        toBeFreed := NIL;
        right := MakeExpr(t, Op.False, "false", NIL, NIL, TRUE);
      ELSIF MiscUtils.Equal(name, "NIaANDb", TRUE) THEN
        op := Op.And;
        opKeyword := "and";
        left := old.leftArg;
        old.leftArg := NIL;
        right := old.rightArg;
        old.rightArg := NIL;
      ELSIF MiscUtils.Equal(name, "NIaORb", TRUE) THEN
        op := Op.Or;
        opKeyword := "or";
        left := old.leftArg;
        old.leftArg := NIL;
        right := old.rightArg;
        old.rightArg := NIL;
      END;
      IF toBeFreed # NIL THEN FreeExpr(toBeFreed) END;
      new := MakeExpr(t, op, opKeyword, left, right, TRUE);
    END;
      (**** Update parent link and parent's down link *)
    new.parent := parent;
    IF parent # NIL THEN
      CASE parent.opClass OF
        | OpClass.UnaryExpr =>
            IF parent.unaryArg = old THEN
              parent.unaryArg := new
            ELSE
              <* ASSERT FALSE *>
            END;
        | OpClass.BinaryExpr =>
            IF parent.leftArg = old THEN
              parent.leftArg := new
            ELSIF parent.rightArg = old THEN
              parent.rightArg := new
            ELSE
              <* ASSERT FALSE *>
            END;
        ELSE <* ASSERT FALSE *>
      END;
    END;
      (**** Install new expression *)
    InsertExpr(t, new, pred, ticks);
  END DoPopupAction;

TYPE Action = { Count, Browse, ShowConversation };

PROCEDURE DoAction(t: T; action: Action;
               msgList: TextList.T := NIL;
               folder: TEXT := NIL): CARDINAL RAISES { Error } =
    (* Common implementation of main entry points *)
    (* LL = PostcardMain.actions *)
  VAR exprText, subject: TEXT;
  VAR nMsgs: INTEGER;
  VAR parsedExpr: Expr;
  VAR wr: Wr.T;
  VAR haveSubject: BOOLEAN;
  VAR maxMessages: INTEGER;
  BEGIN
    (* Interface is passive, so we can delay reading expression until now *)
    TRY
      LOCK VBT.mu DO
        maxMessages := FormsVBT.GetInteger(t.fv, "maxNIMessages");
      END;
      CASE action OF
      | Action.Count, Action.Browse =>
          LOCK VBT.mu DO
            IF t.structured THEN
              exprText := PrintExpr(GetExpr(t.fv),
                                      PrintDest.NI);
            ELSE
              exprText := FormsVBT.GetText(t.fv, "PredicateTxt");
              parsedExpr := ParseExpr(t, TextRd.New(exprText), FALSE);
              exprText := PrintExpr(parsedExpr, PrintDest.NI);
            END;
          END;
      | Action.ShowConversation =>
          wr := TextWr.New();
          haveSubject := FALSE;
          Wr.PutText(wr, " (discussion(");
          WHILE msgList # NIL DO
            subject:= UnixMail.MsgSubject(folder, msgList.head);
            IF  NOT Text.Empty(subject) THEN
              IF haveSubject THEN Wr.PutText(wr, " | ");  END;
              haveSubject := TRUE;
              Wr.PutText(wr, "subject(" & subject & ")");
            END;
            msgList := msgList.tail;
          END;
          Wr.PutText(wr, "))");
          IF NOT haveSubject THEN RAISE Error("No subject fields") END;
          exprText := TextWr.ToText(wr);
      ELSE
          <* ASSERT FALSE *>
      END;
    EXCEPT UnixMail.Error(errmsg) =>
      RAISE Error(errmsg);
    END;
    LOCK t DO
      TRY
        EnsureOpen(t);
        Wr.PutText(t.filter.wr, "count " & exprText & "\n");
        Wr.Flush(t.filter.wr);
        nMsgs := t.filter.getCount();
        CASE action OF
        | Action.Count =>
        | Action.Browse, Action.ShowConversation =>
            IF nMsgs = 0 THEN
              RAISE Error("No messages matched.");
            ELSIF nMsgs > maxMessages THEN
              RAISE Error("Matched " & Fmt.Int(nMsgs) &
                          " messages. Refine the pattern or increase the " &
                          "\"Max\" field and try again.");
            ELSE
              Wr.PutText(t.filter.wr, "summary -c " & exprText & "\n");
              Wr.Flush(t.filter.wr);
              t.cl.loadFromNI(t.filter.rd, t.filter.getCount());
            END;
        ELSE
            <* ASSERT FALSE *>
        END;
      EXCEPT
      | Wr.Failure =>
          EnsureClosed(t);
          RAISE Error("Error while sending request to NI process.");
      | UnixMail.Error(errmsg) =>
          EnsureClosed(t);
          RAISE Error(errmsg);
      END;
    END;
    RETURN nMsgs
  END DoAction;


(* *)
(* Public methods *)
(* *)

PROCEDURE Init(t: T; cl: Closure.T; fv: FormsVBT.T): T =
  (* LL = VBT.mu *)
  VAR wrapSize: REAL; wrapTxt: Pixmap.T;
  <*FATAL Rsrc.NotFound*>
  BEGIN
    t.cl := cl;
    t.fv := fv;
    lastUniqueSymbol := 0;
    TRY
      NIItemTemplate := Rsrc.Get("niItem.fv", cl.path);
    EXCEPT Rd.Failure =>
      Wr.PutText(Stdio.stderr, "Can\'t read NI item template\n");
      Process.Exit(3);
    END;
    wrap := FormsVBT.GetVBT(t.fv, NIWrap);
    BorderedVBT.Get(wrap, wrapSize, wrapOp, wrapTxt);
    highlightedExpr := NIL;
    mouseTracker := NIL;
    mouseRanOut := FALSE;
    freeExpr := NIL;
    niCreator := Thread.Fork(NEW(CreatePopupForkee, path := t.cl.path));
    niCreator2 := Thread.Fork(NEW(CreatePopupForkee, path := t.cl.path));
    InsertExpr(t,
                 MakeExpr(t, Op.False, "false", NIL, NIL, TRUE),
                 NIL, 0);
    RETURN t
  END Init;

PROCEDURE Reset(t: T; time: VBT.TimeStamp) =
    (* Initialize predicate to "TRUE" *)
    (* LL = VBT.mu *)
  VAR new: Expr;
  BEGIN
    IF t.structured THEN
      RemoveExpr(GetExpr(t.fv));
      FreeExpr(GetExpr(t.fv));
      new := MakeExpr(t, Op.False, "false", NIL, NIL, TRUE);
      InsertExpr(t, new, NIL, time);
    ELSE
      FormsVBT.PutText(t.fv, "PredicateTxt", "false");
      WITH focus = FormsVBT.GetVBT(t.fv, "PredicateTxt") DO
        IF TextPort.TryFocus(focus, time) THEN
          TextPort.Select(v := focus, time := time, replaceMode := TRUE);
        END;
      END;
    END;
  END Reset;

PROCEDURE Flip(t: T; time: VBT.TimeStamp) RAISES { Error } =
    (* Flip to/from structured editor *)
    (* LL = PostcardMain.actions *)
  VAR textExpr: TEXT;
  VAR parsedExpr: Expr;
  BEGIN
    IF t.structured THEN
      LOCK VBT.mu DO
        FormsVBT.PutText(t.fv, "PredicateTxt",
                         PrintExpr(GetExpr(t.fv),
                                     PrintDest.Human));
        FormsVBT.PutInteger(t.fv, "PredicateTSplit", 1);
        RemoveExpr(GetExpr(t.fv));
        FreeExpr(GetExpr(t.fv));
        FormsVBT.TakeFocus(t.fv, "PredicateTxt", time);
        t.structured := FALSE;
      END;
    ELSE
      LOCK VBT.mu DO
        textExpr := FormsVBT.GetText(t.fv, "PredicateTxt");
      END;
      parsedExpr := ParseExpr(t, TextRd.New(textExpr), TRUE);
      LOCK VBT.mu DO
        InsertExpr(t, parsedExpr, NIL, 0);
        FormsVBT.PutInteger(t.fv, "PredicateTSplit", 0);
        t.structured := TRUE;
      END;
    END;
  END Flip;

PROCEDURE Count(t: T): CARDINAL RAISES { Error } =
    (* LL = PostcardMain.actions *)
  BEGIN
    RETURN DoAction(t, Action.Count);
  END Count;

PROCEDURE Browse(t: T) RAISES { Error } =
    (* LL = PostcardMain.actions *)
  BEGIN
    EVAL DoAction(t, Action.Browse);
  END Browse;

PROCEDURE ShowConversation(t: T; msgList: TextList.T;
                           folder: TEXT) RAISES { Error } =
    (* LL = PostcardMain.actions *)
  BEGIN
    EVAL DoAction(t, Action.ShowConversation, msgList, folder);
  END ShowConversation;

BEGIN
END NI.
