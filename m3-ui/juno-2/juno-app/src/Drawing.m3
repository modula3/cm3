(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Oct 26 13:40:18 PST 1997 by heydon                   *)
(*      modified on Sun Jun 11 17:26:27 PDT 1995 by gnelson                  *)
(*      modified on Sat Aug 22 22:10:45 PDT 1992 by myers                    *)
<* PRAGMA LL *>
<* PRAGMA SPEC *>

MODULE Drawing;

IMPORT CurrCmd, View, JunoError, JunoBuild, Drag, PSImpl, JunoPt;
IMPORT   Editor, ToolBox, JunoConfig;
IMPORT JunoAST, JunoASTUtils, JunoCompileErr;
IMPORT JunoValue, JunoRT;
IMPORT TextEditVBT, KeyTrans;
IMPORT VBT, VBTClass, PaintOp, Point, Region, Rect, Path;
IMPORT   Axis, Filter, Pixmap, DblBufferVBT;
IMPORT KeyboardKey, Latin1Key;
IMPORT Atom, Text, Real, Fmt;
IMPORT RTCollector;

CONST
  DummyXY = JunoPt.T{0.0, 0.0};		 (* dummy point location *)

REVEAL
  View.Drawing = Drag.T BRANDED "View.Drawing" OBJECT
    (* General state *)
    tool: ArgTool := NIL;
    lastTool: Tool := NIL;
    usingTempTool := FALSE;
    freezeTool, dragTool: Tool := NIL; (* CONST after initialization *)
    labelStyle: [0..2] := 2;
    pointCount := 0;
    continuousUnparse := TRUE;
    hasMouseFocus: BOOLEAN;
    prevStackSize: CARDINAL := 0;
    (* "Click" args *)
    goodDownClick: BOOLEAN;
    (* "Text" arg *)
    txt: TEXT;        (* "txt # NIL" <=> user typing text argument *)
    (* Grid state *)
    toolBox: ToolBox.T;
    gridMode := FALSE;
    grid0, grid1: JunoPt.T;		 (* grid origin and unix X vector *)
    grid0nm, grid1nm: JunoAST.QId;
    gridOn, gridOff: Tool;
  OVERRIDES
    init := InitDrawing;
    update := Update;
  END;

(* If "d" is a "Drawing.T", then "d.tool" is the currently selected tool; it
   is "NIL" if no tool is currently selected. The UI supports freezing and
   dragging without having selected those tools directly (by using different
   mouse buttons). In the case that one of these tools is being applied in
   that way, "usingTempTool" is true and "lastTool" stores the tool that was
   in effect before the freeze or drag tool was used; they are FALSE and NIL,
   respectively, otherwise. "freezeTool" and "dragTool" are simply cached
   tools so new ones don't have to be allocated on each use.

   The values for "d.labelStyle" are: 0 for no labels, 1 for crosses, and 2
   for point-name labels.

   The value "d.pointCount" is the number of the next potential point. The
   drawing view maps cardinals into point names. When a new point is created,
   it searches for an available point name starting with the one designated by
   "d.pointCount". Since point names can only be deleted by editing the source
   view, "d.pointCount" need only be reset whenever the drawing view is
   updated.

   The value "d.hasMouseFocus" is true if "d" was delivered a transition of
   type "FirstDown", but not the matching transition of type "LastUp".
   Drawings ignore transitions of type "LastUp" if they do not have the mouse
   focus.

   The field "d.goodDownClick" is relevant when the current argument of
   "d.tool" has type "ArgType.Click". It is true if the most recent
   "FirstDown" mouse transition or down transition of the space key
   successfully selected an argument point in the drawing.

   The field "d.txt" is relevant when the current argument of "d.tool" has
   type "argType.Text". In this case, "d.tool" must be a "ProcTool". The field
   "d.txt" contains the text typed by the user so far (modulo backspaces), but
   not containing a trailing vertical bar character. *)

REVEAL
  ChildWriteOnly = ChildPublic BRANDED "Drawing.ChildWO" OBJECT
    originVal: JunoConfig.Origin
  OVERRIDES
    init := InitWriteOnly;
    getOrigin := ChildGetOrigin;
    setOrigin := ChildSetOrigin;
    <* LL.sup = VBT.mu.SELF *>
    rescreen := RescreenWriteOnly;
    reshape := ReshapeWriteOnly;
  END;
  Child = ChildWriteOnly BRANDED "Drawing.Child" OBJECT
    hasFocus := FALSE
  OVERRIDES
    <* LL.sup = VBT.mu *>
    mouse := Mouse;
    position := Position;
    key := Key;
    misc := Misc;
    <* LL.sup = VBT.mu.SELF *>
    reshape := Reshape;
    repaint := Repaint;
  END;

(* A "Child" is the leaf VBT in which painting for the drawing view is
   performed. A "Child" is installed as a child (in the tree of VBT's) of
   a "Drawing.T" (which is also both a "View.T" and a "Filter.T"). *)

REVEAL
  ArgTool = Drag.ArgTool BRANDED "Drawing.ArgTool" OBJECT OVERRIDES
    setup := SetupNoop;
  END;

  PredTool = ArgTool BRANDED "Drawing.PredTool" OBJECT OVERRIDES
    apply := ApplyPred
  END;

  FuncTool = ArgTool BRANDED "Drawing.FuncTool" OBJECT OVERRIDES
    apply := ApplyFunc
  END;

  ProcTool = ProcToolPublic BRANDED "Drawing.ProcTool" OBJECT
    txt: JunoAST.Text := NIL;
    call: JunoAST.Save
  OVERRIDES
    setup := SetupProc;
    apply := ApplyProc;
    text := TextProc
  END;

  (* "txt # NIL => call # NIL"; "txt" will be the last argument of "call" *)

  SetTool = Tool BRANDED "Drawing.SetTool" OBJECT
    cmd: JunoAST.ProcCall
  OVERRIDES
    setup := SetupNoop;
    apply := ApplySetTool
  END;

PROCEDURE SetupNoop(<*UNUSED*> tl: Tool; <*UNUSED*> d: T;
  <*UNUSED*> time: VBT.TimeStamp) =
  BEGIN END SetupNoop;

VAR (*CONST*)
  Red             := PaintOp.FromRGB(1.0, 0.0, 0.0);
  PathColor       := Red;
  SelectColor     := Red;
  FrozenColor     := PaintOp.FromRGB(0.0, 0.35, 1.0);
  LabelColor      := PaintOp.BgFg;

PROCEDURE InitDrawing(d: T; ch: ChildPublic; root: View.Root): PSImpl.T =
  BEGIN
    EVAL PSImpl.T.init(d, ch, root);
    d.freezeTool := NewFreezeTool();
    d.dragTool := Drag.NewTool();
    RETURN d
  END InitDrawing;

PROCEDURE InitWriteOnly(ch: ChildWriteOnly; origin: JunoConfig.Origin): Child =
  BEGIN
    ch.originVal := origin;
    RETURN ch
  END InitWriteOnly;

PROCEDURE SetLabelStyle(d: T; style: [0..2]) =
  BEGIN
    IF style # d.labelStyle THEN
      d.labelStyle := style;
      d.root.dTrue := FALSE;
      d.update()
    END
  END SetLabelStyle;

PROCEDURE AcquireKBFocus(ch: Child; time: VBT.TimeStamp) =
(* Acquire the keyboard focus if Juno doesn't have it already. *)
  BEGIN
    IF NOT ch.hasFocus THEN
      TRY
    	VBT.Acquire(ch, VBT.KBFocus, time);
    	ch.hasFocus := TRUE
      EXCEPT VBT.Error => (* SKIP *)
      END
    END
  END AcquireKBFocus;

PROCEDURE Misc(ch: Child; READONLY cd: VBT.MiscRec) =
  <* LL.sup = VBT.mu *>
  BEGIN
    IF cd.type = VBT.Lost AND cd.selection = VBT.KBFocus THEN
      ch.hasFocus := FALSE;
      EVAL FinishTextTool(VBT.Parent(ch))
    END
  END Misc;

PROCEDURE DragModeFromModifiers(mods: VBT.Modifiers): Drag.DragMode =
(* Return the drag mode appropriate for the modifier keys "mods". *)
  BEGIN
    IF VBT.Modifier.Shift IN mods THEN
      RETURN Drag.DragMode.Hor
    ELSIF VBT.Modifier.Option IN mods OR VBT.Modifier.Control IN mods THEN
      RETURN Drag.DragMode.Ver
    ELSE
      RETURN Drag.DragMode.Unconstrained
    END
  END DragModeFromModifiers;

PROCEDURE Mouse(ch: Child; READONLY cd: VBT.MouseRec) =
  <* LL.sup = VBT.mu *>
  VAR d: T := VBT.Parent(ch); BEGIN
    (* Ignore "LastUp" events if "d" does not have the mouse focus. *)
    IF NOT d.hasMouseFocus AND cd.clickType = VBT.ClickType.LastUp THEN
      RETURN
    END;
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      AcquireKBFocus(ch, cd.time)
    END;
    (* Don't allow manipulations to drawing unless the AST is up to date 
       and we are not in the process of entering text. *)
    IF NOT d.root.astTrue THEN 
      JunoError.Display(ch, "Oops! You forgot to click Run.");
      RETURN 
    ELSIF d.txt # NIL THEN
      RETURN
    END;
    (* Check for use of "freeze" or "drag" tool shortcut *)
    IF cd.clickType = VBT.ClickType.FirstDown AND d.stackSize = 0 THEN
      CASE cd.whatChanged OF
        VBT.Modifier.MouseM =>
          d.usingTempTool := TRUE; d.lastTool := d.tool; d.tool := d.dragTool
      | VBT.Modifier.MouseR =>
          d.usingTempTool := TRUE; d.lastTool := d.tool; d.tool := d.freezeTool
      ELSE (* SKIP *)
      END
    END;
    (* Don't proceed  unless a tool has been selected; ignore errant
       middle- and right-mouse clicks. *)
    IF d.tool = NIL THEN
      JunoError.Display(ch, "Oops! You need to choose a tool.");
      RETURN
    ELSIF cd.whatChanged # VBT.Modifier.MouseL AND NOT d.usingTempTool THEN
      RETURN
    END;
    (* Process the mouse click based on the type of the current argument for
       the current drawing tool. *)
    CASE d.tool.argType[d.stackSize] OF <* NOWARN *>
      ArgType.Click =>
        CASE cd.clickType OF
          VBT.ClickType.FirstDown =>
	    d.hasMouseFocus := TRUE;
	    VAR arg: Arg; BEGIN
	      IF VBT.Modifier.Shift IN cd.modifiers THEN
		arg := ClickNewPt(d, cd.cp.pt, ch.xform,
                  frozen := VBT.Modifier.Option IN cd.modifiers);
		SourceUntrue(d, View.ModKind.ImplicitConsistent);
	      ELSIF NOT FindArg(d, JunoPt.FromHV(cd.cp.pt, ch.xform), arg) THEN
		d.goodDownClick := FALSE;
		RETURN
	      END;
	      Push(d, arg);
	      PaintStackTop(d, select := TRUE)
	    END;
	    d.goodDownClick := TRUE;
        | VBT.ClickType.LastUp =>
	    d.hasMouseFocus := FALSE;
	    IF d.goodDownClick THEN
	      PaintStackTop(d, select := FALSE);
	      ApplyIfReady(d, cd.time)
	    END
        ELSE
          RETURN
        END
    | ArgType.CreateClick =>
        CASE cd.clickType OF
          VBT.ClickType.FirstDown =>
	    d.hasMouseFocus := TRUE;
	    VAR arg: Arg; BEGIN
              arg := ClickNewPt(d, cd.cp.pt, ch.xform,
                frozen := VBT.Modifier.Option IN cd.modifiers);
	      SourceUntrue(d, View.ModKind.ImplicitConsistent);
	      Push(d, arg)
	    END;
	    PaintStackTop(d, select := TRUE)
        | VBT.ClickType.LastUp =>
	    d.hasMouseFocus := FALSE;
	    PaintStackTop(d, select := FALSE);
	    ApplyIfReady(d, cd.time)
        ELSE
          RETURN
        END
    | ArgType.Drag =>
        CASE cd.clickType OF
          VBT.ClickType.FirstDown =>
	    d.hasMouseFocus := TRUE;
	    WITH arg = d.stack[d.stackSize] DO
	      IF FindArg(d, JunoPt.FromHV(cd.cp.pt, ch.xform), arg) THEN
		VBT.SetCage(ch, VBT.CageFromPosition(cd.cp));
		d.dragging := TRUE;
		d.dragger := cd.cp.pt;
		d.draggee := JunoPt.ToHV(arg.loc, ch.xform);
		d.dragName := Atom.ToText(arg.name.id1);
                d.dragMode := DragModeFromModifiers(cd.modifiers);
		(* d.root.marquee.putArg(d.dragName); *)
		TRY d.tool.pre(d, cd, d.stackSize) EXCEPT Drag.Aborted =>
                  (* perform same actions as mouse up-click *)
                  UpDrag(d, ch, VBT.MouseRec{whatChanged:=VBT.Modifier.MouseL,
                    time := cd.time, cp := cd.cp, modifiers := cd.modifiers,
                    clickType := VBT.ClickType.LastUp, clickCount := 1});
                  (* set cage so "Position" won't be called again this drag *)
                  VBT.SetCage(ch, VBT.EverywhereCage)
                END
	      END
	    END
        | VBT.ClickType.LastUp =>
            (* drag completed successfully *)
	    d.hasMouseFocus := FALSE;
	    IF d.dragging THEN UpDrag(d, ch, cd) END
        ELSE
          RETURN
        END
    END;
    (* undo the "freeze" or "drag" tool shortcut *)
    IF d.usingTempTool AND cd.clickType = VBT.ClickType.LastUp THEN
      d.tool := d.lastTool; d.lastTool := NIL; d.usingTempTool := FALSE
    END;
    Sync(ch)
  END Mouse;

PROCEDURE Position(ch: Child; READONLY cd: VBT.PositionRec) =
  <* LL.sup = VBT.mu *>
  VAR d: T := VBT.Parent(ch); BEGIN
    IF NOT d.dragging THEN
      VBT.SetCage(ch, VBT.EverywhereCage)
    ELSIF cd.cp.gone THEN
      VBT.SetCage(ch, VBT.GoneCage)
    ELSE
      TRY
    	VAR delta := Point.Sub(cd.cp.pt, d.dragger); BEGIN
    	  d.dragger := cd.cp.pt;
    	  d.draggee := d.tool.during(d, delta, d.stackSize);
    	END;
    	VBT.SetCage(ch, VBT.CageFromPosition(cd.cp))
      EXCEPT
    	Drag.Aborted =>
    	  (* perform same actions as mouse up-click *)
    	  UpDrag(d, ch, VBT.MouseRec{whatChanged := VBT.Modifier.MouseL,
    	    time := cd.time, cp := cd.cp, modifiers := cd.modifiers,
    	    clickType := VBT.ClickType.LastUp, clickCount := 1});
    	  (* set cage so "Position" won't be called again this drag *)
    	  VBT.SetCage(ch, VBT.EverywhereCage)
      END
    END
  END Position;

PROCEDURE UpDrag(d: T; ch: Child; READONLY cd: VBT.MouseRec) =
(* Perform the action associated with up-clicking at the end of a drag arg.
   This can happen when the user explicitly up-clicks while dragging, if a
   compilation error occurred at the start of the drag, or if a Juno run-time
   error occurs while dragging. *)
  <* LL.sup = VBT.mu *>
  BEGIN
    d.dragging := FALSE;
    d.stack[d.stackSize].locUp := JunoPt.FromHV(d.draggee, ch.xform);
    d.tool.post(d, cd, d.stackSize);
    ApplyIfReady(d, cd.time);
  END UpDrag;

TYPE ArgSet = SET OF ArgType;

VAR collecting := TRUE;

PROCEDURE Key(ch: Child; READONLY cd: VBT.KeyRec) =
  <* LL.sup = VBT.mu *>
  CONST
    ClickArgs = ArgSet{ArgType.Click, ArgType.CreateClick};
    NoModifiers = VBT.Modifiers{};
  VAR d: T := VBT.Parent(ch);  BEGIN
    IF d.dragging THEN RETURN END;
    IF d.txt # NIL THEN
      (* user is typing a text argument *)
      WITH tool = NARROW(d.tool, ProcTool) DO
      	IF cd.wentDown THEN
      	  IF cd.whatChanged = KeyboardKey.BackSpace OR
      	     cd.whatChanged = KeyboardKey.Delete THEN
      	    IF Text.Length(d.txt) > 0 THEN
      	      d.txt := Text.Sub(d.txt, 0, Text.Length(d.txt) - 1);
      	      tool.text(d, d.txt)
      	    END
      	  ELSIF cd.whatChanged = KeyboardKey.Return AND
      		cd.modifiers = NoModifiers THEN
      	    d.stack[d.stackSize].text := d.txt;
      	    d.txt := NIL;
      	    ApplyIfReady(d, cd.time)
      	  ELSE
      	    VAR ch := KeyTrans.TTY(cd); BEGIN
      	      IF ch # KeyTrans.NullKey THEN
      		d.txt := d.txt & Text.FromChar(ch);
      		tool.text(d, d.txt)
      	      END
      	    END
      	  END;
      	END
      END
    ELSE
      (* user is not typing a text argument *)
      IF cd.whatChanged = Latin1Key.space THEN 
	IF cd.wentDown THEN
	  IF d.tool = NIL OR (d.stackSize >= d.prevStackSize) THEN
	    JunoError.Display(d, "No previous argument to copy");
	    d.goodDownClick := FALSE;
	    RETURN
	  ELSIF d.tool.argType[d.stackSize] IN ClickArgs THEN
	    d.root.marquee.putArg(Atom.ToText(d.stack[d.stackSize].name.id1));
	    PaintStackTop(d, select := TRUE);
	    d.goodDownClick := TRUE
	  END
	ELSIF d.goodDownClick AND d.tool # NIL
            AND d.stackSize < d.prevStackSize
	    AND d.tool.argType[d.stackSize] IN ClickArgs THEN
          PaintStackTop(d, select := FALSE);
          ApplyIfReady(d, cd.time)
	END
      ELSIF cd.wentDown THEN
	IF cd.whatChanged = Latin1Key.g OR cd.whatChanged = Latin1Key.G THEN
          (* do a garbage collection now *)
          RTCollector.Collect()
	ELSIF cd.whatChanged = Latin1Key.o OR cd.whatChanged = Latin1Key.O THEN
          (* toggle whether collection is on or off *)
          IF collecting
            THEN RTCollector.Disable()
            ELSE RTCollector.Enable()
          END;
          collecting := NOT collecting
	ELSIF cd.whatChanged = Latin1Key.u OR cd.whatChanged = Latin1Key.U THEN
	  IF d.stackSize > 0 THEN
	    d.root.marquee.remArg();
	    DEC(d.stackSize)
	  END
	ELSIF cd.whatChanged = Latin1Key.c OR cd.whatChanged = Latin1Key.C THEN
	  d.continuousUnparse := NOT d.continuousUnparse;
	  IF d.continuousUnparse AND NOT d.root.sTrue THEN
	    d.root.source.update()
	  END
	END
      END
    END;
    Sync(ch)
  END Key;

PROCEDURE ClickNewPt(
    d: T;
    READONLY hvPt: Point.T;
    READONLY xform: JunoPt.Transform;
    frozen: BOOLEAN)
  : Arg =
(* Create a new point in "d" at Trestle coordinate "hvPt" and update "d"'s
   source to contain it. The "frozen" parameter determines whether the new
   point is frozen or not. *)
  <* LL.sup = VBT.mu *>
  VAR xyPt := JunoPt.FromHV(hvPt, xform); BEGIN
    RETURN CreatePoint(d, xyPt, frozen)
  END ClickNewPt;

PROCEDURE CreatePoint(d: T; READONLY xyPt: JunoPt.T; frozen: BOOLEAN): Arg =
  <* LL.sup = VBT.mu *>
  VAR res: Arg; atom: Atom.T; BEGIN
    REPEAT
      VAR
        letter := VAL(ORD('a') + d.pointCount MOD 26, CHAR);
        digit := (d.pointCount DIV 26) - 1;
        name := Text.FromChar(letter);
      BEGIN
        IF digit >= 0 THEN name := name & Fmt.Int(digit) END;
        atom := Atom.FromText(name);
        INC(d.pointCount)
      END
    UNTIL CurrCmd.GetVariable(CurrCmd.GetAST(d.root.ccmd), atom) = NIL;
    VAR hint: JunoAST.Expr; BEGIN
      res.name := IdToQId(atom);
      IF GridActive(d) THEN
        VAR x, y: JunoValue.Real; BEGIN
          EVAL JunoPt.RelVal(xyPt.x, xyPt.y, d.grid0.x, d.grid0.y,
            d.grid1.x, d.grid1.y, x, y);
          x := FLOAT(ROUND(x), JunoValue.Real);
          y := FLOAT(ROUND(y), JunoValue.Real);
          hint := NEW(JunoAST.Rel,
            e1 := JunoASTUtils.NewPoint(x, y),
            e2 := NEW(JunoAST.Pair, e1 := d.grid0nm, e2 := d.grid1nm));
          res.loc := EvalRel(x, y, d.grid0, d.grid1);
          CurrCmd.AddVariable(d.root.ccmd, atom, res.loc, hint, frozen := TRUE)
        END
      ELSE
        res.loc := xyPt;
        hint := JunoPt.ToASTPair(xyPt);
        CurrCmd.AddVariable(d.root.ccmd, atom, xyPt, hint, frozen := frozen);
      END;
      res.locUp := DummyXY;
      RETURN res
    END
  END CreatePoint;

PROCEDURE FindArg(d: T; READONLY pt: JunoPt.T; VAR (*OUT*) arg: Arg): BOOLEAN =
  VAR nm := FindPoint(d, pt); BEGIN
    IF nm = NIL THEN RETURN FALSE END;
    VAR px, py: JunoValue.Real; BEGIN
      EVAL CurrCmd.PointLocation(d.root.ccmd, nm, px, py);
      arg := Arg{IdToQId(nm), JunoPt.T{px, py}, DummyXY};
      RETURN TRUE
    END
  END FindArg;

PROCEDURE FindPoint(d: T; READONLY pt0: JunoPt.T): JunoAST.Id =
  <* LL.sup <= VBT.mu *>
  VAR
    best := Real.MaxFinite;
    bestatom: Atom.T := NIL;
  PROCEDURE P(n: Atom.T; READONLY pt1: JunoPt.T) =
    VAR dx := pt1.x - pt0.x; dy := pt1.y - pt0.y; d := dx*dx + dy*dy; BEGIN
      IF d < best THEN best := d; bestatom := n END
    END P;
  BEGIN
    CurrCmd.ForAllPoints(d.root.ccmd, P);
    RETURN bestatom
  END FindPoint;

PROCEDURE Push(m: T; READONLY arg: Arg) =
  BEGIN
    m.stack[m.stackSize] := arg
    (* ; m.root.marquee.putArg(Atom.ToText(arg.name.id1)) *)
  END Push;

<* SPEC ApplyIfReady REQUIRES sup(LL) = VBT.mu *>

PROCEDURE ApplyIfReady(d: T; time: VBT.TimeStamp) =
(* Increment the stack pointer, and apply "d.tool" if there are enough
   arguments. If the most recent argument was not the last one and the current
   argument has type "ArgType.Text", initialize state in "d" for the next
   argument.

   This procedure changes the drawing without doing a "Sync". *)
  BEGIN
    INC(d.stackSize);
    IF d.stackSize = NUMBER(d.tool.argType^) THEN
      d.tool.apply(d, SUBARRAY(d.stack, 0, d.stackSize));
      (* d.root.marquee.newline(); *)
      ClearStack(d, time)
    ELSIF d.tool.argType[d.stackSize] = ArgType.Text THEN
      d.txt := "";
      NARROW(d.tool, ProcTool).text(d, d.txt)
    END;
  END ApplyIfReady;

PROCEDURE ClearStack(d: T; time: VBT.TimeStamp; tl: Tool := NIL) =
(* Clear "d"'s stack, and call the "setup" method of the tool "tl". If "tl" is
   defaulted, then use the current "ArgTool" of "d" if one is selected. *)
  BEGIN
    IF d.stackSize # 0 THEN d.prevStackSize := d.stackSize END;
    d.stackSize := 0;
    (* d.root.marquee.erase(); *)
    IF tl = NIL THEN tl := d.tool END;
    IF tl # NIL THEN
      tl.setup(d, time);
      (* ; d.root.marquee.putName(tl.label) *)
      (* invoke "text" method of 1-arg text tool *)
      TYPECASE tl OF
        ProcTool (pt) =>
       	  IF NUMBER(pt.argType^) = 1 AND pt.argType[0] = ArgType.Text THEN
    	    d.txt := "";      (* indicate we are typing a text arg *)
    	    pt.text(d, d.txt);
    	    AcquireKBFocus(Filter.Child(d), time)
       	  END
      ELSE (*SKIP*)
      END
    END
  END ClearStack;

PROCEDURE RescreenWriteOnly(ch: ChildWriteOnly; READONLY cd: VBT.RescreenRec) =
  <* LL.sup = VBT.mu.ch *>
  CONST MMPerPoint = 25.4(*mm/in*) /  72.0(*pt/in*); BEGIN
    IF cd.st # NIL THEN
      ch.xform.xScale := VBT.MMToPixels(ch, MMPerPoint, Axis.T.Hor);
      ch.xform.yScale := VBT.MMToPixels(ch, MMPerPoint, Axis.T.Ver);
      ch.xform.widthScale := (ch.xform.xScale + ch.xform.yScale) / 2.0
    END
  END RescreenWriteOnly;

PROCEDURE ReshapeWriteOnly(ch: ChildWriteOnly; READONLY cd: VBT.ReshapeRec) =
  <* LL.sup = VBT.mu.ch *>
  BEGIN
    IF cd.new # Rect.Empty THEN
      CASE ch.originVal OF
        JunoConfig.Origin.SW => ch.xform.origin := Rect.SouthWest(cd.new)
      | JunoConfig.Origin.Center => ch.xform.origin := Rect.Middle(cd.new)
      END;
      ch.repaint(Region.Full)
    END
  END ReshapeWriteOnly;

PROCEDURE Reshape(ch: Child; READONLY cd: VBT.ReshapeRec) =
  <* LL.sup = VBT.mu.ch *>
  BEGIN
    IF cd.new # Rect.Empty THEN
      VAR d: T := VBT.Parent(ch); BEGIN
  	d.root.dTrue := FALSE
      END
    END;
    ChildWriteOnly.reshape(ch, cd)
  END Reshape;

PROCEDURE Repaint(ch: Child; <*UNUSED*> READONLY rgn: Region.T) =
  <* LL.sup = VBT.mu.ch *>
  VAR d: T := VBT.Parent(ch); BEGIN
    DisplayError(d, Exec(d));
    Sync(ch)
  END Repaint;

PROCEDURE Exec(d: T): TEXT =
  VAR res: TEXT := NIL; BEGIN
    IF NOT d.root.dTrue THEN
      res := ExecCurrCmd(d, d.root.skipify); Annotations(d);
      IF res = NIL THEN d.root.dTrue := TRUE END
    END;
    RETURN res
  END Exec;

PROCEDURE ExecCurrCmd(d: T; skipify: BOOLEAN): TEXT =
  VAR ch: Child := Filter.Child(d); BEGIN
    (* prepare drawing *)
    PSImpl.Reset(d, inExec := FALSE);
    VBT.PaintTint(ch, Rect.Full, PaintOp.Bg);
    DblBufferVBT.ClearSaved(ch);
    (* run current command *)
    TRY
      IF CurrCmd.Run(d.root.ccmd, skipify) THEN
        SourceUntrue(d, View.ModKind.ImplicitConsistent)
      END
    EXCEPT
      CurrCmd.CompileError (msg) => RETURN msg
    | CurrCmd.RuntimeError (rec) => RETURN rec.errorMsg
    END;
    RETURN NIL
  END ExecCurrCmd;

PROCEDURE Annotations(d: T) =
(* Draw annotations after running the current command *)
  BEGIN
    PaintPath(d);
    PaintGrid(d);
    PaintBBox(d);
    PaintPoints(d);
  END Annotations;

PROCEDURE Sync(v: VBT.T) =
  BEGIN VBT.Sync(v) END Sync;

PROCEDURE PaintPath(d: T) =
  VAR ch: Child := Filter.Child(d); BEGIN
    WITH path = d.ps.path DO
      VBT.Stroke(ch, Rect.Full, path, width := 3, end := VBT.EndStyle.Square,
        join := VBT.JoinStyle.Bevel, op := PaintOp.Bg);
      VBT.Stroke(ch, Rect.Full, path, op := PathColor)
    END
  END PaintPath;

PROCEDURE GridActive(d: T): BOOLEAN =
  BEGIN
    RETURN d.gridMode AND
      1.0 <= ABS(d.grid1.x - d.grid0.x) + ABS(d.grid1.y - d.grid0.y)
  END GridActive;

PROCEDURE AdjustGridPoints(d: T) =
(* Update the values of "d.grid0" and "d.grid1" from the current command. If
   either of the points "d.grid0nm" or "d.grid1nm" is not defined in the
   current command, then reset "d.gridMode". *)
  PROCEDURE UpdatePoint(nm: JunoAST.Id; VAR (*OUT*) pt: JunoPt.T) =
    VAR x, y: JunoValue.Real; BEGIN
      IF CurrCmd.PointLocation(d.root.ccmd, nm, x, y)
        THEN pt := JunoPt.T{x, y}
        ELSE d.gridMode := FALSE
      END
    END UpdatePoint;
  BEGIN
    IF d.gridMode THEN
      UpdatePoint(d.grid0nm.id1, d.grid0);
      UpdatePoint(d.grid1nm.id1, d.grid1);
      (* turn off grid tool if grid is no longer active *)
      IF NOT d.gridMode THEN
        ToolBox.SwapButton(d.toolBox, d.gridOff, d.gridOn, "Grid On")
      END
    END
  END AdjustGridPoints;

PROCEDURE ToGridCoords(d: T; ch: Child; hvPt: Point.T): JunoPt.T =
  VAR res: JunoPt.T; xyPt := JunoPt.FromHV(hvPt, ch.xform); BEGIN
    EVAL JunoPt.RelVal(xyPt.x, xyPt.y, d.grid0.x, d.grid0.y,
      d.grid1.x, d.grid1.y, res.x, res.y);
    RETURN res
  END ToGridCoords;

PROCEDURE PaintGrid(d: T) =
  BEGIN
    AdjustGridPoints(d);
    IF GridActive(d) THEN
      VAR
	ch: Child := Filter.Child(d);
	dom := VBT.Domain(d);
	nw := ToGridCoords(d, ch, Rect.NorthWest(dom));
	ne := ToGridCoords(d, ch, Rect.NorthEast(dom));
	se := ToGridCoords(d, ch, Rect.SouthEast(dom));
	sw := ToGridCoords(d, ch, Rect.SouthWest(dom));
	mini := MIN(MIN(FLOOR(nw.x), FLOOR(ne.x)),
		    MIN(FLOOR(se.x), FLOOR(sw.x)));
	maxi := MAX(MAX(CEILING(nw.x), CEILING(ne.x)),
		    MAX(CEILING(se.x), CEILING(sw.x)));
	minj := MIN(MIN(FLOOR(nw.y), FLOOR(ne.y)),
		    MIN(FLOOR(se.y), FLOOR(sw.y)));
	maxj := MAX(MAX(CEILING(nw.y), CEILING(ne.y)),
		    MAX(CEILING(se.y), CEILING(sw.y)));
      BEGIN
	FOR i := mini TO maxi DO
	  FOR j := minj TO maxj DO
	    VBT.PaintTint(ch, Rect.FromPoint(
	      JunoPt.ToHV(
                EvalRel(
                  FLOAT(i, JunoValue.Real), FLOAT(j, JunoValue.Real),
		  d.grid0, d.grid1),
		ch.xform)),
	      PaintOp.Fg)
	  END
	END
      END
    END
  END PaintGrid;

PROCEDURE PaintBBox(d: T) =
  VAR ch: Child := Filter.Child(d);
  PROCEDURE HVPoint(x, y: JunoValue.Real): Point.T =
    BEGIN RETURN JunoPt.ToHV(JunoPt.T{x, y}, ch.xform) END HVPoint;
  VAR path := NEW(Path.T); BEGIN
    WITH bbox = d.ps.bbox DO
      Path.MoveTo(path, HVPoint(bbox.west, bbox.south));
      Path.LineTo(path, HVPoint(bbox.east, bbox.south));
      Path.LineTo(path, HVPoint(bbox.east, bbox.north));
      Path.LineTo(path, HVPoint(bbox.west, bbox.north));
      Path.Close(path)
    END;
    VBT.Stroke(ch, Rect.Full, path, width := 1, end := VBT.EndStyle.Butt, 
      join := VBT.JoinStyle.Miter, src := Pixmap.Gray)
  END PaintBBox;

PROCEDURE PaintPoints(d: T) =
  VAR
    ccmd := d.root.ccmd;
    ch: Child := Filter.Child(d);
  PROCEDURE P(atom: Atom.T; READONLY pt: JunoPt.T) =
    VAR op: PaintOp.T; BEGIN
      IF CurrCmd.IsFrozen(ccmd, atom)
        THEN op := FrozenColor
        ELSE op := LabelColor
      END;
      PaintPoint(ch, Atom.ToText(atom), op,
        pt := JunoPt.ToHV(pt, ch.xform))
    END P;
  BEGIN
    IF d.labelStyle > 0 THEN
      CurrCmd.ForAllPoints(ccmd, P)
    END
  END PaintPoints;

PROCEDURE PaintStackTop(d: T; select: BOOLEAN) =
(* Paint the label on the top of "d"'s stack. If "select" is true, then paint
   the label in "SelectColor"; otherwise, paint it in "LabelColor" or
   "FrozenColor" based on whether the point is frozen or not. *)
  VAR
    arg := d.stack[d.stackSize];
    ch: Child := Filter.Child(d);
    op: PaintOp.T;
  BEGIN
    IF select THEN
      op := SelectColor
    ELSE
      IF CurrCmd.IsFrozen(d.root.ccmd, arg.name.id1)
        THEN op := FrozenColor
        ELSE op := LabelColor
      END;
    END;
    PaintPoint(ch, Atom.ToText(arg.name.id1), op := op,
      pt := JunoPt.ToHV(arg.loc, ch.xform))
  END PaintStackTop;

PROCEDURE PaintPoint(ch: Child; name: TEXT; op: PaintOp.T; pt: Point.T) =
  VAR m: T := VBT.Parent(ch); BEGIN
    CASE m.labelStyle OF
      0 => (* SKIP *)
    | 1 =>
        VAR delta := Point.Sub(pt, Rect.Middle(JunoConfig.cross.r)); BEGIN
          JunoConfig.crossBdry := Region.Add(JunoConfig.crossBdry, delta);
          JunoConfig.cross := Region.Add(JunoConfig.cross, delta);
          VBT.PaintRegion(ch, JunoConfig.crossBdry, PaintOp.Bg);
          VBT.PaintRegion(ch, JunoConfig.cross, op)
        END
    | 2 =>
        VAR
          delta := Point.Sub(pt, Rect.Middle(JunoConfig.dot.r));
          rect := VBT.BoundingBox(ch, name, JunoConfig.labelFont);
        BEGIN
	  JunoConfig.dot := Region.Add(JunoConfig.dot, delta);
	  VBT.PaintText(ch, t := name, op := op, fnt := JunoConfig.labelFont,
	    pt := Point.Sub(pt, Rect.NorthEast(rect)), clip := Rect.Full);
	  VBT.PaintRegion(ch, JunoConfig.dot, op)
        END
    END
  END PaintPoint;

PROCEDURE Update(d: T) =
  <* LL.sup <= VBT.mu *>
  BEGIN
    d.pointCount := 0;
    d.txt := NIL;
    ClearStack(d, 0);
    DisplayError(d, Exec(d));
    Sync(Filter.Child(d))
  END Update;

PROCEDURE ChildGetOrigin(ch: ChildWriteOnly): JunoConfig.Origin =
  BEGIN RETURN ch.originVal END ChildGetOrigin;

PROCEDURE ChildSetOrigin(ch: ChildWriteOnly; origin: JunoConfig.Origin) =
  BEGIN ch.originVal := origin END ChildSetOrigin;

PROCEDURE FinishTextTool(d: T): BOOLEAN =
  VAR res := d.txt # NIL; BEGIN
    IF res THEN ToolBox.Unselect(d.root) END;
    RETURN res
  END FinishTextTool;

PROCEDURE Make(d: T; skipify: BOOLEAN) =
  BEGIN
    d.pointCount := 0;
    d.txt := NIL;
    ClearStack(d, 0);
    VAR err := ExecCurrCmd(d, skipify); BEGIN
      Annotations(d);
      IF err = NIL
        THEN d.root.dTrue := TRUE
        ELSE DisplayError(d, err)
      END
    END;
    Sync(Filter.Child(d))
  END Make;

PROCEDURE ArrayToList(READONLY arg: ARRAY OF Arg; byVal := FALSE):
    JunoAST.ExprList =
(* Return a list of expressions in "arg", where the "i"th expression in the
   list corresponds to "arg[i]". If "arg[i].text # NIL", then the "i"th
   expression is the text string "arg[i].text". Otherwise, if "byVal", then
   the "i"th expression is the value "arg[i].loc"; if not, then it is the name
   of the point "arg[i].name". *)
  VAR res: JunoAST.ExprList; expr: JunoAST.Expr; BEGIN
    IF byVal
      THEN res := NEW(JunoAST.ExprList, bp := JunoAST.End)
      ELSE res := NEW(JunoAST.QIdList, bp := JunoAST.End)
    END;
    FOR i := LAST(arg) TO FIRST(arg) BY -1 DO
      IF arg[i].text # NIL THEN
        expr := ToASTText(arg[i].text)
      ELSIF byVal THEN
        expr := JunoPt.ToASTPair(arg[i].loc)
      ELSE
        <* ASSERT arg[i].name # NIL *>
        expr := arg[i].name
      END;
      res.head := NEW(JunoAST.ExprLink, expr := expr, next := res.head);
      INC(res.size)
    END;
    RETURN res
  END ArrayToList;

PROCEDURE FindBreak(t: TEXT): CARDINAL =
(* Return the index of the character after the last run of newlines not
   counting a run of newlines that terminates "t", or 0 if "t" contains no
   non-terminal newlines. *) 
  VAR i := Text.Length(t) - 1; BEGIN
    WHILE i >= 0 AND Text.GetChar(t, i) = '\n' DO DEC(i) END;
    RETURN Text.FindCharR(t, '\n', start := i) + 1
  END FindBreak;

PROCEDURE ToASTText(t: TEXT): JunoAST.Expr =
(* Return an expression equivalent to the text "t". If "t" contains newline
   characters, the result will be a concatenation of text literals. *)
  VAR i := FindBreak(t); res: JunoAST.Expr; BEGIN
    res := NEW(JunoAST.Text, val := Text.Sub(t, i), bp := JunoAST.End);
    IF i # 0 THEN
      res := NEW(JunoAST.Concat, bp := JunoAST.End,
        e1 := ToASTText(Text.Sub(t, 0, i)), e2 := res)
    END;
    RETURN res
  END ToASTText;

PROCEDURE MakeConstraint(tl: PredTool; READONLY arg: ARRAY OF Arg):
    JunoAST.Formula =
  BEGIN
    IF tl.name.id0 = JunoAST.NilId THEN
      IF tl.name.id1 = CongToolSym THEN
	RETURN NEW(JunoAST.Cong, bp := JunoAST.End,
	  e1 := MakePair(arg[0].name, arg[1].name),
	  e2 := MakePair(arg[2].name, arg[3].name))
      ELSIF tl.name.id1 = ParaToolSym THEN
	RETURN NEW(JunoAST.Para, bp := JunoAST.End,
	  e1 := MakePair(arg[0].name, arg[1].name),
	  e2 := MakePair(arg[2].name, arg[3].name))
      ELSIF tl.name.id1 = HorToolSym THEN
	RETURN NEW(JunoAST.Hor, bp := JunoAST.End,
          e1 := arg[0].name, e2 := arg[1].name)
      ELSIF tl.name.id1 = VerToolSym THEN
	RETURN NEW(JunoAST.Ver, bp := JunoAST.End,
          e1 := arg[0].name, e2 := arg[1].name)
      END
    END;
    RETURN NEW(JunoAST.Call, inouts := JunoAST.EmptyExprList,
      name := tl.name, ins := ArrayToList(arg), bp := JunoAST.End)
  END MakeConstraint;

PROCEDURE MakeEqConstraint(tl: FuncTool; READONLY arg: ARRAY OF Arg):
    JunoAST.Formula =
  BEGIN
    RETURN NEW(JunoAST.Equals, bp := JunoAST.End, e1 := arg[0].name,
      e2 := NEW(JunoAST.Call, bp := JunoAST.End,
        inouts := JunoAST.EmptyExprList, name := tl.name,
        ins := ArrayToList(SUBARRAY(arg, 1, tl.in_cnt))))
  END MakeEqConstraint;

PROCEDURE MakeCommand(
    tl: ProcTool; 
    READONLY arg: ARRAY OF Arg; 
    byVal := FALSE): JunoAST.ProcCall =
(* Return a procedure call to the procedure "tl.name" with the arguments
   stored in "arg". *)
  BEGIN
    <* ASSERT tl.out_cnt + tl.inout_cnt + tl.in_cnt = NUMBER(arg) *>
    WITH
      outArgs = SUBARRAY(arg, 0, tl.out_cnt),
      inoutArgs = SUBARRAY(arg, tl.out_cnt, tl.inout_cnt),
      inArgs = SUBARRAY(arg, tl.out_cnt + tl.inout_cnt, tl.in_cnt)
    DO
      RETURN NEW(JunoAST.ProcCall, bp := JunoAST.End, name := tl.name,
        outs := ArrayToList(outArgs, byVal := FALSE),
        inouts := ArrayToList(inoutArgs, byVal := FALSE),
        inout_parens := NUMBER(inoutArgs) > 1,
        ins := ArrayToList(inArgs, byVal))
    END
  END MakeCommand;

PROCEDURE MakePair(id1, id2: JunoAST.QId): JunoAST.Pair =
  BEGIN
    RETURN NEW(JunoAST.Pair, e1 := id1, e2 := id2, bp := JunoAST.End)
  END MakePair;

PROCEDURE EvalRel(x, y: JunoValue.Real; p0, p1: JunoPt.T): JunoPt.T =
  VAR res: JunoPt.T; dx := p1.x - p0.x; dy := p1.y - p0.y; BEGIN
    res.x := p0.x + dx * x - dy * y;
    res.y := p0.y + dx * y + dy * x;
    RETURN res
  END EvalRel;

<* INLINE *>
PROCEDURE IdToQId(a: JunoAST.Id): JunoAST.QId =
  BEGIN
    RETURN NEW(JunoAST.QId, bp := JunoAST.End, id0 := JunoAST.NilId, id1 := a)
  END IdToQId;

PROCEDURE SelectTool(d: T; t: Tool; time: VBT.TimeStamp) =
  BEGIN
    (* IF d.tool = NIL THEN PopupMarquee(d) END; *)
    IF d.txt # NIL THEN
      IF Text.Length(d.txt) > 0 THEN
        (* In the middle of entering text; simulate <RETURN> *)
        d.stack[d.stackSize].text := d.txt;
        d.txt := NIL;
        ApplyIfReady(d, time);
        (* NOTE: If the current tool is a text tool taking a single argument,
           its "setup" method will have set "d.txt" to the empty string. *)
      END;
      IF d.txt # NIL THEN
        <* ASSERT Text.Length(d.txt) = 0 *>
        (* erase the vertical bar for the empty text arg *)
        DblBufferVBT.Restore(Filter.Child(d));
        d.txt := NIL
      END;
      Sync(Filter.Child(d))
    END;
    IF t # NIL AND NUMBER(t.argType^) = 0 THEN 
      (* attempt to apply a tool with 0 user-supplied arguments;
         this includes set tools *)
      IF d.root.astTrue THEN (* AST must be up-to-date *)
        ClearStack(d, time, tl := t);
        t.apply(d, ARRAY OF Arg{});
      	ClearStack(d, time);
      	Sync(Filter.Child(d));
(*
      	d.root.marquee.erase();
      	d.root.marquee.putName(t.label);
      	d.root.marquee.newline()
*)
      ELSE
        JunoError.Display(Filter.Child(d), "Oops! You forgot to click Run.");
      END
    ELSE
      (* deselect tool or select tool with > 0 arguments *)
      d.tool := t;
      ClearStack(d, time)
    END
  END SelectTool;

PROCEDURE NewArgArray(n: CARDINAL; type: ArgType): REF ARRAY OF ArgType =
  VAR res := NEW(REF ARRAY OF ArgType, n); BEGIN
    FOR i := 0 TO LAST(res^) DO res[i] := type END;
    RETURN res
  END NewArgArray;

PROCEDURE NewCreateTool(): ArgTool =
  BEGIN
    RETURN NEW(ArgTool, argType := NewArgArray(1, ArgType.CreateClick),
      apply := ApplyPoint)
  END NewCreateTool;

<* SPEC ApplyPoint REQUIRES sup(LL) = VBT.mu *>

PROCEDURE ApplyPoint(
  <*UNUSED*> tl: Tool;
  <*UNUSED*> d: T;
  <*UNUSED*> READONLY arg: ARRAY OF Arg) =
  BEGIN
    (*SKIP*)
  END ApplyPoint;

PROCEDURE NewPredTool(name: JunoAST.QId; in_cnt: CARDINAL): ArgTool =
  BEGIN
    RETURN NEW(PredTool, name := name, in_cnt := in_cnt,
      argType := NewArgArray(in_cnt, ArgType.Click))
  END NewPredTool;

<* SPEC ApplyPred REQUIRES sup(LL) = VBT.mu *>

PROCEDURE ApplyPred(tl: PredTool; d: T; READONLY arg: ARRAY OF Arg) =
(* This is the default implementation of the "apply" method for predicate
   tools. *)
  BEGIN
    <* ASSERT d.root.astTrue *>
    CurrCmd.AddConstraint(d.root.ccmd, MakeConstraint(tl, arg));
    d.root.dTrue := FALSE;		 (* indicate implicit modification *)
    SourceUntrue(d, View.ModKind.ImplicitOutOfDate)
  END ApplyPred;

PROCEDURE NewFuncTool(name: JunoAST.QId; in_cnt: CARDINAL): ArgTool =
  BEGIN
    RETURN NEW(FuncTool, name := name, in_cnt := in_cnt,
      argType := NewArgArray(1 + in_cnt, ArgType.Click))
  END NewFuncTool;

<* SPEC ApplyFunc REQUIRES sup(LL) = VBT.mu *>

PROCEDURE ApplyFunc(tl: FuncTool; d: T; READONLY arg: ARRAY OF Arg) =
(* This is the default implementation of the "apply" method for function
   tools. *)
  BEGIN
    <* ASSERT d.root.astTrue *>
    CurrCmd.AddConstraint(d.root.ccmd, MakeEqConstraint(tl, arg));
    d.root.dTrue := FALSE;		 (* indicate implicit modification *)
    SourceUntrue(d, View.ModKind.ImplicitOutOfDate)
  END ApplyFunc;

PROCEDURE NewProcTool(name: JunoAST.QId; in_cnt: CARDINAL;
  out_cnt, inout_cnt: CARDINAL := 0; isText: BOOLEAN): ArgTool =
  VAR
    num := in_cnt + out_cnt + inout_cnt;
    args := NewArgArray(num, ArgType.Click);
  BEGIN
    IF isText THEN args[num-1] := ArgType.Text END;
    RETURN NEW(ProcTool, name := name, argType := args,
      out_cnt := out_cnt, inout_cnt := inout_cnt, in_cnt := in_cnt)
  END NewProcTool;

PROCEDURE SetupProc(tl: ProcTool; <*UNUSED*> d: T;
    <*UNUSED*> time: VBT.TimeStamp) =
  BEGIN
    (* initialize "ProcTool" fields *)
    tl.txt := NIL; tl.call := NIL;
  END SetupProc;

<* SPEC ApplyProc REQUIRES sup(LL) = VBT.mu *>

PROCEDURE ApplyProc(tl: ProcTool; d: T; READONLY arg: ARRAY OF Arg) =
(* This is the default implementation of the "apply" method for procedure
   tools. *)
  BEGIN
    <* ASSERT d.root.astTrue *>
    IF tl.txt # NIL THEN
      (* This is a procedure whose last argument is a text *)
      tl.txt.val := d.stack[d.stackSize-1].text;
      DblBufferVBT.Restore(Filter.Child(d));
      tl.txt := NIL; tl.call := NIL
    END;
    IncrRunCmd(d, MakeCommand(tl, arg, byVal := TRUE));
    CurrCmd.AddCommand(d.root.ccmd, MakeCommand(tl, arg, byVal := FALSE));
    SourceUntrue(d, View.ModKind.ImplicitConsistent)
  END ApplyProc;

PROCEDURE IncrRunCmd(d: T; cmd: JunoAST.Cmd) =
(* Incrementally compile and run "cmd", then annotate the current path and the
   point labels. Requires that "cmd" compiles without any errors. *)
  <* FATAL JunoCompileErr.Error *>
  VAR
    slot := JunoBuild.Cmd(cmd, CurrCmd.GetScope(d.root.ccmd));
    execRes: JunoRT.ExecRes;
  BEGIN
    execRes := JunoRT.ExecFromSlot(slot);
    IF execRes.trapCode # JunoRT.TrapCode.NormalHalt THEN 
      JunoError.Display(d, JunoRT.TrapMessage(execRes))
    END;
    PaintPath(d);
    PaintPoints(d)
  END IncrRunCmd;

PROCEDURE TextProc(t: ProcTool; d: T; txt: TEXT) =
  <* LL.sup = VBT.mu *>
  VAR txtArg := txt & "|"; ch := Filter.Child(d); BEGIN
    IF t.txt = NIL THEN
      (* save partial arg on stack for subsequent call to "MakeCommand" *)
      d.stack[d.stackSize].text := txtArg;
      WITH args = SUBARRAY(d.stack, 0, d.stackSize+1) DO
        t.call := NEW(JunoAST.Save, bp := JunoAST.End,
          nm := JunoASTUtils.QIdFromText("PS"),
          body := MakeCommand(t, args, byVal := TRUE))
      END;
      t.txt := GetLastArg(t.call.body);
      DblBufferVBT.Save(ch);
    ELSE
      DblBufferVBT.Restore(ch);
      t.txt.val := txtArg   (* destructively change "t.call" *)
    END;
    IncrRunCmd(d, t.call);
    Sync(ch)
  END TextProc;

PROCEDURE GetLastArg(c: JunoAST.ProcCall): JunoAST.Expr =
(* Return the last "IN" argument of "c". Requires that "c" has at least one
   "IN" parameter. *)
  VAR curr := c.ins.head; BEGIN
    WHILE curr.next # NIL DO curr := curr.next END;
    RETURN curr.expr
  END GetLastArg;

PROCEDURE NewRelTool(): ArgTool =
  BEGIN
    RETURN NEW(ArgTool, argType := NewArgArray(3, ArgType.Click),
      apply := ApplyRel)
  END NewRelTool;

<* SPEC ApplyRel REQUIRES sup(LL) = VBT.mu *>

PROCEDURE ApplyRel(<*UNUSED*> tl: Tool; m: T; READONLY arg: ARRAY OF Arg) =
  BEGIN
    CurrCmd.DoRel(m.root.ccmd, arg[0].name.id1,
      arg[1].name.id1, arg[2].name.id1);
    SourceUntrue(m, View.ModKind.ImplicitConsistent)
  END ApplyRel;

PROCEDURE NewRel1Tool(): ArgTool =
  BEGIN
    RETURN NEW(ArgTool, argType := NewArgArray(2, ArgType.Click),
      apply := ApplyRel1)
  END NewRel1Tool;

<* SPEC ApplyRel1 REQUIRES sup(LL) = VBT.mu *>

PROCEDURE ApplyRel1(<*UNUSED*> tl: Tool; m: T; READONLY arg: ARRAY OF Arg) =
  BEGIN
    CurrCmd.DoRel1(m.root.ccmd, arg[0].name.id1,
      arg[1].name.id1);
    SourceUntrue(m, View.ModKind.ImplicitConsistent)
  END ApplyRel1;

PROCEDURE NewFreezeTool(): ArgTool =
  BEGIN
    RETURN NEW(ArgTool, argType := NewArgArray(1, ArgType.Click),
      apply := ApplyFreeze)
  END NewFreezeTool;

<* SPEC ApplyFreeze REQUIRES sup(LL) = VBT.mu *>

PROCEDURE ApplyFreeze(<*UNUSED*> tl: Tool; m: T; READONLY arg: ARRAY OF Arg) =
  BEGIN
    CurrCmd.FreezePoint(m.root.ccmd, arg[0].name.id1);
    SourceUntrue(m, View.ModKind.ImplicitConsistent);
    PaintPoints(m)
  END ApplyFreeze;

PROCEDURE NewAdjustTool(): ArgTool =
  BEGIN
    RETURN NEW(ArgTool, argType := NewArgArray(1, ArgType.Drag),
      apply := ApplyAdjust);
  END NewAdjustTool; 

PROCEDURE RoundToGrid(d: T; p: JunoPt.T): JunoPt.T =
(* If "d"'s grid is not active, return "p". Otherwise, return the grid point
   nearest to "p". *)
  VAR x, y: JunoValue.Real; BEGIN
    IF NOT GridActive(d) THEN RETURN p END;
    EVAL JunoPt.RelVal(p.x, p.y, d.grid0.x, d.grid0.y,
      d.grid1.x, d.grid1.y, x, y);
    x := FLOAT(ROUND(x), JunoValue.Real);
    y := FLOAT(ROUND(y), JunoValue.Real);
    RETURN EvalRel(x, y, d.grid0, d.grid1)
  END RoundToGrid;

<* SPEC ApplyAdjust REQUIRES sup(LL) = VBT.mu *>

PROCEDURE ApplyAdjust(<*UNUSED*> tl: Tool; d: T; READONLY arg: ARRAY OF Arg) =
  VAR newLoc := RoundToGrid(d, arg[0].locUp); BEGIN
    Adjust(d, arg[0].name.id1, newLoc)
  END ApplyAdjust;

PROCEDURE Adjust(d: T; ptName: JunoAST.Id; READONLY ptLoc: JunoPt.T) =
(* Adjusts the point named "ptName" in the drawing "d" to the new location
   "ptLoc". If the point is currently unfrozen, Juno temporarily freezes the
   point and runs the current command. If that does not succeed, the point is
   unfrozen again, and the current command is re-tried.

   This procedure calls "SourceUntrue(d)", and displays any run-time error
   message. It also automatically adjusts the drawing's grid if the grid is
   active and "ptName" is one of the grid control points. *)
  VAR isFrozen := CurrCmd.IsFrozen(d.root.ccmd, ptName); res: TEXT; BEGIN
    IF NOT isFrozen THEN
      (* temporarily freeze the point *)
      CurrCmd.FreezePoint(d.root.ccmd, ptName)
    END;
    CurrCmd.MovePoint(d.root.ccmd, ptName, ptLoc.x, ptLoc.y);
    res := ExecCurrCmd(d, d.root.skipify);
    IF NOT isFrozen THEN
      (* point was originally unfrozen; unfreeze the point *)
      CurrCmd.FreezePoint(d.root.ccmd, ptName);
      (* run again if the command failed with the point frozen *)
      IF res # NIL THEN
        res := ExecCurrCmd(d, d.root.skipify)
      END
    END;
    Annotations(d);
    SourceUntrue(d, View.ModKind.ImplicitConsistent);
    DisplayError(d, res)
  END Adjust;

PROCEDURE NewGridTools(tb: VBT.Split; d: T): ArgTool =
  BEGIN
    d.toolBox := tb;
    d.gridOn := NEW(ArgTool, argType := NewArgArray(2, ArgType.Click),
      apply := ApplyGridOn);
    d.gridOff := NEW(ArgTool, argType := NewArgArray(0, ArgType.Click),
      apply := ApplyGridOff);
    RETURN d.gridOn
  END NewGridTools;

<* SPEC ApplyGridOn REQUIRES sup(LL) = VBT.mu *>

PROCEDURE ApplyGridOn(<*UNUSED*> tl: Tool; d: T; READONLY arg: ARRAY OF Arg) =
  BEGIN
    d.gridMode := TRUE;
    d.grid0 := arg[0].loc; d.grid0nm := arg[0].name;
    d.grid1 := arg[1].loc; d.grid1nm := arg[1].name;
    ToolBox.Unselect(d.root);
    ToolBox.SwapButton(d.toolBox, d.gridOn, d.gridOff, "Grid Off");
    PaintGrid(d);
    PaintBBox(d);
    PaintPoints(d);
    Sync(Filter.Child(d))
  END ApplyGridOn;

<* SPEC ApplyGridOff REQUIRES sup(LL) = VBT.mu *>

PROCEDURE ApplyGridOff(
    <*UNUSED*> tl: Tool;
    d: T;
    <*UNUSED*> READONLY arg: ARRAY OF Arg) =
  BEGIN
    ToolBox.SwapButton(d.toolBox, d.gridOff, d.gridOn, "Grid On");
    d.gridMode := FALSE;
    d.root.dTrue := FALSE; (* force exec to repaint drawing *)
    DisplayError(d, Exec(d));
    Sync(Filter.Child(d))
  END ApplyGridOff;

PROCEDURE NewSetTool(procNm: JunoAST.QId; arg: JunoAST.Expr): SetTool =
  VAR res := NEW(SetTool, argType := NEW(REF ARRAY OF ArgType, 0)); BEGIN
    res.cmd := NEW(JunoAST.ProcCall, bp := JunoAST.End,
      outs := JunoAST.EmptyQIdList, inouts := JunoAST.EmptyQIdList,
      name := procNm, ins := JunoASTUtils.NewExprList(arg));
    RETURN res
  END NewSetTool;

<* SPEC ApplySetTool REQUIRES sup(LL) = VBT.mu *>

PROCEDURE ApplySetTool(tl: SetTool; d: T;
    <*UNUSED*> READONLY arg: ARRAY OF Arg) =
  VAR cmd := Unqualify(tl.cmd, Editor.ModuleName(d.root.editor)); BEGIN
    CurrCmd.AddCommand(d.root.ccmd, cmd);
    IncrRunCmd(d, cmd);
    SourceUntrue(d, View.ModKind.ImplicitConsistent)
  END ApplySetTool;

PROCEDURE Unqualify(call: JunoAST.ProcCall; mod: Atom.T): JunoAST.Cmd =
(* Requires "call" to have one argument. This unqualifies any top-level
   qualified identifier in "call" whose qualifier is "mod". *)
  BEGIN
    RETURN NEW(JunoAST.ProcCall, bp := JunoAST.End,
      outs := call.outs, inouts := call.inouts,
      name := UnqualifyQId(call.name, mod),
      ins := JunoASTUtils.NewExprList(UnqualifyQId(call.ins.head.expr, mod)))
  END Unqualify;

PROCEDURE UnqualifyQId(ex: JunoAST.Expr; mod: Atom.T): JunoAST.Expr =
  BEGIN
    TYPECASE ex OF
      JunoAST.QId (qid) =>
        IF qid.id0 # JunoAST.NilId AND qid.id0 = mod THEN
          RETURN NEW(JunoAST.QId, bp := JunoAST.End,
            id0 := JunoAST.NilId, id1 := qid.id1)
        END
    ELSE (* SKIP *)
    END;
    RETURN ex
  END UnqualifyQId;
  
(*
PROCEDURE PopupMarquee(m: T) =
  BEGIN
    IF VBT.Parent(m.root.marquee) = NIL THEN
      VAR b := BorderedVBT.New(
        BorderedVBT.New(m.root.marquee, size := 3.0, op := PaintOp.Bg)); BEGIN
        VBTClass.Rescreen(b, VBT.ScreenTypeOf(m.root.source));
        VAR
          sh := VBTClass.GetShapes(b);
          se := Rect.SouthEast(VBT.Domain(m.root.source));
        BEGIN
          ZSplit.InsertAt(VBT.Parent(m.root.source), b,
            Point.MoveHV(se,
              -sh[Axis.T.Hor].pref - 10,
              -sh[Axis.T.Ver].pref - 10));
          ZSplit.SetReshapeControl(b, ZSplit.ESChains)
        END
      END
    END
  END PopupMarquee;
*)

PROCEDURE SourceUntrue(d: T; how: View.ModKind) =
  BEGIN
    d.root.source.modified(how);
    IF d.continuousUnparse THEN d.root.source.update() END
  END SourceUntrue;

PROCEDURE DisplayError(d: T; errmsg: TEXT) =
  <* LL.sup = VBT.mu *>
  BEGIN
    IF errmsg # NIL THEN
      VAR ch := Filter.Child(d.root.source); BEGIN
        JunoError.P(NARROW(ch, TextEditVBT.T).tp, errmsg)
      END
    END
  END DisplayError;

BEGIN
  HorToolSym  := Atom.FromText("_HOR");
  VerToolSym  := Atom.FromText("_VER");
  CongToolSym := Atom.FromText("_CONG");
  ParaToolSym := Atom.FromText("_PARA");
END Drawing.
