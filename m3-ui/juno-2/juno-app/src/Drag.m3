(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Oct 26 13:55:27 PST 1997 by heydon                   *)
(*      modified on Tue Feb  7 15:53:46 PDT 1995 by gnelson                  *)
<* PRAGMA LL *>

MODULE Drag;

IMPORT View, Drawing, JunoUIImpl, PSImpl, Source, JunoPt, CurrCmd;
IMPORT   ExternalProc, JunoError;
IMPORT JunoAST, JunoASTUtils, JunoScope, JunoCompile, JunoCompileErr;
IMPORT JunoRT, RTVal, JunoArgs, JunoRTError;
IMPORT VBT, Cursor, Point, PaintOp, Filter, DblBufferVBT;
IMPORT Atom, Thread, Fmt;

(* For debugging: *)
IMPORT Wr, Stdio, Time;

REVEAL
  T = Public BRANDED "Drag.T" OBJECT
    (* The following fields are used by the "DuringDrag", "PostDrag",
       "RunDeltaCmd", and "CallbackProc" procedures. The first two of these
       procedures are invoked by a Trestle thread in response to a mouse
       motion or up-click, and the latter two are invoked by the thread that
       is forked to run the compiled drag command. *)
    firstTime: BOOLEAN;    (* unprotected; only accessed by drag loop thread *)
    mu: MUTEX := NIL;      (* protects the following fields *)
    c: Thread.Condition := NIL;
    lastReady: BOOLEAN;
    last: Point.T;
    deltaReady: BOOLEAN;
    delta: Point.T;	   (* read/written by "DuringDrag" *)
    done: BOOLEAN;         (* set by "PostDrag" *)
  END;

REVEAL
  ArgTool = ArgToolPublic BRANDED "Drag.ArgTool" OBJECT OVERRIDES
    pre := PreNoop;
    post := PostNoop;
    during := DuringNoop;
  END;

VAR (*CONST*)
  DragColor := PaintOp.FromRGB(1.0, 0.0, 0.0);

(* Default Pre, Post, During methods --------------------------------------- *)

PROCEDURE PreNoop(
    <*UNUSED*> tl: ArgTool;
    d: Drawing.T;
    <*UNUSED*> READONLY cd: VBT.MouseRec;
    <*UNUSED*> i: INTEGER) =
  VAR ch: Drawing.Child := Filter.Child(d); BEGIN
    DblBufferVBT.Save(ch);
    Drawing.PaintPoint(ch, d.dragName, DragColor, d.draggee)
  END PreNoop;

PROCEDURE DuringNoop(
    <*UNUSED*> tl: ArgTool;
    d: Drawing.T;
    READONLY delta: Point.T;
    <*UNUSED*> i: INTEGER): Point.T RAISES {} =
  VAR
    ch: Drawing.Child := Filter.Child(d);
    new := Point.Add(d.draggee, ProjectMode(delta, d.dragMode));
  BEGIN
    DblBufferVBT.Restore(ch);
    Drawing.PaintPoint(ch, d.dragName, DragColor, new);
    Drawing.Sync(ch);
    RETURN new
  END DuringNoop;

PROCEDURE ProjectMode(READONLY delta: Point.T; mode: DragMode): Point.T =
(* Return the point "delta" projected to the horizontal or vertical axis
   according to the dragging mode "mode". *)
  BEGIN
    CASE mode OF
      DragMode.Unconstrained => RETURN delta
    | DragMode.Hor => RETURN Point.T{delta.h, 0}
    | DragMode.Ver => RETURN Point.T{0, delta.v}
    END
  END ProjectMode;

PROCEDURE PostNoop(
    <*UNUSED*> tl: ArgTool;
    <*UNUSED*> d: Drawing.T;
    <*UNUSED*> READONLY cd: VBT.MouseRec;
    <*UNUSED*> i: INTEGER) =
  BEGIN END PostNoop;

(* New Dragging Tools ------------------------------------------------------ *)

TYPE
  DraggingTool = Drawing.ArgTool BRANDED "Drag.DraggingTool" OBJECT
    forkee: Thread.T;
    execRes: JunoRT.ExecRes
  OVERRIDES
    pre    := PreDrag;
    during := DuringDrag;
    post   := PostDrag;
    apply  := ApplyDrag
  END;

(* A "DraggingTool" is the tool used to implement continuous solve dragging.
   It's "pre", "during", "post", and "apply" methods work together. If "tl" is
   a "DraggingTool", then "tl.pre" forks off a thread and sets "tl.forkee" to
   this thread. The thread is set running a compiled command (a loop) that
   gets new values for the dragged point by synchronizing with the "during"
   method, and determines when the user has stopped dragging by synchronizing
   with the "post" method. This command is contructed by the "CompileDrag"
   procedure below. When the thread finishes, "tl.execRes" is set to the
   execution result. *)

PROCEDURE NewTool(): Drawing.ArgTool =
  VAR args := Drawing.NewArgArray(1, Drawing.ArgType.Drag); BEGIN
    RETURN NEW(DraggingTool, argType := args)
  END NewTool; 

(* Drag Tool Pre, Post, During, Apply methods ------------------------------ *)

TYPE
  Closure = Thread.Closure BRANDED "Drag.Closure" OBJECT
    d: Drawing.T;
    slot: CARDINAL;
  OVERRIDES
    apply := RunDeltaCmd
  END;

  (* A "Drag.Closure" is the closure used to fork the compiled drag command.
     If "cl: Closure", "cl.apply" executes the bytestream in "cl.slot", using
     the fields of "cl.d" (for example, "cl.d.mu", "cl.d.c", "cl.d.firstTime",
     and "cl.d.done") to synchronize with the drag command. *)

PROCEDURE RunDeltaCmd(cl: Closure): REFANY =
(* The "apply" method of a "Drag.Closure"; i.e., this procedure is called by
   the thread that gets forked to run the compiled drag command. It runs the
   byte-code program in code table slot "cl.slot". Returns a pointer to the
   resulting trapcode. *)
  VAR res := NEW(REF JunoRT.ExecRes); BEGIN
    cl.d.firstTime := TRUE;
    res^ := JunoRT.ExecFromSlot(cl.slot, reset := TRUE);
    <* ASSERT res^.trapCode = JunoRT.TrapCode.Error *>
    (* If the current command failed, abort the dragging prematurely by
       setting "cl.d.done". *)
    IF res.errorCode # JunoRTError.Code.Halt THEN
      LOCK cl.d.mu DO cl.d.done := TRUE END
    END;
    (* signal thread block in "DuringDrag" *)
    Thread.Signal(cl.d.c);
    RETURN res
  END RunDeltaCmd;

VAR
  debug := 0;
  start: Time.T;			 (* start of drag *)
  count: CARDINAL;			 (* number of drag frames *)

VAR (*CONST*)
  WatchCursor: Cursor.T;

PROCEDURE PreDrag(
    tl: DraggingTool;
    d: Drawing.T;
    READONLY cd: VBT.MouseRec;
    i: INTEGER)
    RAISES {Aborted} =
  <* LL.sup = VBT.mu *>
  VAR ch := Filter.Child(d); BEGIN
    DblBufferVBT.ClearSaved(ch);
start := Time.Now();
count := 0;
    IF d.mu = NIL THEN
      d.mu := NEW(MUTEX);
      d.c := NEW(Thread.Condition)
    END;
    LOCK d.mu DO
      d.last := d.draggee;
      d.lastReady := FALSE;
      d.deltaReady := FALSE;
      d.done := FALSE;
    END;
    (* hilight the selected point and display watch cursor *)
    Drawing.PaintPoint(ch, d.dragName, DragColor, d.draggee);
    VBT.SetCursor(ch, WatchCursor);
    Drawing.Sync(ch);
    (* initialize "forkee" in case "CompileDrag" raises an exception *)
    tl.forkee := NIL;
    TRY
      (* Fork off a new thread to run the compiled Juno command. *)
      tl.forkee := Thread.Fork(NEW(Closure, d := d,
        slot := CompileDrag(d, d.stack[i].name, cd.time)));
    FINALLY
      (* reset the cursor *)
      VBT.SetCursor(ch, Cursor.DontCare)
    END
  END PreDrag;

VAR (* CONST *)
  DragLoopAtom := Atom.FromText("Drag Loop");
  NewptAtom    := Atom.FromText("_newpt");
  OldptAtom    := Atom.FromText("_oldpt");

VAR (* CONST *)
  LoopMu := NEW(MUTEX);
  LoopHeader := NEW(JunoAST.ProcHeader, name := DragLoopAtom,
    ins := JunoAST.EmptyIdList, outs := JunoAST.EmptyIdList,
    inouts := JunoAST.EmptyIdList, bp := JunoAST.End);
  LoopDecl := NEW(JunoAST.ProcDecl, header := LoopHeader, bp := JunoAST.End);
  Newpt := NEW(JunoAST.QId, bp := JunoAST.End,
    id0 := JunoAST.NilId, id1 := NewptAtom);
  Oldpt := NEW(JunoAST.QId, bp := JunoAST.End,
    id0 := JunoAST.NilId, id1 := OldptAtom);
  NewGrd := NEW(JunoAST.Differs, bp := JunoAST.End,
    e1 := Newpt, e2 := JunoAST.NilVal);
  Locals := NEW(JunoAST.NearVarList, bp := JunoAST.End, size := 2, head :=
    NEW(JunoAST.NearVarLink, id := NewptAtom, hint := JunoAST.NilExpr, next :=
    NEW(JunoAST.NearVarLink, id := OldptAtom, hint := JunoAST.NilExpr)));
  IfCmd := NEW(JunoAST.If, bp := JunoAST.End);

PROCEDURE CompileDrag(d: Drawing.T; p: JunoAST.QId; ts: VBT.TimeStamp):
    CARDINAL RAISES {Aborted} =
(* Let "p" be the name of the dragged point, and let "r" denote the rest
   of the points in the current command. Let the current command C have the
   form:

|     VAR nv IN P(nv) -> A END

   and let P' be P conjoined with any equality constraints in "nv" (except for
   equality constraints on the dragged point "p"). Then create, compile,
   install, and return the slot for the following "drag" command:

|     IF
|     	VAR nv IN
|     	  VAR _newpt, _oldpt IN
|     	    _newpt := JunoUI._DRAG(p) 
|     	  ; DO _newpt # NIL ->
|     	      _oldpt := p
|     	    ; p := _newpt                             ***
|     	    ; { P'?(r) | P'?(p,r) | p := _oldpt }     ***
|     	    ; IF A FI
|     	    ; _newpt := JunoUI._DRAG(p)
|     	    OD
|     	  ; HALT
|     	  END
|     	END
|     FI

   In the event that "d.dragMode" is "DragMode.Hor" or "DragMode.Ver", we
   have to compile a slightly different command. In that case, the two lines
   with (***) beside them become:

|    DragMode.Hor:                    DragMode.Ver:
|      ; p := (CAR(p), CDR(_newpt))     ; p := (CAR(_newpt), CDR(p))
|      ; { PH'?(p,r) | p := _oldpt }    ; { PV'?(p,r) | p := _oldpt }

   where "PH'" is "P' AND p HOR _newpt" and "PV'" is "P' AND p VER _newpt".

   Note how the drag command is cobbled together from syntactic parts of the
   current command, rather than from semantic parts. In particular, the drag
   command only solves for the variables listed at the outermost level of the
   current command, and it solves these variables only for the outermost
   predicate P. This technique fails to reflect the true semantics of the
   current command, for example, if A has the form:

|     VAR nv' IN Q(nv, nv') -> A' END

   Now, "Grd(C) = P(nv) AND Q(nv, nv') AND Grd(A')", and the solver solves the
   current command for the set of variables "nv \union nv'". But, as previously
   mentioned, the drag command solves only for the variables "nv" such that
   "P(nv)". To rectify this problem, we would have to inductively define the
   "solve variables" of a command and the "non-guard" part of a command. We
   could then compile a drag command that solved the "SolveVars(C)" for
   "Grd(C)", and then executed "NonGuard(C)" in the drag loop. *)
  <* LL.sup = VBT.mu *>
  VAR
    ast := CurrCmd.GetAST(d.root.ccmd);
    locals := CurrCmd.GetVariables(ast);
    con := Conjoin(CurrCmd.GetConstraint(ast), EqConstraints(locals, p.id1));
    cmd := SkipifyBody(ast, d.root.skipify);
    allVars := MoveToFront(locals, p.id1);
    dragCall := NEW(JunoAST.ProcCall, bp := JunoAST.End,
      outs := JunoASTUtils.NewQIdList(Newpt),
      inouts := JunoAST.EmptyExprList,
      name := JunoASTUtils.QIdFromIds(JunoUIImpl.ModSym, JunoUIImpl.DragSym),
      ins := JunoASTUtils.NewExprList(p));
    innerSemi := NEW(JunoAST.Seq, bp := JunoAST.End,
      c1 := JunoASTUtils.NewAssign(Oldpt, p),
      c2 := NEW(JunoAST.Seq, bp := JunoAST.End,
        c1 := HintP(p, d.dragMode),
        c2 := NEW(JunoAST.Seq, bp := JunoAST.End,
          c1 := SolveCmd(p, con, allVars, d.dragMode),
          c2 := NEW(JunoAST.Seq, bp := JunoAST.End,
            c1 := cmd, c2 := dragCall))));
    do := NEW(JunoAST.Do, bp := JunoAST.End,
      body := NEW(JunoAST.Guard, bp := JunoAST.End,
      grd := NewGrd, body := innerSemi));
    outerSemi := NEW(JunoAST.Seq, bp := JunoAST.End, c1 := dragCall,
      c2 := NEW(JunoAST.Seq, bp := JunoAST.End, c1 := do,
        c2 := JunoAST.HaltVal));
    innerProj := NEW(JunoAST.Proj, bp := JunoAST.End,
      vars := Locals, body := outerSemi);
    outerProj := NEW(JunoAST.Proj, bp := JunoAST.End,
      vars := locals, body := innerProj);
    outerIf := NEW(JunoAST.If, bp := JunoAST.End, body := outerProj);
    proc: JunoScope.Proc;
  BEGIN
    LOCK LoopMu DO
      LoopDecl.body := outerIf;
      TRY
        proc := JunoScope.NewProc(LoopDecl, mod := NIL);
        EVAL JunoCompile.ProcDecl(DragLoopAtom, proc,
          CurrCmd.GetScope(d.root.ccmd))
      EXCEPT
        JunoCompileErr.Error (err) =>
          Source.ShowError(d.root.source, ast, err, ts);
          RAISE Aborted
      END
    END;
    RETURN proc.index
  END CompileDrag;

PROCEDURE MoveToFront(l: JunoAST.NearVarList; id: JunoAST.Id):
    JunoAST.NearVarList =
(* Return a list like "l" such that all occurrences of variables named "id"
   appear first in the result. The order of the other variables in the result
   is undefined. Each variable in the result has its "evar" bit reset, is
   unhinted, and has its "frozen" bit set iff the corresponding variable in
   "l" was frozen or hinted. *)
  VAR
    res := NEW(JunoAST.NearVarList, bp := l, size := l.size);
    curr := l.head; named: JunoAST.NearVarLink := NIL;
  BEGIN
    WHILE curr # NIL DO
      <* ASSERT NOT curr.frozen OR curr.hint # JunoAST.NilExpr *>
      IF curr.id = id THEN
        named := NEW(JunoAST.NearVarLink, id := curr.id, evar := FALSE,
          hint := JunoAST.NilExpr, frozen := (curr.hint # JunoAST.NilExpr),
          index := curr.index, next := named)
      ELSE
        res.head := NEW(JunoAST.NearVarLink, id := curr.id, evar := FALSE,
          hint := JunoAST.NilExpr, frozen := (curr.hint # JunoAST.NilExpr),
          index := curr.index, next := res.head)
      END;
      curr := curr.next
    END;
    IF named # NIL THEN
      (* ``res.head := Append(named, res.head)'' *)
      curr := named;
      WHILE curr.next # NIL DO curr := curr.next END;
      curr.next := res.head;
      res.head := named
    END;
    RETURN res
  END MoveToFront;

PROCEDURE EqConstraints(l: JunoAST.NearVarList; id: JunoAST.Id):
    JunoAST.Formula =
(* Return a conjunction containing a conjunct for each frozen variable in "l"
   other than "id". Return NIL if there are no variables in "l" other than
   "id". *)
  VAR res: JunoAST.Formula := NIL; curr := l.head; eq: JunoAST.Equals; BEGIN
    WHILE curr # NIL DO
      <* ASSERT NOT curr.frozen OR curr.hint # JunoAST.NilExpr *>
      IF curr.id # id AND curr.frozen THEN
        eq := NEW(JunoAST.Equals, bp := JunoAST.End, near := FALSE,
          e1 := JunoASTUtils.QIdFromNearVar(curr), e2 := curr.hint);
        res := Conjoin(eq, res);
      END;
      curr := curr.next
    END;
    RETURN res
  END EqConstraints;

PROCEDURE Conjoin(f1, f2: JunoAST.Formula): JunoAST.Formula =
(* If "f2 # NIL", return the AST "f1 AND f2"; otherwise, return "f1" *)
  BEGIN
    IF f2 = NIL THEN RETURN f1 ELSE
      RETURN NEW(JunoAST.And, bp := JunoAST.End, f1 := f1, f2 := f2)
    END
  END Conjoin;

PROCEDURE SkipNamed(l: JunoAST.NearVarList; id: JunoAST.Id):
    JunoAST.NearVarList =
(* Return a list whose head points to the element just after the longest
   prefix of "l" containing links named "id". *)
  VAR
    res := NEW(JunoAST.NearVarList, bp := l, size := l.size);
    curr := l.head;
  BEGIN
    WHILE curr # NIL AND curr.id = id DO
      DEC(res.size); curr := curr.next
    END;
    res.head := curr;
    RETURN res
  END SkipNamed;

PROCEDURE SkipifyBody(ast: JunoAST.Cmd; skipify: BOOLEAN): JunoAST.Cmd =
(* If "skipify", return "SKIP". Otherwise, return "IF ast FI". *)
  BEGIN
    IF skipify
      THEN RETURN JunoAST.SkipVal
      ELSE IfCmd.body := CurrCmd.GetCmd(ast); RETURN IfCmd
    END
  END SkipifyBody;

PROCEDURE HintP(p: JunoAST.QId; mode: DragMode): JunoAST.Cmd =
(* Return the assignment statement appropriate for the drag mode "mode":
|    DragMode.Unconstrained: "p := _newpt"
|    DragMode.Hor: "p := (CAR(p), CDR(_newpt))
|    DragMode.Ver: "p := (CAR(_newpt), CDR(p))
*)
  PROCEDURE CarCdrPair(e1, e2: JunoAST.Expr): JunoAST.Pair =
  (* Return the expression "(CAR(e1), CDR(e2))". *)
    BEGIN
      RETURN NEW(JunoAST.Pair, bp := JunoAST.End,
        e1 := NEW(JunoAST.Car, bp := JunoAST.End, e := e1),
        e2 := NEW(JunoAST.Cdr, bp := JunoAST.End, e := e2))
    END CarCdrPair;
  BEGIN
    CASE mode OF
      DragMode.Unconstrained => RETURN JunoASTUtils.NewAssign(p, Newpt)
    | DragMode.Hor => RETURN JunoASTUtils.NewAssign(p, CarCdrPair(p, Newpt))
    | DragMode.Ver => RETURN JunoASTUtils.NewAssign(p, CarCdrPair(Newpt, p))
    END
  END HintP;

PROCEDURE SolveCmd(p: JunoAST.QId; con: JunoAST.Formula;
    vars: JunoAST.NearVarList; mode: DragMode): JunoAST.Cmd =
(* Return the solve command for the point "p" given dragging mode "mode".
   "con" is the constraint to solve, and "vars" is the list of unknowns (with
   "p" moved to the front for easy processing). The different cases for "mode"
   are:

|    DragMode.Unconstrained: { P'?(r) | P'?(p,r) | p := _oldpt }
|    DragMode.Hor: { ((p HOR _newpt) AND P')?(p,r) | p := _oldpt }
|    DragMode.Ver: { ((p VER _newpt) AND P')?(p,r) | p := _oldpt }

   where P' is the set of constraints "con" and "r" is the set of variables
   "vars" minus the variable "p". *)
  VAR res: JunoAST.Cmd; BEGIN
    CASE mode OF
      DragMode.Hor =>
        con := Conjoin(NEW(JunoAST.Hor, e1 := p, e2 := Newpt), con)
    | DragMode.Ver =>
        con := Conjoin(NEW(JunoAST.Ver, e1 := p, e2 := Newpt), con)
    | DragMode.Unconstrained => (* SKIP *)
    END;
    res := NEW(JunoAST.Else, bp := JunoAST.End,
      c1 := NEW(JunoAST.Query, bp := JunoAST.End, f := con, vars := vars),
      c2 := JunoASTUtils.NewAssign(p, Oldpt));
    IF mode = DragMode.Unconstrained THEN
      res := NEW(JunoAST.Else, bp := JunoAST.End, c2 := res,
        c1 := NEW(JunoAST.Query, bp := JunoAST.End,
          f := con, vars := SkipNamed(vars, p.id1)))
    END;
    RETURN res
  END SolveCmd;

PROCEDURE CallbackProc(cl: ExternalProc.Closure): BOOLEAN =
(* This procedure is the implementation of the Juno procedure named by the
   global variable "JunoUIImpl.DragSym", which is called inside the drag
   loop compiled by "CompileDrag". Hence, it is called by the thread running
   the compiled Juno program, namely, the one forked in "PreDrag".

   It is called with the current value of the point being dragged (as a Juno
   value representing a point in Juno coordinates). It sets "d.last" to the
   new point value (converted to Trestle coordinates), and signals the
   "DuringDrag" procedure that this value is ready.

   It then synchronizes with the "DuringDrag" procedure to get the delta
   for the difference between the mouse and the dragged point, adds this delta
   to the current value of the point, and returns (on the Juno stack) this
   value as the result. However, if "d.done", then this procedure instead
   returns Juno "NIL". *)
  <* LL.sup < VBT.mu *>
  VAR
    d := cl.rt.drawing;
    ch: Drawing.Child := Filter.Child(d);
    new: Point.T;
    res: RTVal.T;
  BEGIN
    <* FATAL JunoPt.BadPt *>
    VAR err := FALSE; pr: RTVal.Pair; BEGIN
      (* convert argument from Juno coords -> Trestle coords *)
      pr := JunoArgs.ReadPair(1, err);
      <* ASSERT NOT err *>
      new := JunoPt.ToHV(JunoPt.FromValuePair(pr), ch.xform)
    END;

    IF d.firstTime THEN
      d.firstTime := FALSE;
    ELSE
      (* add final annotations to drawing *)
      Drawing.PaintPath(d);
      Drawing.PaintPoint(ch, d.dragName, DragColor, new);
      Drawing.Sync(ch);
      (* update shared data *)
      LOCK d.mu DO
        d.last := new;
        d.lastReady := TRUE;
        d.deltaReady := FALSE;
      END;
      (* signal "DuringDrag" thread *)
      Thread.Signal(d.c)
    END;
    LOCK d.mu DO
      (* wait for next call to "DuringDrag" (i.e., next mouse motion)
         or "PostDrag" (i.e., next up-click terminating drag) *)
      WHILE NOT d.deltaReady DO
        Thread.Wait(d.mu, d.c)
      END;
      IF d.done THEN res := RTVal.nil ELSE
        res := JunoPt.ToValuePair(JunoPt.FromHV(
          Point.Add(d.last, d.delta), ch.xform))
      END
    END;
    JunoArgs.WriteValue(2, res);
    (* if not done, restart a new drawing *)
    IF res # RTVal.nil THEN
      PSImpl.Reset(d);
      DblBufferVBT.Restore(ch);
    END;
    RETURN TRUE
  END CallbackProc;

PROCEDURE DuringDrag(
    tl: DraggingTool;
    d: Drawing.T;
    <*UNUSED*> READONLY delta: Point.T;
    <*UNUSED*> i: INTEGER):
    Point.T RAISES {Aborted} =
(* This is the procedure implementing the "during" method of the built-in
   "Drag" tool. This procedure is invoked by the "Drawing.Position" procedure
   each time the mouse moves.

   This procedure ignores its "delta" argument, and instead computes "d.delta"
   as the difference between the current mouse location "d.dragger" and the
   current value of the point being dragged "d.draggee". In this way, we solve
   the system for the case where the dragged point is initially at the mouse
   cursor.

   If the thread running the compiled Juno command has set "d.done", then that
   command hit a run-time error, so this procedure raises "Aborted" to abort
   the drag prematurely. *)
  <* LL.sup = VBT.mu *>
  BEGIN
INC(count);
    IF tl.forkee = NIL THEN
      (* The drag was aborted during "pre" method due to compilation error,
         so just return the current location of the dragged point. *)
      RETURN d.draggee
    ELSE
      LOCK d.mu DO
    	d.delta := Point.Sub(d.dragger, d.draggee);
    	d.deltaReady := TRUE;
    	d.lastReady := FALSE
      END;
      (* signal "CallbackProc" thread *)
      Thread.Signal(d.c);
      LOCK d.mu DO
        (* wait for current frame to finish painting to get new value *)
    	WHILE NOT d.lastReady AND NOT d.done DO
    	  Thread.Wait(d.mu, d.c)
    	END;
    	IF d.done THEN RAISE Aborted END;
    	RETURN d.last
      END
    END
  END DuringDrag;

PROCEDURE PostDrag(
    tl: DraggingTool;
    d: Drawing.T;
    <*UNUSED*> READONLY cd: VBT.MouseRec;
    <*UNUSED*> i: INTEGER) =
  <* LL.sup = VBT.mu *>
  BEGIN
IF debug > 0 THEN
  <* FATAL Thread.Alerted, Wr.Failure *>
  VAR
    delta := Time.Now() - start;
    usecs := ROUND(1.0d6 * delta) DIV count;
  BEGIN
    Wr.PutText(Stdio.stderr, "Average time = "
      & Fmt.Pad(Fmt.Int(usecs), 8) & " usecs\n");
  END
END;
    IF tl.forkee # NIL THEN
      LOCK d.mu DO
        d.delta := Point.Origin;
        d.deltaReady := TRUE;
        d.lastReady := FALSE
      END;
      (* signal "CallbackProc" thread *)
      Thread.Signal(d.c);
      LOCK d.mu DO
        (* wait for current frame to finish painting *)
    	WHILE NOT d.lastReady AND NOT d.done DO
    	  Thread.Wait(d.mu, d.c)
    	END;
      END;
      (* now finish dragging *)
      LOCK d.mu DO
        d.done := TRUE;
        d.deltaReady := TRUE
      END;
      (* signal thread blocked in "CallbackProc" *)
      Thread.Signal(d.c);
      (* Wait for the thread running the compiled Juno command to terminate *)
      tl.execRes := NARROW(Thread.Join(tl.forkee), REF JunoRT.ExecRes)^
    END
  END PostDrag;

PROCEDURE ApplyDrag(
    tl: DraggingTool;
    d: Drawing.T;
    <*UNUSED*> READONLY arg: ARRAY OF Drawing.Arg) =
  VAR hintsChanged: BOOLEAN; BEGIN
    IF tl.forkee # NIL THEN
      <* ASSERT tl.execRes.trapCode = JunoRT.TrapCode.Error *>
      hintsChanged := CurrCmd.UpdateHints(d.root.ccmd);
      IF tl.execRes.errorCode # JunoRTError.Code.Halt THEN
        (* display an error message *)
        JunoError.Display(d, JunoRT.TrapMessage(tl.execRes))
      ELSE
        (* finish running current command if it ran up to final "HALT" *)
    	VAR execRes2 := JunoRT.Exec(); BEGIN
    	  <* ASSERT execRes2.trapCode = JunoRT.TrapCode.NormalHalt *>
    	END
      END;
      IF hintsChanged THEN
        Drawing.SourceUntrue(d, View.ModKind.ImplicitConsistent)
      END
    END;
    Drawing.Annotations(d)
  END ApplyDrag;

BEGIN
  WatchCursor := Cursor.FromName(ARRAY OF TEXT{"XC_watch"})
END Drag.
