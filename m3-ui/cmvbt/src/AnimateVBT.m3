
MODULE AnimateVBT;

IMPORT TextVBT, Filter, FilterClass, VBT, VBTClass, Time;
IMPORT Region;
IMPORT RefList;
IMPORT PaintOp;
IMPORT IntervalTimer;

REVEAL
  Private = Filter.T BRANDED OBJECT END;
  T = Public BRANDED OBJECT
    cycle := 0;
    stopped: BOOLEAN := TRUE;
    snap: REF ARRAY OF VBT.T;
  OVERRIDES
    init    := Init;
    repaint := Repaint;
    reset   := Reset;
  END;

  Group = GroupPublic BRANDED OBJECT 
    mu: MUTEX;
    timer: IntervalTimer.T;
    members: RefList.T;
  METHODS
    join(v: T) := GroupJoin;
  OVERRIDES
    init := GroupInit;
    reset := GroupReset; 
    wakeup := GroupWakeup;
  END;


(*-------------------------------------------------------------groups---*)

TYPE
  Timer = IntervalTimer.T OBJECT
    grp: Group;
  OVERRIDES
    wakeup := TimerWakeup;
  END;

PROCEDURE GroupInit(g: Group; duration: Time.T := DefaultDuration): Group =
BEGIN
  g.members := NIL;
  g.timer := NEW(Timer, grp := g).init(duration);
  g.mu := NEW(MUTEX);
  RETURN g;
END GroupInit;

PROCEDURE TimerWakeup(self: Timer) = 
  BEGIN
    self.grp.wakeup();
  END TimerWakeup;

PROCEDURE GroupJoin(g: Group; v: T) =
VAR
  first: T;
BEGIN
  LOCK g.mu DO 
    IF g.members # NIL THEN
      first := g.members.head;
      Reset (v, first.cycle);
    END;
    g.members := RefList.Cons (v, g.members);
    (* Join the cycle. *)
  END;
END GroupJoin;

PROCEDURE GroupReset(g: Group; position: CARDINAL := 0) =
VAR
  l: RefList.T;
  v: T;
BEGIN
  LOCK g.mu DO
    l := g.members;
    WHILE l # NIL DO
      v := l.head;
      v.cycle := position;
      l := l.tail;
    END; 
  END;
END GroupReset;

PROCEDURE GroupWakeup(g: Group) =
BEGIN
  LOCK g.mu DO
    VAR
      i := g.members;
      l: RefList.T := NIL;
      v: T;
    BEGIN
      WHILE i # NIL DO
        v := i.head;
        IF v.stopped THEN RETURN; END;
        v.snap[v.cycle].parent := NIL; (* Why? *)
        EVAL Filter.Replace (v, v.snap[v.cycle]); 
        l := RefList.Cons (v, l);
        v.cycle := (v.cycle + 1) MOD NUMBER (v.snap^);
        i := i.tail;
      END;
      WHILE l # NIL DO 
        VBT.Mark (l.head);
        l := l.tail;
      END;
    END;
  END;
END GroupWakeup;

(*------------------------------------------------ AnimateVBT.T methods ---*)

PROCEDURE New(READONLY snap: ARRAY OF VBT.T; dur: Time.T): T = 
  VAR t := NEW(T); g := NEW(Group).init(dur); 
  BEGIN RETURN t.init(snap, g) END New;

PROCEDURE Init(v: T; READONLY snap: ARRAY OF VBT.T; g: Group; 
               <*UNUSED*>op: PaintOp.T): T =
  BEGIN  
    v.snap := NEW(REF ARRAY OF VBT.T, NUMBER(snap)); v.snap^ := snap; 
    IF g = NIL THEN g := NEW(Group).init(duration := DefaultDuration) END;
    g.join (v);
    Go(v); 
    EVAL Filter.T.init(v, v.snap[0]);
    RETURN v;
  END Init;

PROCEDURE Repaint(v: T; READONLY br: Region.T) =
  BEGIN
    Public.repaint(v, br);
    VBT.Sync (v);           (* Make sure changes are flushed *)
  END Repaint;

PROCEDURE Reset (v: T; position: CARDINAL := 0) = 
  BEGIN
    v.cycle := position;
  END Reset;

PROCEDURE Stop(v: T) =
  BEGIN
    v.stopped := TRUE;
  END Stop;

PROCEDURE Go(v: T) =
  BEGIN
    v.stopped := FALSE;
  END Go;

(*------------------------------------------------------------ blinking ---*)

PROCEDURE Blink (ch: VBT.T; op: PaintOp.T := PaintOp.Bg): T = 
VAR
  cq := PaintOp.MakeColorQuad (op, PaintOp.FromRGB (1.0, 1.0, 1.0));
BEGIN 
  RETURN NEW(T).init(snapshots := ARRAY OF VBT.T 
                                        { TextVBT.New(" ", bgFg := cq), ch},
                     group := BlinkGroup());
END Blink;

(* BlinkGroup lazily creates a new group. *)

VAR
  bg: Group := NIL;

PROCEDURE BlinkGroup(): Group = 
  BEGIN
    IF bg = NIL THEN 
      bg := NEW(Group).init(duration := DefaultDuration);
    END;
    RETURN bg;
  END BlinkGroup;

BEGIN 
END AnimateVBT.
