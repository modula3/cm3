(* Copyright 1996-2000, Critical Mass, Inc. All Rights Reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE HoverVBT;

IMPORT Filter, Point, Rect, Thread, Time, Trestle, VBT, Word;

REVEAL
  T = Filter.T BRANDED OBJECT
        state     : State    := State.Looking;
        timer     : Timer    := NIL;
      OVERRIDES
        init     := Init;
        position := Position;
      END;

TYPE
  State = {
    Looking,  (* => haven't noticed mouse over a registered vbt *)
    Hovering, (* => we have noticed one, waiting for the timer to expire *)
    Delivered (* => made the callback, now waiting for the mouse to leave *)
  };

TYPE
  Child = REF RECORD
    v     : VBT.T;
    cb    : CallBack;
    ref   : REFANY;
    delay : Time.T;
    next  : Child;
  END;

VAR
  mu := NEW (MUTEX);
  registered : Child := NIL;

PROCEDURE New (ch: VBT.T): T =
  BEGIN
    RETURN NEW(T).init(ch);
  END New;

PROCEDURE Init (v: T;  ch: VBT.T): Filter.T =
  BEGIN
    EVAL Filter.T.init (v, ch);
    VBT.SetCage (v, VBT.GoneCage);
    RETURN v;
  END Init;

PROCEDURE Register (v: VBT.T;  delay: Time.T;  cb: CallBack;  ref: REFANY) =
  VAR x: Child;
  BEGIN
    LOCK mu DO
      x := registered;
      WHILE (x # NIL) DO
        IF (x.v = v) THEN
          x.delay := delay;
          x.cb := cb;
          x.ref := ref;
          RETURN;
        END;
        x := x.next;
      END;
      registered := NEW (Child, v := v, delay := delay, cb := cb,
                         ref := ref, next := registered);
    END;
  END Register;

PROCEDURE Position (v: T;  READONLY cd: VBT.PositionRec) =
  VAR x: Child;  p, hDelta: Point.T;
  BEGIN
    CASE v.state OF
    | State.Looking =>
        IF cd.cp.gone THEN
          VBT.SetCage (v, VBT.GoneCage);
          RETURN;
        END;

        p := Trestle.ScreenOf (v, cd.cp.pt).q;
        hDelta := Point.Sub (cd.cp.pt, p);

        LOCK mu DO
          x := registered;
          LOOP
            IF (x = NIL) THEN
              (* no matches, still looking *)
              VBT.SetCage (v, VBT.CageFromPosition (cd.cp));
              RETURN;
            END;
            IF HoverParent (x.v) = v THEN
              VAR
                chDom := VBT.Domain (x.v);
                nw    := Rect.NorthWest (chDom);
                qnw   := Trestle.ScreenOf (x.v, nw).q;
                delta := Point.Sub (qnw, nw);
                qDom  := Rect.Move (chDom, delta);
              BEGIN
                IF Rect.Member (p, qDom) THEN
                  v.state  := State.Hovering;
                  StartTimer (v, x);
                  VBT.SetCage (v, VBT.CageFromRect(Rect.Move(qDom, hDelta), cd.cp));
                  RETURN;
                END;
              END;
            END;
            x := x.next;
          END;
        END;

    | State.Hovering =>
        (* oops, the mouse left the target before its timeout... *)
        AbortTimer (v);
        v.state  := State.Looking;
        VBT.SetCage (v, VBT.GoneCage);

    | State.Delivered =>
        (* the mouse finally left the target vbt *)
        v.state  := State.Looking;
        VBT.SetCage (v, VBT.GoneCage);
    END;
  END Position;

PROCEDURE HoverParent (v: VBT.T): T =
  (* Find the nearest ancestor of "v" that is a hover vbt *)
  BEGIN
    LOOP
      TYPECASE v OF
      | T(t) => RETURN t;
      ELSE v := VBT.Parent (v);
      END;
    END;
  END HoverParent;

(*------------------------------------------------------- timeout thread ---*)

TYPE
  Timer = Thread.Closure OBJECT
    self      : Thread.T := NIL;
    hover     : T        := NIL;
    target    : Child    := NIL;    (* the current hover target *)
    start     : Time.T   := 0.0d0;  (* when we started the delay timer *)
    start_uid : INTEGER  := 0;      (* the "target"'s UID *)
    uid       : INTEGER  := 0;      (* UID for each timer activation *)
    mu        : MUTEX    := NIL;
    burp      : Thread.Condition := NIL;
  OVERRIDES
    apply := TimerRoot;
  END;

PROCEDURE StartTimer (v: T;  x: Child) =
  VAR t := v.timer;
  BEGIN
    IF (t = NIL) THEN
      t := NEW (Timer);  v.timer := t;
      t.hover := v;
      t.mu    := NEW (MUTEX);
      t.burp  := NEW (Thread.Condition);
      t.self  := Thread.Fork (t);
    END;

    LOCK t.mu DO
      t.uid       := Word.Plus (t.uid, 1);
      t.target    := x;
      t.start     := Time.Now ();
      t.start_uid := t.uid;
    END;
    Thread.Signal (t.burp);
  END StartTimer;

PROCEDURE AbortTimer (v: T) =
  VAR t := v.timer;
  BEGIN
    LOCK t.mu DO
      t.uid    := Word.Plus (t.uid, 1);
      t.target := NIL;
    END;
    Thread.Alert (t.self);
  END AbortTimer;

PROCEDURE TimerRoot (t: Timer): REFANY =
  VAR
    x     : Child;
    seq   : INTEGER;
    pause : Time.T;
    now   : Time.T;
  BEGIN
    LOOP
      (* wait for a timer to be scheduled *)
      LOCK t.mu DO
        WHILE (t.target = NIL) OR (t.start_uid # t.uid) DO
          Thread.Wait (t.mu, t.burp);
        END;
        x   := t.target;
        seq := t.start_uid;
        now := Time.Now ();
        pause := (t.start + x.delay) - now;
      END;

      IF (pause > 0.0d0) THEN
        (* wait... *)
        TRY Thread.AlertPause (pause);
        EXCEPT Thread.Alerted => x := NIL; (* aborted *)
        END;
      END;

      IF (x # NIL) THEN
        LOCK t.mu DO
          IF (t.uid = seq)
            THEN t.target := NIL;    (* this timeout is still current. *)
            ELSE x := NIL;           (* darn, this timeout was canceled *)
          END;
        END;

        IF x # NIL THEN MakeCallback (t, x); END;
      END;
      
    END;
  END TimerRoot;

PROCEDURE MakeCallback (t: Timer;  x: Child) =
  CONST Here = VBT.CursorPosition { Point.Origin, 0, FALSE, FALSE };
  VAR
    dom, cDom : Rect.T;
    nw, qnw   : Point.T;
    cnw, cqnw : Point.T;
    d0, d1    : Point.T;
  BEGIN
    (* let the hover vbt know that this event was delivered *)
    LOCK VBT.mu DO
      dom   := VBT.Domain (t.hover);
      nw    := Rect.NorthWest (dom);
      qnw   := Trestle.ScreenOf (t.hover, nw).q;
      d0    := Point.Sub (nw, qnw);

      cDom  := VBT.Domain (x.v);
      cnw   := Rect.NorthWest (cDom);
      cqnw  := Trestle.ScreenOf (x.v, cnw).q;
      d1    := Point.Sub (cqnw, cnw);

      (* map the child domain to screen coordinates *)
      cDom  := Rect.Move (cDom, d1);

      (* and then map it back to the hover vbt's coordinates *)
      cDom  := Rect.Move (cDom, d0);

      (* finally, reset the hover VBT's cage *)
      t.hover.state := State.Delivered;
      VBT.SetCage (t.hover, VBT.CageFromRect (cDom, Here));
    END;

    (* and deliver it... *)
    x.cb (x.v, x.ref);
  END MakeCallback;

BEGIN
END HoverVBT.

