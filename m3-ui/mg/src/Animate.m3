(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Mon May 23 23:01:16 PDT 1994 by mhb      *)
(*      modified on Tue Jun 22 10:04:08 PDT 1993 by steveg   *)
(*      modified on Fri Jul 17 19:12:40 PDT 1992 by harrison *)


MODULE Animate;

<* PRAGMA LL *>

IMPORT Axis, RefList, Math, MG, Pts, R2, R2Box, RefListUtils, Region, 
  Thread, Time;

VAR
  etMu := NEW(MUTEX);
  <* LL = etMu *>
  etScale: LONGREAL := 1.0D0; (* = MIN(1000000.0, 1.0/etSeconds) *)
  etStart: Time.T;

PROCEDURE ATime (): REAL =
  <* LL = arbitrary *>
  BEGIN
    LOCK etMu DO RETURN FLOAT(etScale * (Time.Now() - etStart)) END;
  END ATime;

PROCEDURE ResetATime () =
  BEGIN
    LOCK etMu DO etStart := Time.Now() END
  END ResetATime;

PROCEDURE SetDuration (seconds: REAL) =
  BEGIN
    LOCK etMu DO 
      IF seconds = 0.0 THEN etScale := 1.0D0
      ELSE etScale := FLOAT(MIN(1000000.0, 1.0 / seconds), LONGREAL) 
      END
    END
  END SetDuration;

<* INLINE *> PROCEDURE DoOneAnimation (t             : T;
                                       time, timePrev: REAL;
                                       v             : MG.V;
                                       mg            : MG.T  ) =
  BEGIN
    t.doStep(t.tf.map(time), t.tf.map(timePrev), v, mg);
  END DoOneAnimation;

PROCEDURE DoAnimation (t: T; time, timePrev: REAL; v: MG.V; mg: MG.T)
  RAISES {Thread.Alerted} =
  BEGIN
    IF Thread.TestAlert() THEN RAISE Thread.Alerted END;
    DoOneAnimation(t, time, timePrev, v, mg);
    v.mgRedisplay(Region.Empty);
  END DoAnimation;

PROCEDURE Do (t: T; mg: MG.T; v: MG.V; duration := 1.0) RAISES {Thread.Alerted} =
  VAR
    timePrev       := 0.0;
    time           := ATime();
    dt      : REAL;
  BEGIN
    t.start(v);
    dt := duration / FLOAT(MAX(1, t.length(v, mg)));
    WHILE time < duration DO
      WITH pause = dt - (time - timePrev) DO
        IF pause > 0.0 THEN <* ASSERT (pause < 60.0) *>
          Thread.AlertPause(FLOAT(pause, LONGREAL));
        END;
      END;
      DoAnimation(t, time / duration, timePrev / duration, v, mg);
      timePrev := time;
      time := ATime();
    END;
    IF duration = 0.0 THEN
      DoAnimation(t, 1.0, 0.0, v, mg);
    ELSE
      DoAnimation(t, 1.0, timePrev / duration, v, mg);
    END;
    t.end(v);
  END Do;

PROCEDURE Undo (t: T; mg: MG.T; v: MG.V; duration := 1.0) RAISES {Thread.Alerted} =
  VAR
    timePrev       := 0.0;
    time           := ATime();
    dt      : REAL;
  BEGIN
    t.start(v);
    dt := duration / FLOAT(MAX(1, t.length(v, mg)));
    WHILE time < duration DO
      WITH pause = dt - (time - timePrev) DO
        IF pause > 0.0 THEN <* ASSERT (pause < 60.0) *>
          Thread.AlertPause(FLOAT(pause, LONGREAL));
        END;
      END;
      DoAnimation(t, (duration - time) / duration,
                  (duration - timePrev) / duration, v, mg);
      timePrev := time;
      time := ATime();
    END;
    IF duration = 0.0 THEN
      DoAnimation(t, 0.0, 1.0, v, mg);
    ELSE
      DoAnimation(t, 0.0, (duration - timePrev) / duration, v, mg);
    END;
    t.end(v);
  END Undo;

REVEAL
  T = TPublic BRANDED OBJECT
      OVERRIDES
        init   := InitT;
        start  := StartDefault;
        end    := EndDefault;
        length := DefaultLength; (* sic *)
        doStep := DoStepError;
      END;

PROCEDURE InitT (t: T; tf: TimeFunction := NIL): T =
  BEGIN
    IF tf = NIL THEN t.tf := tfLinear ELSE t.tf := tf END;
    RETURN t;
  END InitT;
PROCEDURE StartDefault (<* UNUSED *> t: T; <* UNUSED *> v: MG.V) =
  BEGIN
  END StartDefault;
PROCEDURE EndDefault (<* UNUSED *> t: T; <* UNUSED *> v: MG.V) =
  BEGIN
  END EndDefault;
PROCEDURE DefaultLength (<* UNUSED *> t : T;
                         <* UNUSED *> v : MG.V;
                         <* UNUSED *> mg: MG.T  ): INTEGER =
  BEGIN
    RETURN 30
  END DefaultLength;
PROCEDURE DoStepError (<* UNUSED *> t             : T;
                       <* UNUSED *> time, timePrev: REAL;
                       <* UNUSED *> v             : MG.V;
                       <* UNUSED *> mg            : MG.T  ) =
  BEGIN                         <* ASSERT FALSE *>
  END DoStepError;

PROCEDURE MaxLength (v: MG.V; x, y: REAL): INTEGER =
  BEGIN
    RETURN ROUND(MAX(Pts.ToPixels(v, ABS(x), Axis.T.Hor),
               Pts.ToPixels(v, ABS(y), Axis.T.Ver)));
  END MaxLength;

REVEAL
  Group = GroupPublic BRANDED OBJECT
  OVERRIDES
    start := StartGroup;
    end := EndGroup;
    length := LengthGroup;
    doStep := DoStepGroup;
    add := GroupAdd; (* sic *)
    remove := GroupRemove; (* sic *)
    iterate := GroupIterate; (* sic *)
  END;

PROCEDURE Starter(iter: GroupIterator; comp: Composite): BOOLEAN =
  BEGIN
    comp.t.start(iter.v);
    RETURN TRUE;
  END Starter;

PROCEDURE StartGroup(group: Group; v: MG.V) =
  BEGIN
    EVAL group.iterate(NEW(GroupIterator, v:= v, proc := Starter));
  END StartGroup;

PROCEDURE Ender(iter: GroupIterator; comp: Composite): BOOLEAN =
  BEGIN
    comp.t.end(iter.v);
    RETURN TRUE;
  END Ender;

PROCEDURE EndGroup(group: Group; v: MG.V) =
  BEGIN
    EVAL group.iterate(NEW(GroupIterator, v:= v, proc := Ender));
  END EndGroup;

TYPE
  IterLength = GroupIterator OBJECT
    length: INTEGER;
  OVERRIDES
    proc := Lengther;
  END;

PROCEDURE Lengther(iter: IterLength; comp: Composite): BOOLEAN =
  BEGIN
    iter.length := MAX(iter.length, comp.t.length(iter.v, comp.mg));
    RETURN TRUE;
  END Lengther;

PROCEDURE LengthGroup (group: Group; v: MG.V; <* UNUSED *> mg: MG.T):
  INTEGER =
  BEGIN
    WITH iter = NEW(IterLength, v := v, length := 0) DO
      EVAL group.iterate(iter);
      RETURN iter.length;
    END;
  END LengthGroup;

TYPE
  IterDoStep = GroupIterator OBJECT
    time, timePrev: REAL;
  OVERRIDES
    proc := DoStepper;
  END;

PROCEDURE DoStepper (iter: IterDoStep; comp: Composite): BOOLEAN =
  BEGIN
    DoOneAnimation(comp.t, iter.time, iter.timePrev, iter.v, comp.mg);
    RETURN TRUE;
  END DoStepper;

PROCEDURE DoStepGroup (             group         : Group;
                                    time, timePrev: REAL;
                                    v             : MG.V;
                       <* UNUSED *> mg            : MG.T   ) =
  BEGIN
    EVAL group.iterate(
      NEW(IterDoStep, v := v, time := time, timePrev := timePrev));
  END DoStepGroup;

PROCEDURE AddToGroup(g: Group; v: MG.V; comp: Composite) =
  BEGIN
    LOCK v.mu DO g.add(v, comp) END
  END AddToGroup;

PROCEDURE RemoveFromGroup(g: Group; v: MG.V; comp: Composite) =
  BEGIN
    LOCK v.mu DO g.remove(v, comp) END;
  END RemoveFromGroup;

PROCEDURE IterateGroup (g: Group; v: MG.V; iter: GroupIterator): BOOLEAN =
  BEGIN
    LOCK v.mu DO iter.v := v; RETURN g.iterate(iter) END;
  END IterateGroup;

PROCEDURE GroupAdd (group: Group; <* UNUSED *> v: MG.V; comp: Composite) =
  BEGIN
    group.elems := RefList.Cons(comp, group.elems);
  END GroupAdd;

PROCEDURE GroupRemove (group: Group; <* UNUSED *> v: MG.V; comp: Composite) =
  BEGIN
    RefListUtils.DeleteQ(group.elems, comp);
  END GroupRemove;

PROCEDURE GroupIterate (group: Group; iter: GroupIterator): BOOLEAN =
  VAR f := group.elems;
  BEGIN
    WHILE f # NIL DO
      WITH comp = NARROW(f.head, Composite) DO
        IF NOT iter.proc(comp) THEN RETURN FALSE; END; 
      END;
      f := f.tail;
    END;  
    RETURN TRUE
  END GroupIterate;

REVEAL
  Linear = LinearPublic BRANDED OBJECT
           OVERRIDES
             setVector := SetVector;
             length := LengthLinear;
             doStep := DoStepLinear;
           END;

PROCEDURE SetVector(t: Linear;  <* UNUSED *> v: MG.V; READONLY vector: R2.T) =
  BEGIN
    t.vector := vector;
  END SetVector;

PROCEDURE LengthLinear (t: Linear; v: MG.V; <* UNUSED *> mg: MG.T):
  INTEGER =
  BEGIN
    RETURN MaxLength(v, t.vector[0], t.vector[1]);
  END LengthLinear;

PROCEDURE DoStepLinear (t: Linear; time, timePrev: REAL; v: MG.V; mg: MG.T) =
  BEGIN
    LOCK v.mu DO
      MG.RTranslateLocked(mg, v, R2.Scale(time - timePrev, t.vector));
    END;
  END DoStepLinear;

REVEAL
  Rotate = RotatePublic BRANDED OBJECT
  OVERRIDES
    setRotate := SetRotate;
    length := LengthRotate;
    doStep := DoStepRotate;
  END;

PROCEDURE SetRotate(t: Rotate;  <* UNUSED *> v: MG.V; READONLY origin: R2.T; angle: REAL) =
  BEGIN
    t.origin := origin;
    t.angle := angle;
  END SetRotate;

PROCEDURE Furthest (pt: R2.T; bounds: R2Box.T): R2.T =
  BEGIN
    RETURN R2.T{MAX(ABS(bounds[0].lo - pt[0]), ABS(bounds[0].hi - pt[0])),
                MAX(ABS(bounds[1].lo - pt[1]), ABS(bounds[1].hi - pt[1]))}
  END Furthest;

CONST
  DToR = Math.Pi / 180.0;

(* 2 * pi * r * angle / 360 *)
PROCEDURE LengthRotate (t: Rotate; v: MG.V; mg: MG.T): INTEGER =
  VAR
    r     : R2.T;
    bounds: R2Box.T;
  BEGIN
    LOCK v.mu DO bounds := MG.BoundingBoxLocked(mg, v); END;
    r := Furthest(t.origin, bounds);
    RETURN MaxLength(v, t.angle * DToR * r[0], t.angle * DToR * r[1]);
  END LengthRotate;

PROCEDURE DoStepRotate (t: Rotate; time, timePrev: REAL; v: MG.V; mg: MG.T) =
  BEGIN
    LOCK v.mu DO
      MG.RotateLocked(mg, v, (time - timePrev) * t.angle, t.origin);
    END;
  END DoStepRotate;

REVEAL
  Scale = ScalePublic BRANDED OBJECT
          OVERRIDES
            setScale := SetScale;
            length := LengthScale;
            doStep := DoStepScale;
          END;

PROCEDURE SetScale(t: Scale;  <* UNUSED *> v: MG.V; READONLY wrt, factor: R2.T) =
  BEGIN
    t.wrt := wrt;
    t.factor := factor;
  END SetScale;

PROCEDURE LengthScale (t: Scale; v: MG.V; mg: MG.T): INTEGER =
  VAR r: R2.T;
  BEGIN
    LOCK v.mu DO r := Furthest(t.wrt, MG.BoundingBoxLocked(mg, v)); END;
    RETURN MaxLength(v, ABS(t.factor[0] - 1.0) * r[0],
                     ABS(t.factor[1] - 1.0) * r[1]);
  END LengthScale;

PROCEDURE ScaleStep (time, timePrev, factor: REAL): REAL =
  VAR
    num   := 1.0 + time * (factor - 1.0);
    denom := 1.0 + timePrev * (factor - 1.0);
  BEGIN
    IF denom = 0.0 THEN
      IF num = 0.0 THEN RETURN 1.0 ELSE <* ASSERT FALSE *> END;
    ELSE
      RETURN num / denom;
    END;
  END ScaleStep;

(* scale in arithmetic steps *)
PROCEDURE DoStepScale (t: Scale; time, timePrev: REAL; v: MG.V; mg: MG.T) =
  VAR
    dsx := ScaleStep(time, timePrev, t.factor[0]);
    dsy := ScaleStep(time, timePrev, t.factor[1]);
  BEGIN
    LOCK v.mu DO MG.ScaleLocked(mg, v, R2.T{dsx, dsy}, t.wrt) END;
  END DoStepScale;

REVEAL
  Translate = TranslatePublic BRANDED OBJECT
              OVERRIDES
                setTranslate := SetTranslate;
                start        := StartTranslate;
                end          := EndTranslate;
                length       := LengthTranslate;
                doStep       := DoStepTranslate;
              END;

PROCEDURE SetTranslate(t: Translate; <* UNUSED *> v: MG.V; path: Path) =
  BEGIN
    t.path := path;
  END SetTranslate;

PROCEDURE StartTranslate (<* UNUSED *> t: Translate; <* UNUSED *> v: MG.V) =
  BEGIN <* ASSERT FALSE *>
  END StartTranslate;
PROCEDURE EndTranslate (<* UNUSED *> t: Translate; <* UNUSED *> v: MG.V) =
  BEGIN <* ASSERT FALSE *>
  END EndTranslate;
PROCEDURE LengthTranslate (<* UNUSED *> t : Translate;
                            <* UNUSED *> v : MG.V;
                            <* UNUSED *> mg: MG.T       ): INTEGER =
  BEGIN <* ASSERT FALSE *>
  END LengthTranslate;
PROCEDURE DoStepTranslate (<* UNUSED *> t             : Translate;
                           <* UNUSED *> time, timePrev: REAL;
                           <* UNUSED *> v             : MG.V;
                           <* UNUSED *> mg            : MG.T       ) =
  BEGIN <* ASSERT FALSE *>
  END DoStepTranslate;

REVEAL Weight = WeightPublic BRANDED OBJECT
  OVERRIDES
    setWeightDelta := SetWeightDelta;
    length := LengthWeight;
    doStep := DoStepWeight;
  END;

PROCEDURE SetWeightDelta(t: Weight; <* UNUSED *> v: MG.V; delta: REAL) =
  BEGIN
    t.delta := delta;
  END SetWeightDelta;

PROCEDURE LengthWeight (t: Weight; v: MG.V; <* UNUSED *> mg: MG.T): INTEGER =
  BEGIN
    RETURN MaxLength(v, t.delta, t.delta)
  END LengthWeight;

PROCEDURE DoStepWeight (t: Weight; time, timePrev: REAL; v: MG.V; mg: MG.T) =
  BEGIN
    LOCK v.mu DO
      mg.setWeight(v, mg.weight + (time - timePrev) * t.delta);
    END;
  END DoStepWeight;

REVEAL Highlight = HighlightPublic BRANDED OBJECT
  OVERRIDES
    length := LengthHighlight;
    doStep := DoStepHighlight;
  END;

PROCEDURE LengthHighlight (<* UNUSED *> t: Highlight; <* UNUSED *> v: MG.V; <* UNUSED *> mg: MG.T): INTEGER =
  BEGIN
    RETURN 30;
  END LengthHighlight;

PROCEDURE DoStepHighlight (<* UNUSED *> t             : Highlight;
                                        time, timePrev: REAL;
                                        v             : MG.V;
                                        mg            : MG.T       ) =
  BEGIN
    LOCK v.mu DO mg.setHighlight(v, mg.highlight + (time - timePrev)); END;
  END DoStepHighlight;

REVEAL Visibility = VisibilityPublic BRANDED OBJECT
  OVERRIDES
    length := LengthVisible;
    doStep := DoStepVisible;
  END;

PROCEDURE LengthVisible (<* UNUSED *> t : Visibility;
                         <* UNUSED *> v : MG.V;
                         <* UNUSED *> mg: MG.T        ): INTEGER =
  BEGIN
    RETURN 30;
  END LengthVisible;

PROCEDURE DoStepVisible (<* UNUSED *> t             : Visibility;
                                      time, timePrev: REAL;
                                      v             : MG.V;
                                      mg            : MG.T        ) =
  BEGIN
    LOCK v.mu DO mg.setVisible(v, mg.visible + (time - timePrev)) END;
  END DoStepVisible;

PROCEDURE TFZero (<* UNUSED *> tf: TimeFunction; <* UNUSED *> t: REAL): REAL =
  BEGIN
    RETURN 0.0
  END TFZero;

PROCEDURE TFOne (<* UNUSED *> tf: TimeFunction; <* UNUSED *> t: REAL): REAL =
  BEGIN
    RETURN 1.0
  END TFOne;

PROCEDURE TFLinear (<* UNUSED *> tf: TimeFunction; t: REAL): REAL =
  BEGIN
    RETURN t
  END TFLinear;

PROCEDURE TFInverse (<* UNUSED *> tf: TimeFunction; t: REAL): REAL =
  BEGIN
    RETURN 1.0 - t
  END TFInverse;

REVEAL
  TimeDiscrete =
    TimeDiscretePublic BRANDED OBJECT OVERRIDES map := TFDiscrete END;

PROCEDURE TFDiscrete (tf: TimeDiscrete; t: REAL): REAL =
  BEGIN
    FOR i := 0 TO LAST(tf.values^) DO
      IF tf.values[i].step >= t THEN
        RETURN tf.values[i].value
      END;
    END;
    RETURN tf.values[LAST(tf.values^)].value
  END TFDiscrete;

REVEAL
  TimeStep = TimeStepPublic BRANDED OBJECT OVERRIDES map := TFSteps; END;

PROCEDURE TFSteps (tf: TimeStep; t: REAL): REAL =
  BEGIN
    IF tf.steps = 0 THEN
      RETURN 0.0
    ELSE
      RETURN FLOAT(FLOOR(t * FLOAT(tf.steps))) / FLOAT(tf.steps);
    END;
  END TFSteps;

BEGIN
  tfZero := NEW(TimeFunction, map := TFZero);
  tfOne := NEW(TimeFunction, map := TFOne);
  tfLinear := NEW(TimeFunction, map := TFLinear);
  tfInverse := NEW(TimeFunction, map := TFInverse);
END Animate.
