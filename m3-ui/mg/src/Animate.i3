(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Fri Aug 19 16:14:39 PDT 1994 by steveg   *)
(*      modified on Fri Jul 17 19:10:08 PDT 1992 by harrison *)
(*      modified on Thu Jul  9 18:35:53 1992 by mhb      *)

INTERFACE Animate;

<* PRAGMA LL *>

IMPORT RefList, MG, MGV, R2, R2Path, Thread;

TYPE
  TimeFunction = OBJECT
              METHODS
                <* LL = {v.mu} *>
                map (t: REAL): REAL;
                (* map range [0.0, 1.0] onto itself *)
              END;
  (* A TimeFunction controls the rate of change within an animation. An
     animation moves uniformly through the values 0.0 to 1.0 for t.
     TimeFunction.map adjusts t to have a different behaviour. *)

  TimeDiscrete <: TimeDiscretePublic;
  TimeDiscretePublic = TimeFunction OBJECT
                <* LL = {v.mu} *>
                values: REF ARRAY OF
                              RECORD
                                step : REAL;
                                value: REAL
                              END;
              END;
  (* A TimeDiscrete.map(t) returns TimeDiscrete.values[i].value where i is the
     first TDiscrete.values[i].step >= t. *)

  TimeStep <: TimeStepPublic;
  TimeStepPublic = TimeFunction OBJECT
            <* LL = {v.mu} *>
            steps := 1;
          END;
(* A TimeState.map(t) returns FLOOR(t * steps) / steps *)

VAR
  tfZero   : TimeFunction;          (* tfLinear.map(t) returns 0.0 *)
  tfOne    : TimeFunction;          (* tfLinear.map(t) returns 1.0 *)
  tfLinear : TimeFunction;          (* tfLinear.map(t) returns t *)
  tfInverse: TimeFunction;          (* tfInverse.map(t) returns 1.0 - t *)

TYPE
  T = MGV.AnimateT;

REVEAL
  T <: TPublic;

TYPE
  TPublic = OBJECT
        <* LL = {v.mu} *>
        tf: TimeFunction := NIL;   (* NIL => tfLinear *)
      METHODS
        <* LL < v.mu *>
        init (tf: TimeFunction := NIL): T; (* Default assigns tfLinear to "tf"
                                           if NIL *)

        <* LL <= VBT.mu *>
        start(v: MG.V);
        end(v: MG.V);
        length(v: MG.V; mg: MG.T): INTEGER;
        (* number of animation steps *)

        <* LL <= VBT.mu *>
        doStep (time, timePrev: REAL; v: MG.V; mg: MG.T);
        (* Do a step in the animation from "timePrev" to "time".
           "time" and "timePrev"have already been transformed by self.tf.map.
           "time" may be greater than, equal to or less than "timePrev"
          *)
      END;

TYPE
  Composite = OBJECT t: T; mg: MG.T END;

TYPE
  Group = MGV.AnimateGroup;

REVEAL
  Group <: GroupPublic;

TYPE
  GroupPublic = T OBJECT
    elems: RefList.T := NIL; (* RefList of "Composite"s *)
  METHODS
    (* must call init method *)
    <* LL = {v.mu} *>
    add(v: MG.V; composite: Composite);
    remove(v: MG.V; composite: Composite);

    iterate(gi: GroupIterator): BOOLEAN;
  END;

TYPE
  GroupIterator = OBJECT
    v: MG.V;
  METHODS
    proc(comp: Composite): BOOLEAN;
  END;

<* LL < v.mu for following procedures *>
PROCEDURE AddToGroup(g: Group; v: MG.V; comp: Composite);
PROCEDURE RemoveFromGroup(g: Group; v: MG.V; comp: Composite);
PROCEDURE IterateGroup(g: Group; v: MG.V; iter: GroupIterator): BOOLEAN;

TYPE
  (* Animation effects *)
  Linear <: LinearPublic;
  LinearPublic = T OBJECT
    <* LL = {v.mu} *>
    (* READONLY use methods to set *)
    vector: R2.T;
  METHODS
    (* must call init method *)
    <* LL = {v.mu} *>
    setVector(v: MG.V; READONLY vector: R2.T)
  END;

  Rotate <: RotatePublic;
  RotatePublic = T OBJECT
    <* LL = {v.mu} *>
    (* READONLY use methods to set *)
    origin: R2.T;
    angle: REAL; (* degrees *)
  METHODS
    (* must call init method *)
    <* LL = {v.mu} *>
    setRotate(v: MG.V; READONLY origin: R2.T; angle: REAL);
  END;
  (* rotate "angle" degrees around "origin" clockwise *)

  Scale <: ScalePublic;
  ScalePublic = T OBJECT
    <* LL = {v.mu} *>
    (* READONLY use methods to set *)
    wrt: R2.T;
    factor: R2.T;
  METHODS
    (* must call init method *)
    <* LL = {v.mu} *>
    setScale(v: MG.V; READONLY wrt, factor: R2.T);
  END;
  (* "wrt" remains constant in the animation *)

  Path = R2Path.T;

  Translate <: TranslatePublic;
  TranslatePublic = T OBJECT
       <* LL = {v.mu} *>
    (* READONLY use methods to set *)
    path: Path;
   METHODS
    (* must call init method *)
    <* LL = {v.mu} *>
    setTranslate(v: MG.V; path: Path);
  END;

  Weight <: WeightPublic;
  WeightPublic = T OBJECT
       <* LL = {v.mu} *>
    (* READONLY use methods to set *)
    delta: REAL;
   METHODS
    (* must call init method *)
    <* LL = {v.mu} *>
    setWeightDelta(v: MG.V; delta: REAL);
  END;

  Highlight <: HighlightPublic;
  HighlightPublic = T OBJECT
    (* must call init method *)
  END;
  (* length = 30 *)

  Visibility <: VisibilityPublic;
  VisibilityPublic = T OBJECT
    (* must call init method *)
  END;
  (* length = 30 *)

PROCEDURE Do(t: T; mg: MG.T; v: MG.V; duration := 1.0) RAISES {Thread.Alerted};
  (* call t.doStep(t.tf.map(time), v, mg) where time increases (roughly linearly)
     from 0.0 to 1.0 so that the animation takes duration seconds.  If 
     "duration" = 0.0 then only the last scene (time = 1.0) of the animation
     occurs. t.start is called at the start of the animation and t.end is called
     at the end.  Thread.Alerted may be called before or after any frame
     in the animation.
     LL <= VBT.mu *)
PROCEDURE Undo(t: T; mg: MG.T; v: MG.V; duration := 1.0) RAISES {Thread.Alerted};
  (* call t.doStep(t.tf.map(time), v, mg) where time decreases (roughly linearly)
     from 1.0 to 0.0 so that the animation takes duration seconds.  If
     "duration" = 0.0 then only the last scene (time = 0.0) of the animation
     occurs. t.start is called at the start of the animation and t.end is called
     at the end. Thread.Alerted may be called before or after any frame
     in the animation.
     LL <= VBT.mu *)


(* Animations are kept semi-synchronous by maintaining a global "animation
   time" based on real time.  Animation time is the real time that has 
   taken place, scaled by the current animation speed.  Each active animation
   calls "ATime" to determine the current time of the animation to display.

   Clients should call ResetAnimationTime at appropriate intervals between
   animations.  A call to Animate.Do/Undo without an intervening "ResetATime"
   will only display the final scene of the animation.

   Calling ResetATime or SetDuration during an animation will have
   undefined results.
*)

<* LL = Arbitrary *>
PROCEDURE ATime(): REAL;
  (* Returns the current animation time.  *)

PROCEDURE ResetATime();
  (* Resets animation time to 0.  *)

PROCEDURE SetDuration(seconds: REAL);
  (* Makes 1 second of animation time last "seconds" seconds of real time  *)

END Animate.
