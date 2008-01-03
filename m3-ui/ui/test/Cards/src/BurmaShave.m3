(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Fri Apr 30 15:45:03 PDT 1993 by mjordan  *)
(*      modified on Mon Feb 24 14:01:28 PST 1992 by muller   *)
(*      modified on Tue Nov 19  0:01:38 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:54:15 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE BurmaShave;

IMPORT TextVBT, Filter, FilterClass, Thread, VBT, Rect, 
VBTClass;

REVEAL 
  Private = Filter.T BRANDED OBJECT END;
  T = Public BRANDED OBJECT 
    die, dead := TRUE; 
  OVERRIDES 
    reshape := Reshape; 
    rescreen := Rescreen; 
    init := Be 
  END;

TYPE AnimateClosure = Thread.SizedClosure OBJECT v: T 
  OVERRIDES apply := Animate END;

PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) RAISES {} =
  BEGIN
    v.die := Rect.IsEmpty(cd.new);
    IF v.dead AND NOT v.die THEN 
      v.dead := FALSE; EVAL Thread.Fork(NEW(AnimateClosure, v := v,
        stackSize := 20000)) 
    END;
    Filter.T.reshape(v, cd);
  END Reshape;

PROCEDURE Animate(cl: AnimateClosure): REFANY RAISES {} =
  VAR
    v := cl.v; ch: TextVBT.T := v.ch; oddCycle := FALSE;
  BEGIN
    LOOP
      LOCK VBT.mu DO
        IF v.die THEN v.dead := TRUE; RETURN NIL END;
        IF oddCycle THEN
          TextVBT.Put(ch, "Burma")
        ELSE
          TextVBT.Put(ch, "Shave")
        END;
        oddCycle := NOT oddCycle
      END;
      Thread.Pause(1.0d0)
    END
  END Animate;
  
PROCEDURE Rescreen(v: T; READONLY cd: VBT.RescreenRec) RAISES {} =
  BEGIN
    v.die := TRUE;
    VBT.Split.rescreen(v, cd);
  END Rescreen;

PROCEDURE New(): T = VAR t := NEW(T); BEGIN RETURN Be(t) END New;

PROCEDURE Be(v: T): T = 
  BEGIN RETURN Filter.T.init(v, TextVBT.New("Burma")) END Be;

BEGIN END BurmaShave.
