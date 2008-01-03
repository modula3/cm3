
(* Copyright 1996-2000 Critical Mass, Inc. All Rights Reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE ClockVBT;
IMPORT Time, FmtTime, IntervalTimer, Font, TextVBT, PaintOp;

REVEAL
  Private = TextVBT.T BRANDED OBJECT END;
  T = Public BRANDED OBJECT 
    timer: Timer;
    proc: FmtProc;
  OVERRIDES
    init := Init;
  END;

TYPE
  Timer = IntervalTimer.T OBJECT vbt: T; OVERRIDES wakeup := Wakeup END;

PROCEDURE Wakeup(self: Timer) = 
  BEGIN
    TextVBT.Put (self.vbt, FmtTime.Long (Time.Now()));
  END Wakeup;

PROCEDURE Init(v: T; fnt: Font.T; halign: REAL; bgFg: PaintOp.ColorQuad;
               proc: FmtProc): T = 
  BEGIN
    EVAL TextVBT.T.init(v, "",
                   fnt := fnt, 
                   halign := halign, valign := 0.5,
                   bgFg := bgFg);
    v.timer := NEW(Timer, vbt := v).init(1.0D0);
    v.proc := proc;
    RETURN v;
  END Init;
  
BEGIN
END ClockVBT.
