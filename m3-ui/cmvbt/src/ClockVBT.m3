
(* Copyright 1996-2000 Critical Mass, Inc. All Rights Reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* 05/16/2001 - Fixed bugs that caused T.proc not to be used and that
                prevented specification of the time zone. -- R.C.Coleburn *)

MODULE ClockVBT;
IMPORT Time, IntervalTimer, Font, TextVBT, PaintOp, Date;

REVEAL
  Private = TextVBT.T BRANDED OBJECT END;
  T = Public BRANDED OBJECT
    timer: Timer;
    proc: FmtProc;
    zone: Date.TimeZone;
  OVERRIDES
    init := Init;
  END;

TYPE
  Timer = IntervalTimer.T OBJECT vbt: T; OVERRIDES wakeup := Wakeup END;

PROCEDURE Wakeup(self: Timer) =
  BEGIN
    TextVBT.Put (self.vbt, self.vbt.proc(Time.Now(), self.vbt.zone));
  END Wakeup;

PROCEDURE Init(v: T; fnt: Font.T; halign: REAL; bgFg: PaintOp.ColorQuad;
               proc: FmtProc;
               zone: Date.TimeZone): T =
  BEGIN
    EVAL TextVBT.T.init(v, "",
                   fnt := fnt,
                   halign := halign, valign := 0.5,
                   bgFg := bgFg);
    v.timer := NEW(Timer, vbt := v).init(1.0D0);
    v.proc := proc;
    v.zone := zone;
    RETURN v;
  END Init;

BEGIN
END ClockVBT.
