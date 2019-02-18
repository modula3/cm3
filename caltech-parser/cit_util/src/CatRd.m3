MODULE CatRd;
IMPORT Rd, Thread;
IMPORT TextRd;

<* FATAL Rd.Failure, Thread.Alerted *>
(* future implementations might propagate these more nicely... *)

PROCEDURE Cat(r0, r1: Rd.T): Rd.T =
  VAR
    t0 := Rd.GetText(r0, LAST(INTEGER));
    t1 := Rd.GetText(r1, LAST(INTEGER));
  BEGIN
    RETURN TextRd.New(t0 & t1);
    (* future implementations might not use "&". *)
  END Cat;

BEGIN
END CatRd.
