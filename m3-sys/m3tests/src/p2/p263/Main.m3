(* Test First_readable_addr. *)

MODULE Main;

IMPORT F0, F1, F2, F4094, F4095, F4096, F4097;
IMPORT F4096x8, F4096x8p1, F4096x8p2;
IMPORT F4096x8m1, F4096x8m2;
IMPORT RTParams;

PROCEDURE F3() =
BEGIN
  EVAL F0.F1(NEW(REF F0.T0));
  EVAL F1.F1(NEW(REF F0.T1));
  EVAL F2.F1(NEW(REF F0.T2));

  EVAL F4094.F1(NEW(REF F0.T4094));
  EVAL F4095.F1(NEW(REF F0.T4095));
  EVAL F4096.F1(NEW(REF F0.T4096));
  EVAL F4097.F1(NEW(REF F0.T4097));
  
  (* x8 is times 8, m is minus, p is plus *)
  
  EVAL F4096x8m2.F1(NEW(REF F0.T4096x8m2));
  EVAL F4096x8m1.F1(NEW(REF F0.T4096x8m1));
  EVAL F4096x8.F1(NEW(REF F0.T4096x8));
  EVAL F4096x8p1.F1(NEW(REF F0.T4096x8p1));
  EVAL F4096x8p2.F1(NEW(REF F0.T4096x8p2));
END F3;

BEGIN
 F3();
 IF RTParams.IsPresent("checked") THEN
    EVAL F4096x8p2.F1(NIL); (* large -- gets null checked *)
 END;
 IF RTParams.IsPresent("unchecked") THEN
    EVAL F0.F1(NIL); (* small -- does not get null check *)
 END;
END Main.
