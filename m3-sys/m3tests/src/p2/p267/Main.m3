(* Test if I can RTHeapRep.RegisterFinalCleanup multiple functions
for the same object *)

UNSAFE MODULE Main;
IMPORT RTHeapRep, RTIO, RTCollector;

PROCEDURE Clean1(r: REFANY) =
BEGIN
  RTIO.PutText("Clean1:");
  RTIO.PutAddr(LOOPHOLE(r, ADDRESS));
  RTIO.PutText("\n");
  RTIO.Flush();
END Clean1;

PROCEDURE Clean2(r: REFANY) =
BEGIN
  RTIO.PutText("Clean2:");
  RTIO.PutAddr(LOOPHOLE(r, ADDRESS));
  RTIO.PutText("\n");
  RTIO.Flush();
END Clean2;

TYPE A = OBJECT END;

PROCEDURE Test() =
VAR a := NEW(A);
BEGIN
  RTIO.PutText("Test:");
  RTIO.PutAddr(LOOPHOLE(a, ADDRESS));
  RTIO.PutText("\n");
  RTIO.Flush();
  RTHeapRep.RegisterFinalCleanup(a, Clean1);
  RTHeapRep.RegisterFinalCleanup(a, Clean2);
END Test;

BEGIN
 WHILE TRUE DO
   Test();
   RTCollector.Collect();
 END;
END Main.
