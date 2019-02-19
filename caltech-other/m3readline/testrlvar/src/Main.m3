(* $Id$ *)

MODULE Main;
IMPORT ReadLine;
IMPORT VarUI;
IMPORT Fmt, XTime AS Time;

PROCEDURE ShowTime(p : VarUI.VarProxy; varName : TEXT) : TEXT =
  BEGIN RETURN Fmt.LongReal(Time.Now()) END ShowTime;

BEGIN
  WITH t = NEW(ReadLine.T).init(),
       intf = NEW(VarUI.T).init("testvar") DO

    EVAL intf.addVar("time", NEW(VarUI.VarProxy, show := ShowTime).init(VarUI.ProxyMode.ReadOnly), "Time since the local OS epoch");

    EVAL intf.addVar("a", NEW(VarUI.DefIntProxy).init(11));

    t.startProc();
    t.display("Hello there.\n");
    intf.run(t);
  END
END Main.









