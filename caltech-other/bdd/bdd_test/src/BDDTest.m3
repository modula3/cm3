(* $Id$ *)
MODULE BDDTest EXPORTS Main;
IMPORT BDD,Debug,Fmt;
IMPORT RTProcess;
IMPORT RTCollectorSRC;

CONST 
  N = 500;

PROCEDURE Test() =
  BEGIN
    Debug.Out("BDDTest.Test...");

    VAR
      a : ARRAY [0..N - 1] OF BDD.T;
      x := BDD.True();
      y := BDD.False();
    BEGIN
(*
      FOR i := 0 TO 1000 DO
        FOR j := 0 TO 1000 DO
          VAR new := BDD.New(); BEGIN END
        END;
        Debug.Out(Fmt.Int(i))
      END;
*)

      FOR i := 0 TO N - 1 DO
        a[i] := BDD.New();
        x := BDD.And(x,a[i]);
        y := BDD.Or(y,BDD.Not(a[i]));
      END;
      x := BDD.Not(x);
(*      Debug.Out("x: " & BDD.Format(x));
      Debug.Out("y: " & BDD.Format(y));
*)
      Debug.Out("x=y: " & Fmt.Bool(x = y));
    END;
    RTProcess.Exit(0);
  END Test;

BEGIN 
  (* these tweaks have a large impact on program performance *)
  RTCollectorSRC.incremental := FALSE; 
  RTCollectorSRC.generational := FALSE; 
  RTCollectorSRC.gcRatio := 0.2;
  Test()
END BDDTest.
