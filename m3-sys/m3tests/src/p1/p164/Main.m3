(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT Test, Thread (*, Wr, Stdio, Fmt*);

TYPE
  Array    = ARRAY [1 .. 10] OF RECORD data: INTEGER; lock: MUTEX; END;
  RefArray = REF Array;
  ScanProc = PROCEDURE(r: RefArray);

(*
VAR PrintLock := NEW (MUTEX);

PROCEDURE Prant(t: TEXT; n: INTEGER);
  BEGIN
    LOCK PrintLock DO
      Wr.PutText (Stdio.stdout, t & ", " & Fmt.Int (n));
      Wr.Flush(Stdio.stdout);
    END;
  END Prant;
*)

PROCEDURE Create(): RefArray =
  VAR result := NEW (RefArray);
  BEGIN
    FOR i := FIRST (result^) TO LAST (result^) DO
      result[i].lock := NEW (MUTEX);
      result[i].data := 0;
    END;
    RETURN (result);
  END Create;

PROCEDURE ScanUp (r: RefArray) =
  VAR t: INTEGER;
  BEGIN
    FOR h := 1 TO 100 DO
      Thread.Acquire(r[1].lock);
      FOR i := 1 TO 9 DO
        Thread.Acquire(r[i + 1].lock);
        t := r[i].data;
        FOR j := 0 TO 100 DO END;
        r[i].data := t + 1;
        Thread.Release(r[i].lock);
      END;
      r[10].data := r[10].data + 1;
      Thread.Release(r[10].lock);
    END;
  END ScanUp;

PROCEDURE ScanDown(r: RefArray) =
  VAR t: INTEGER;
  BEGIN
    FOR h := 1 TO 98 DO
      FOR i := 10 TO 1 BY -1 DO
        LOCK r[i].lock DO
          t := r[i].data;
          FOR j := 0 TO 100 DO END;
          r[i].data := t + 1;
        END;
      END;
    END
  END ScanDown;

TYPE
  GobbledyGook = RECORD ss: ScanProc; rr: RefArray END;
  RefGobbledyGook = REF GobbledyGook;

TYPE
  MyClosure = Thread.Closure OBJECT
                boiler: REFANY;
              OVERRIDES
                apply := DoForkScan
              END;

PROCEDURE ForkScan(s: ScanProc; r: RefArray): Thread.T =
  VAR temp := NEW (RefGobbledyGook);  schlemp: REFANY;  result: Thread.T;
  BEGIN
    temp^.rr := r;
    temp^.ss := s;
    schlemp := temp;
    result := Thread.Fork (NEW (MyClosure, boiler := schlemp));
    RETURN (result);
  END ForkScan;

PROCEDURE DoForkScan (cl: MyClosure): REFANY =
  BEGIN
    TYPECASE cl.boiler OF
    | NULL                    => Test.check (FALSE);
    | RefGobbledyGook (plate) => plate.ss(plate.rr);
    ELSE                         Test.check (FALSE);
    END;
    RETURN NIL;
  END DoForkScan;

VAR
  ra: RefArray;
  s1, s2, s3, s4: Thread.T;
  junk: REFANY;

BEGIN
  ra := Create();
  s1 := ForkScan(ScanUp, ra);
  s2 := ForkScan(ScanDown, ra);
  s3 := ForkScan(ScanUp, ra);
  s4 := ForkScan(ScanDown, ra);
  junk := Thread.Join(s1);
  junk := Thread.Join(s2);
  junk := Thread.Join(s3);
  junk := Thread.Join(s4);
  FOR i := 1 TO 10 DO Test.checkI (ra[i].data, 396); END;
  Test.done ();
END Main.
