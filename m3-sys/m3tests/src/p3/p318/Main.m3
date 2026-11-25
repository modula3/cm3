(* Loads of traced references during an incremental collection, from each
   kind of storage the front end decides read barriers for: a constant, a
   local (scalar, array, record), a global (scalar, array, record), the
   heap (dereference, object field, open array), and VAR and open-array
   parameters.  The barrier (RTHooks.CheckLoadTracedRef) is needed for a
   reference loaded from a global, heap or indirect location, not from a
   local or a constant.  Each load must yield the right object. *)

MODULE Main;
IMPORT RTCollector, RTCollectorSRC, Test, Text;

TYPE
  R = REF INTEGER;
  Rec = RECORD a: INTEGER;  r: R;  t: TEXT END;
  Obj = OBJECT r: R END;

CONST
  Names = ARRAY [0..2] OF TEXT {"zero", "one", "two"};
  CRec  = Rec {7, NIL, "seven"};
  Lengths = ARRAY [0..2] OF INTEGER {4, 3, 3};

VAR
  gR  : R;
  gArr: ARRAY [0..2] OF R;
  gRec: Rec;

PROCEDURE NewR (i: INTEGER): R =
  VAR r := NEW (R);
  BEGIN
    r^ := i;
    RETURN r;
  END NewR;

PROCEDURE Churn () =
  (* Allocate enough that the collector has gray objects to scan. *)
  VAR x: R;
  BEGIN
    FOR i := 1 TO 2000 DO x := NewR (i) END;
    EVAL x;
  END Churn;

PROCEDURE FromVar (VAR v: R;  i: INTEGER) =
  BEGIN
    Test.checkI (v^, i);
  END FromVar;

PROCEDURE FromOpen (READONLY a: ARRAY OF R;  i: INTEGER) =
  BEGIN
    Test.checkI (a[i]^, i);
  END FromOpen;

PROCEDURE Round (n: INTEGER) =
  VAR
    lR  : R;
    lArr: ARRAY [0..2] OF R;
    lRec: Rec;
    heap := NEW (REF ARRAY OF R, 3);
    hRec := NEW (REF Rec);
    obj  := NEW (Obj);
    k    := n MOD 3;
  BEGIN
    gR := NewR (n);
    lR := NewR (n + 1);
    FOR i := 0 TO 2 DO
      gArr[i] := NewR (i);  lArr[i] := NewR (i);  heap[i] := NewR (i);
    END;
    gRec.r := NewR (n + 2);  lRec.r := NewR (n + 3);  hRec.r := NewR (n + 4);
    obj.r := NewR (n + 5);

    RTCollectorSRC.StartCollection ();
    Churn ();

    Test.checkI (Text.Length (Names[k]), Lengths[k]);
    Test.checkI (Text.Length (CRec.t), 5);
    Test.checkI (gR^, n);
    Test.checkI (lR^, n + 1);
    Test.checkI (gArr[k]^, k);
    Test.checkI (lArr[k]^, k);
    Test.checkI (heap[k]^, k);
    Test.checkI (gRec.r^, n + 2);
    Test.checkI (lRec.r^, n + 3);
    Test.checkI (hRec.r^, n + 4);
    Test.checkI (obj.r^, n + 5);
    FromVar (lArr[k], k);
    FromVar (gArr[k], k);
    FromOpen (heap^, k);
    FromOpen (lArr, k);

    RTCollectorSRC.FinishCollection ();
  END Round;

BEGIN
  RTCollectorSRC.incremental := TRUE;
  FOR n := 0 TO 20 DO Round (n) END;
  RTCollector.Collect ();
  Test.done ();
END Main.
