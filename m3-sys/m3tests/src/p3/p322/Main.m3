(* Block copies of records and arrays holding traced references, from heap,
   global and indirect storage into a procedure's frame, during an
   incremental collection.  A block copy loads every reference in the
   block, so each of them needs the read barrier; without it a frame can
   hold a reference to a gray object, whose fields still refer to white
   ones.  Each case copies right after the flip, follows a second reference
   from the copy into a scalar local, finishes the collection, reuses the
   freed pages, and checks that the scalar still refers to the right
   object. *)

MODULE Main;
IMPORT RTCollector, RTCollectorSRC, Test;

TYPE
  Leaf = REF INTEGER;
  Node = REF RECORD leaf: Leaf END;
  Pair = RECORD a, b: Node END;
  Wrap = RECORD i: INTEGER;  p: Pair END;
  Arr  = ARRAY [0..2] OF Pair;

EXCEPTION E (Pair);

VAR
  gP: Pair;
  gArr: Arr;
  gH: REF ARRAY OF Pair;  (* RAISE E (local^) is unhandled: a separate bug *)

PROCEDURE NewLeaf (i: INTEGER): Leaf =
  VAR r := NEW (Leaf);
  BEGIN
    r^ := i;
    RETURN r;
  END NewLeaf;

PROCEDURE Junk () =
  (* Fill the rest of the current allocation page, so that leaves, nodes
     and containers lie on different pages and pinning one page (by a
     reference from a frame) does not pin the others. *)
  VAR x: Leaf;
  BEGIN
    FOR i := 1 TO 1500 DO x := NewLeaf (0) END;
    EVAL x;
  END Junk;

PROCEDURE NewPair (i: INTEGER): Pair =
  VAR la, lb: Leaf;  a, b: Node;
  BEGIN
    la := NewLeaf (i);      Junk ();
    lb := NewLeaf (i + 1);  Junk ();
    a := NEW (Node, leaf := la);  Junk ();
    b := NEW (Node, leaf := lb);  Junk ();
    RETURN Pair {a, b};
  END NewPair;

PROCEDURE Churn () =
  (* Allocate enough to reuse the pages a collection has freed. *)
  VAR x: Leaf;
  BEGIN
    FOR i := 1 TO 20000 DO x := NewLeaf (-i) END;
    EVAL x;
  END Churn;

PROCEDURE ByValue (p: Pair): Leaf =
  BEGIN
    RETURN p.a.leaf;
  END ByValue;

PROCEDURE ByValueArr (a: Arr;  k: INTEGER): Leaf =
  BEGIN
    RETURN a[k].b.leaf;
  END ByValueArr;

PROCEDURE ByValueOpen (a: ARRAY OF Pair;  k: INTEGER): Leaf =
  BEGIN
    RETURN a[k].a.leaf;
  END ByValueOpen;

PROCEDURE Returned (h: REF Pair): Pair =
  BEGIN
    RETURN h^;
  END Returned;

PROCEDURE Setup (n: INTEGER;  hP: REF Pair;  hArr: REF Arr;
                 hOpen: REF ARRAY OF Pair) =
  (* Build the structures in a frame that is gone by the flip, so that only
     the containers are referenced from the stack. *)
  BEGIN
    hP^ := NewPair (n);
    gP := NewPair (n + 10);
    FOR i := 0 TO 2 DO
      hArr[i] := NewPair (n + 20 + 2 * i);
      gArr[i] := NewPair (n + 30 + 2 * i);
      hOpen[i] := NewPair (n + 40 + 2 * i);
    END;
    Junk ();
  END Setup;

PROCEDURE Round (n: INTEGER) =
  CONST Cases = 10;
  VAR
    hP    := NEW (REF Pair);
    hArr  := NEW (REF Arr);
    hOpen := NEW (REF ARRAY OF Pair, 3);
    lP: Pair;
    lArr: Arr;
    lW: Wrap;
    lCons: ARRAY [0..1] OF Pair;
    w: ARRAY [0..Cases-1] OF Leaf;
    k := n MOD 3;
  BEGIN
    Setup (n, hP, hArr, hOpen);

    RTCollectorSRC.StartCollection ();

    lP := hP^;                 w[0] := lP.a.leaf;
    lP := gP;                  w[1] := lP.b.leaf;
    lArr := hArr^;             w[2] := lArr[k].a.leaf;
    lArr := hOpen^;            w[3] := lArr[k].b.leaf;
    lP := hOpen[k];            w[4] := lP.a.leaf;
    lW := Wrap {n, hP^};       w[5] := lW.p.b.leaf;
    lCons := ARRAY [0..1] OF Pair {gArr[k], hP^};
                               w[6] := lCons[0].b.leaf;
    w[7] := ByValue (hP^);
    w[8] := ByValueArr (hArr^, k);
    gH := hOpen;
    TRY RAISE E (gH[k]) EXCEPT E (p) => w[9] := p.b.leaf END;
    lP := Returned (hP);

    RTCollectorSRC.FinishCollection ();
    Churn ();

    Test.checkI (w[0]^, n);
    Test.checkI (w[1]^, n + 11);
    Test.checkI (w[2]^, n + 20 + 2 * k);
    Test.checkI (w[3]^, n + 41 + 2 * k);
    Test.checkI (w[4]^, n + 40 + 2 * k);
    Test.checkI (w[5]^, n + 1);
    Test.checkI (w[6]^, n + 31 + 2 * k);
    Test.checkI (w[7]^, n);
    Test.checkI (w[8]^, n + 21 + 2 * k);
    Test.checkI (w[9]^, n + 41 + 2 * k);
    Test.checkI (lP.a.leaf^, n);
    Test.checkI (ByValueOpen (hOpen^, k)^, n + 40 + 2 * k);
  END Round;

BEGIN
  RTCollectorSRC.incremental := TRUE;
  FOR n := 0 TO 10 DO Round (n) END;
  RTCollector.Collect ();
  Test.done ();
END Main.
