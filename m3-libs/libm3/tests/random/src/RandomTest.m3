(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Created by Jorge Stolfi                                     *)
(* Last modified on Mon Apr 18 18:21:34 PDT 1994 by detlefs    *)
(*      modified on Tue Jun  1 00:13:05 PDT 1993 by swart      *)
(*      modified on Tue May  8 13:54:08 PDT 1990 by mcjones    *)
(*      modified on Mon Sep 11  1:23:21 PDT 1989 by stolfi     *)
(*      modified on Wed Jan  1 12:47:22 1986 by mbrown         *)

MODULE RandomTest EXPORTS Main;

IMPORT Random, RandomPerm, Wr, Stat, Params, Fmt, RealFloat, LongFloat, Text,
       Word, FloatMode, Thread, AutoFlushWr, Stdio, RefSeq;

FROM Stdio IMPORT stderr;

<*FATAL Wr.Failure, FloatMode.Failure, FloatMode.Trap, Thread.Alerted*>

CONST 
  NPrint = 100;
  NStat = 10000;

VAR r: Random.T; stdout := NEW(AutoFlushWr.T).init(Stdio.stdout);

PROCEDURE Main () =
  BEGIN
    r := NEW(Random.Default).init();
    FOR p := 1 TO Params.Count - 1 DO
      IF Text.Equal(Params.Get(p), "-all") THEN
        TestSeedGenerator();
        TestInteger();
        TestCardinal();
        TestSubrange();
        TestReal();
        TestLongReal();
        TestPerm();
        TestMod();

      ELSIF Text.Equal(Params.Get(p), "-fixed") THEN
        r := NEW(Random.Default).init(fixed := TRUE);

      ELSIF Text.Equal(Params.Get(p), "-random") THEN
        r := NEW(Random.Default).init(fixed := FALSE);

      ELSIF Text.Equal(Params.Get(p), "-seed") THEN
        TestSeedGenerator();

      ELSIF Text.Equal(Params.Get(p), "-integer") THEN
        TestInteger();

      ELSIF Text.Equal(Params.Get(p), "-cardinal") THEN
        TestCardinal();

      ELSIF Text.Equal(Params.Get(p), "-subrange") THEN
        TestSubrange();

      ELSIF Text.Equal(Params.Get(p), "-real") THEN
        TestReal();

      ELSIF Text.Equal(Params.Get(p), "-longReal") THEN
        TestLongReal();

      ELSIF Text.Equal(Params.Get(p), "-perm") THEN
        TestPerm();

      ELSIF Text.Equal(Params.Get(p), "-mod") THEN
        TestMod();

      ELSE
        Wr.PutText(stderr,
          "Usage: " & Params.Get(0)
            & "[-all -fixed -random -seed -integer -cardinal -subrange -real -longReal -perm]\n");
        EXIT;
      END;

    END;
    Wr.PutText(stdout, "\n\nDone.\n\n");
    Wr.Close(stdout);
  END Main;

PROCEDURE TestSeedGenerator () =
  CONST NBits = BITSIZE(INTEGER);
  VAR
    int: INTEGER;
    st : Stat.T;
    bst: ARRAY [0 .. NBits - 1] OF Stat.T;
  BEGIN
    Wr.PutText(stdout, "\n----------------------------------------");
    Wr.PutText(
      stdout, "\nTesting: r := NEW(Random.Default).init().integer()\n");
    Stat.Init(st);
    FOR j := 0 TO NBits - 1 DO Stat.Init(bst[j]) END;
    FOR i := 1 TO NPrint DO
      int := NEW(Random.Default).init().integer();
      Wr.PutText(stdout, Fmt.Int(int) & "\n");
    END;
    FOR i := 1 TO NStat DO
      int := NEW(Random.Default).init().integer();
      Stat.Accum(st, FLOAT(int));
      FOR j := 0 TO NBits - 1 DO
        Stat.Accum(bst[j], FLOAT(Word.And(int, 1)));
        int := Word.RightShift(int, 1)
      END;
    END;
    Wr.PutText(stdout, "\n\nValue statistics:\n");
    Stat.Print(stdout, st);
    Wr.PutText(stdout, "\n\nBit statistics:\n");
    FOR j := NBits - 1 TO 0 BY -1 DO
      Wr.PutText(stdout, "\nBit " & Fmt.Int(j) & ": ");
      Stat.Print(stdout, bst[j]);
    END
  END TestSeedGenerator;

PROCEDURE TestInteger () =
  CONST NBits = BITSIZE(INTEGER);
  VAR
    int: INTEGER;
    st       : Stat.T;
    bst      : ARRAY [0 .. NBits - 1] OF Stat.T;
  BEGIN
    Wr.PutText(stdout, "\n----------------------------------------");
    Wr.PutText(stdout, "\nTesting: r.integer()\n");
    Stat.Init(st);
    FOR j := 0 TO NBits - 1 DO Stat.Init(bst[j]) END;
    FOR i := 1 TO NPrint DO
      int := r.integer();
      Wr.PutText(stdout, Fmt.Int(int) & "\n");
    END;
    FOR i := 1 TO NStat DO
      int := r.integer();
      Stat.Accum(st, FLOAT(int));
      FOR j := 0 TO NBits - 1 DO
        Stat.Accum(bst[j], FLOAT(Word.And(int, 1)));
        int := Word.RightShift(int, 1)
      END;
    END;
    Wr.PutText(stdout, "\n\nValue statistics:\n");
    Stat.Print(stdout, st);
    Wr.PutText(stdout, "\n\nBit statistics:\n");
    FOR j := NBits - 1 TO 0 BY -1 DO
      Wr.PutText(stdout, "\nBit " & Fmt.Int(j) & ": ");
      Stat.Print(stdout, bst[j]);
    END;
  END TestInteger;

PROCEDURE TestCardinal() =
  CONST NBits = BITSIZE(INTEGER) - 1;
  VAR int: INTEGER; card: CARDINAL;
    st: Stat.T;
    bst: ARRAY [0..NBits-1] OF Stat.T;
  BEGIN
    Wr.PutText(stdout, "\n----------------------------------------");
    Wr.PutText(stdout, "\nTesting: r.integer(0)\n");
    Stat.Init(st);
    FOR j := 0 TO NBits-1 DO Stat.Init(bst[j]) END;
    FOR i := 1 TO NPrint DO
      card := r.integer(0);
      Wr.PutText(stdout, Fmt.Int(card) & "\n");
    END;
    FOR i := 1 TO NStat DO
      card := r.integer(0);
      Stat.Accum(st, FLOAT(card));
      int := card;
      FOR j := 0 TO NBits-1 DO
        Stat.Accum(bst[j], FLOAT(Word.And(int, 1)));
        int := Word.RightShift(int, 1)
      END;
    END;
    Wr.PutText(stdout, "\n\nValue statistics:\n");
    Stat.Print(stdout, st);
    Wr.PutText(stdout, "\n\nBit statistics:\n");
    FOR j := NBits-1 TO 0 BY -1 DO
      Wr.PutText(stdout, "\nBit " & Fmt.Int(j) & ": ");
      Stat.Print(stdout, bst[j]);
    END
  END TestCardinal;

PROCEDURE TestSubrange () =
  CONST
    NBits = BITSIZE(INTEGER) - 1;
    Lo    = 1024;
    Hi    = Lo + 5 * BITSIZE(INTEGER);
  VAR
    int, card: INTEGER;
    st       : Stat.T;
    bst      : ARRAY [0 .. NBits - 1] OF Stat.T;
  BEGIN
    Wr.PutText(stdout, "\n----------------------------------------");
    Wr.PutText(stdout, "\nTesting: r.integer(" & Fmt.Int(Lo) & ", "
                           & Fmt.Int(Hi) & ")\n");
    Stat.Init(st);
    FOR j := 0 TO NBits - 1 DO Stat.Init(bst[j]) END;
    FOR i := 1 TO NPrint DO
      card := r.integer(Lo, Hi);
      Wr.PutText(stdout, Fmt.Int(card) & "\n");
    END;
    FOR i := 1 TO NStat DO
      card := r.integer(Lo, Hi);
      Stat.Accum(st, FLOAT(card));
      int := card;
      FOR j := 0 TO NBits - 1 DO
        Stat.Accum(bst[j], FLOAT(Word.And(int, 1)));
        int := Word.RightShift(int, 1)
      END;
    END;
    Wr.PutText(stdout, "\n\nValue statistics:\n");
    Stat.Print(stdout, st);
    Wr.PutText(stdout, "\n\nBit statistics:\n");
    FOR j := NBits - 1 TO 0 BY -1 DO
      Wr.PutText(stdout, "\nBit " & Fmt.Int(j) & ": ");
      Stat.Print(stdout, bst[j]);
    END
  END TestSubrange;

PROCEDURE TestReal () =
  CONST NBits = BITSIZE(REAL);
  VAR
    real: REAL;
    st  : Stat.T;
    bst : ARRAY [0 .. NBits - 1] OF Stat.T;
  BEGIN
    Wr.PutText(stdout, "\n----------------------------------------");
    Wr.PutText(stdout, "\nTesting: r.real()\n");
    Stat.Init(st);
    FOR j := 0 TO NBits - 1 DO Stat.Init(bst[j]) END;
    FOR i := 1 TO NPrint DO
      real := r.real();
      Wr.PutText(stdout, Fmt.Real(real) & "\n");
    END;
    FOR i := 1 TO NStat DO
      real := r.real();
      Stat.Accum(st, real);
      FOR j := 0 TO NBits - 1 DO
        real := RealFloat.Scalb(real, 1);
        Stat.Accum(bst[j], FLOAT(TRUNC(real)));
        real := real - FLOAT(TRUNC(real));
      END;
    END;
    Wr.PutText(stdout, "\n\nValue statistics:\n");
    Stat.Print(stdout, st);
    Wr.PutText(stdout, "\n\nBit statistics:\n");
    FOR j := NBits - 1 TO 0 BY -1 DO
      Wr.PutText(stdout, "\nBit " & Fmt.Int(j) & ":");
      Stat.Print(stdout, bst[j]);
    END
  END TestReal;

PROCEDURE TestLongReal() =
  CONST 
    NBits =  BITSIZE(LONGREAL);
  VAR 
    long: LONGREAL;
    st: Stat.T;
    bst: ARRAY [0..NBits-1] OF Stat.T;
  BEGIN
    Wr.PutText(stdout, "\n----------------------------------------");
    Wr.PutText(stdout, "\nTesting: r.longReal()\n");
    Stat.Init(st);
    FOR j := 0 TO NBits-1 DO Stat.Init(bst[j]) END;
    FOR i := 1 TO NPrint DO
      long := r.longreal();
      Wr.PutText(stdout, Fmt.LongReal(long) & "\n");
    END;
    FOR i := 1 TO NStat DO
      long := r.longreal();
      Stat.Accum(st, FLOAT(long));
      FOR j := 0 TO NBits - 1 DO
        long := LongFloat.Scalb(long, 1);
        Stat.Accum(bst[j], FLOAT(TRUNC(long), REAL));
        long := long - FLOAT(TRUNC(long), LONGREAL);
      END;
    END;
    Wr.PutText(stdout, "\n\nValue statistics:\n");
    Stat.Print(stdout, st);
    Wr.PutText(stdout, "\n\nBit statistics:\n");
    FOR j := 0 TO NBits - 1 DO
      Wr.PutText(stdout, "\nBit " & Fmt.Int(j) & ": ");
      Stat.Print(stdout, bst[j]);
    END
  END TestLongReal;

PROCEDURE TestPerm () =
  CONST MaxSize = 21;
  VAR
    size, ixCpy: CARDINAL;
    perm, inv  : REF ARRAY OF CARDINAL;
    gen, cpy   : RandomPerm.T;
    elst       : ARRAY [0 .. MaxSize - 1] OF Stat.T;
  BEGIN
    Wr.PutText(stdout, "\n----------------------------------------");
    Wr.PutText(
      stdout, "\nTesting: RandomPerm.New(NIL), RandomPerm.Fill(NIL)\n");

    (* Basic tests and sample printout: *)
    inv := NEW(REF ARRAY OF CARDINAL, MaxSize);
    FOR i := 1 TO NPrint DO
      size := r.integer(0, MaxSize);
      CASE i MOD 4 OF
      | 0, 1 =>
          (* Test RandomPerm.Fill *)
          perm := NEW(REF ARRAY OF CARDINAL, size);
          RandomPerm.Fill(perm^, r);
      | 2, 3 =>
          (* Test RandomPerm.New, RandomPerm.Next, etc *)
          IF (i MOD 4) = 2 THEN
            gen := NEW(RandomPerm.LowQuality).init(size, r);
          ELSE
            gen := NEW(RandomPerm.HighQuality).init(size, r);
          END;
          <*ASSERT(gen.size() = size)*>
          ixCpy := r.integer(0, size);
          perm := NEW(REF ARRAY OF CARDINAL, size);
          FOR j := 0 TO LAST(perm^) DO perm^[j] := LAST(CARDINAL); END;
          FOR j := 0 TO size - 1 DO
            IF (j = ixCpy) THEN
              cpy := gen.copy();
              <*ASSERT(cpy.size() = size)*>
            END;
            <*ASSERT j = gen.index()*>
            perm^[j] := gen.next();
            IF j >= ixCpy THEN
              <*ASSERT j = cpy.index()*>
              <*ASSERT perm^[j] = cpy.next()*>
            END;
          END;
          IF ixCpy = size THEN
            cpy := gen.copy();
            <*ASSERT cpy.size() = size*>
          END;
          <*ASSERT gen.index() = size*>
          <*ASSERT cpy.index() = size*>
          TRY
            EVAL gen.next();
            <*ASSERT FALSE*>
          EXCEPT
          | RandomPerm.Exhausted => (* Ok *)
          END;
          FOR j := 0 TO size - 1 DO
            <*ASSERT j = gen.index() *>
            <*ASSERT perm^[j] = gen.next()*>
          END;
      END;
      (* Check if perm^ is indeed a permutation of [0..size-1]: *)
      FOR j := 0 TO MaxSize - 1 DO inv^[j] := LAST(CARDINAL) END;
      Wr.PutText(stdout, "(");
      FOR j := 0 TO size - 1 DO
        WITH k = perm^[j] DO
          <*ASSERT (k >= 0 ) AND (k < size)*>
          Wr.PutText(stdout, " " & Fmt.Int(k));
          <*ASSERT inv^[k] = LAST(CARDINAL) *>
          inv^[k] := j;
        END;
      END;
      Wr.PutText(stdout, " )\n");
    END;

    (* Statistics *)
    size := MaxSize;
    perm := NEW(REF ARRAY OF CARDINAL, MaxSize);
    FOR q := 0 TO 1 DO
      FOR j := 0 TO MaxSize - 1 DO Stat.Init(elst[j]) END;
      FOR i := 1 TO NStat DO
        (* Fill perm^ with new permutation: *)
        IF q = 0 THEN
          gen := NEW(RandomPerm.LowQuality).init(size, r);
        ELSE
          gen := NEW(RandomPerm.HighQuality).init(size, r);
        END;
        <*ASSERT(gen.size() = size)*>
        FOR j := 0 TO size - 1 DO
          <*ASSERT(gen.index() = j) *>
          perm^[j] := gen.next();
        END;
        <*ASSERT (gen.index() = size)*>

        (* Collect statistics on elements: *)
        FOR j := 0 TO MaxSize - 1 DO inv^[j] := LAST(CARDINAL) END;
        FOR j := 0 TO size - 1 DO
          WITH k = perm^[j] DO
            Stat.Accum(elst[j], FLOAT(k));
            <*ASSERT(inv^[k] = LAST(CARDINAL))*>
            inv^[k] := j;
          END;
        END;
      END;

      Wr.PutText(
        stdout, "\n\nElement statistics (" & Fmt.Int(NStat) & " trials");
      CASE q OF
      | 0 => Wr.PutText(stdout, ", LowQuality");
      | 1 => Wr.PutText(stdout, ", HighQuality");
      END;
      Wr.PutText(stdout, ")\n");
      FOR j := 0 TO MaxSize - 1 DO
        Wr.PutText(stdout, "\nElement " & Fmt.Int(j) & ": ");
        Stat.Print(stdout, elst[j]);
      END
    END;
  END TestPerm;

PROCEDURE TestMod() =
  BEGIN
    Wr.PutText(stdout, "\n\n");
    FOR i := 2 TO 10 DO
      TestModWork(i)
    END (* FOR *);
  END TestMod;

CONST seqSize = 20;
      reps = 50;
TYPE IntArr = REF ARRAY OF INTEGER;

PROCEDURE TestModWork(n: CARDINAL) = 
  VAR r: Random.T;
      seqs := NEW(RefSeq.T).init();
      seq: IntArr;
      min, max, sum: INTEGER;
  BEGIN
    FOR k := 0 TO reps-1 DO
      r := NEW(Random.Default).init();
      seq := NEW(IntArr, seqSize);
      FOR j := 0 TO seqSize-1 DO
        seq[j] := r.integer(0, n-1);
        IF k = 0 AND j = 0 THEN
          min := seq[j]; max := seq[j]; sum := seq[j]
        ELSE
          IF seq[j] < min THEN min := seq[j] END (* IF *);
          IF seq[j] > max THEN max := seq[j] END (* IF *);
          sum := sum + seq[j]
        END (* IF *)
      END (* FOR *);
      VAR i := 0; BEGIN
	LOOP
	  IF i = seqs.size() THEN EXIT END (* IF *);
	  VAR seq2: IntArr := seqs.get(i); k2 := 0; BEGIN
	    WHILE k2 < seqSize AND seq[k2] = seq2[k2] DO INC(k2) END (* WHILE *);
	    IF k2 = seqSize THEN
	      EXIT
	    ELSE
	      INC(i)
	    END (* IF *);
	  END (* BEGIN *)
	END (* LOOP *);
	IF i = seqs.size() THEN
	  seqs.addhi(seq)
	END (* IF *)
      END (* BEGIN *)
    END (* FOR *);
    Wr.PutText(stdout,
               "  " & Fmt.Int(reps) & " runs yield " &
               Fmt.Int(seqs.size()) & " different sequences MOD " &
               Fmt.Int(n) & "; min = " & Fmt.Int(min) & ", max = " &
               Fmt.Int(max) & ", avg = " &
               Fmt.Real(FLOAT(sum)/FLOAT(reps*seqSize)) & ".\n")
  END TestModWork;

BEGIN
  Main()
END RandomTest.
