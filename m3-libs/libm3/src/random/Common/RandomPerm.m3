(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Created September 1989 by Bill Kalsow                       *)
(* Based on RandPerm.mod by Mark Manasse                       *)
(* Last modified on Thu Jan 26 13:53:26 PST 1995 by kalsow     *)
(*      modified on Mon May 31 15:39:17 PDT 1993 by swart      *)
(*      modified on Tue Aug 18 14:51:37 PDT 1992 by mcjones    *)
(*      modified on Mon Jan 29 21:16:45 PST 1990 by stolfi     *)
(*      modified on Thu Nov  2 18:28:00 1989 by muller         *)


MODULE RandomPerm;

IMPORT Word, Random;

REVEAL
  HighQuality = T OBJECT
                METHODS
                  init (n: CARDINAL; r: Random.T): HighQuality;
                END
                  BRANDED OBJECT
                  num, count: CARDINAL := 0;
                  deck: REF ARRAY OF CARDINAL;  (* Current permutation *)
                OVERRIDES
                  init  := HQInit;
                  next  := HQNext;
                  copy  := HQCopy;
                  index := HQIndex;
                  size  := HQSize;
                END;

  LowQuality = T OBJECT
               METHODS
                 init (n: CARDINAL; r: Random.T): LowQuality;
               END
                 BRANDED OBJECT
                 num, count: CARDINAL  := 0;
                 state     : INTEGER;  (* Current state *)
                 mult      : INTEGER;  (* Multiplier *)
                 bits: CARDINAL;  (* CEILING(LOG2(num)), or 0 if num=0. *)
               OVERRIDES
                 init  := LQInit;
                 next  := LQNext;
                 copy  := LQCopy;
                 index := LQIndex;
                 size  := LQSize;
               END;

PROCEDURE HQInit (t: HighQuality; n: CARDINAL; r: Random.T): HighQuality =
  BEGIN
    t.num := n;
    t.count := n;
    t.deck := NEW(REF ARRAY OF CARDINAL, n);
    Fill(t.deck^, r);
    RETURN t
  END HQInit;

PROCEDURE LQInit (t: LowQuality; n: CARDINAL; r: Random.T): LowQuality =
  VAR m, bits: CARDINAL;
  BEGIN
    t.num := n;
    t.count := n;
    IF n = 0 THEN
      t.bits := 0
    ELSE
      m := n - 1;
      bits := 0;
      WHILE m # 0 DO m := m DIV 2; INC(bits) END;
      <*ASSERT (bits <= BITSIZE(INTEGER) - 2) *>
      t.bits := bits;
    END;
    t.state := Word.Plus(Word.Times(r.integer(0), 2), 1);
    t.mult := Word.Plus(Word.Times(r.integer(0), 8), 3);
    IF ((r.integer(0) MOD 2) # 0) THEN INC(t.mult, 2) END;
    RETURN t;
  END LQInit;

PROCEDURE HQNext (t: HighQuality): CARDINAL RAISES {Exhausted} =
  BEGIN
    IF t.count = 0 THEN t.count := t.num; RAISE Exhausted END;
    DEC(t.count);
    RETURN t.deck[t.count];
  END HQNext;

PROCEDURE LQNext (t: LowQuality): CARDINAL RAISES {Exhausted} =
  VAR res: CARDINAL;
  BEGIN
    IF t.count = 0 THEN t.count := t.num; RAISE Exhausted END;
    DEC(t.count);
    REPEAT
      t.state := Word.Times(t.state, t.mult);
      res := Word.Extract(Word.Plus(t.state, 1), 2, t.bits)
    UNTIL res < t.num;
    RETURN res
  END LQNext;

PROCEDURE HQSize (p: HighQuality): CARDINAL =
  BEGIN
    RETURN p.num
  END HQSize;

PROCEDURE HQIndex (p: HighQuality): CARDINAL =
  BEGIN
    RETURN p.num - p.count
  END HQIndex;

PROCEDURE LQSize (p: LowQuality): CARDINAL =
  BEGIN
    RETURN p.num
  END LQSize;

PROCEDURE LQIndex (p: LowQuality): CARDINAL =
  BEGIN
    RETURN p.num - p.count
  END LQIndex;

PROCEDURE HQCopy (p: HighQuality): T =
  BEGIN
    RETURN NEW(HighQuality, num := p.num, count := p.count, deck := p.deck)
  END HQCopy;

PROCEDURE LQCopy (p: LowQuality): T =
  BEGIN
    RETURN NEW(LowQuality, num := p.num, count := p.count,
               state := p.state, mult := p.mult, bits := p.bits)
  END LQCopy;

PROCEDURE Fill (VAR(*OUT*) perm: ARRAY OF CARDINAL;  r: Random.T) =
  VAR j, t: CARDINAL;
  BEGIN
    FOR i := 0 TO LAST(perm) DO perm[i] := i END;
    FOR i := 0 TO LAST(perm) - 1 DO
      j := r.integer (i, LAST(perm));
      IF j # i THEN t := perm[j]; perm[j] := perm[i]; perm[i] := t END
    END
  END Fill;


BEGIN
END RandomPerm.

