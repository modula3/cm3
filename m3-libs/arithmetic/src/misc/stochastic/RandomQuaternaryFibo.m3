MODULE RandomQuaternaryFibo;
(*Gnu CopyLefted.*)
(*
Abstract:
Pseudo-random number generator by Warren D. Smith.
*)

IMPORT RandomBasic,
       Word;
IMPORT RandomRep;

<*UNUSED*> CONST Module = "RandomQuaternaryFibo.";
(*==========================*)

CONST
  asf3 = 89;
  bsf3 = 57;
  csf3 = 14;
  dsf3 = 5;   (* x^89 + x^57 + x^14 + x^5 + 1 primitive mod 2 *)

(*------------------*)
REVEAL T = TPublic BRANDED OBJECT
    isf3 := asf3;
    jsf3 := bsf3;
    ksf3 := csf3;
    msf3 := dsf3;
    arrsf3 : ARRAY [0..asf3-1] OF Word.T; (* initialize to random words, not all even *)
  OVERRIDES
    init:=Init;
    engine:=Engine;
  END;

PROCEDURE Init(SELF:T;initrng:RandomBasic.T):T=
  VAR
    BEGIN
    FOR i:=asf3-1 TO 0 BY -1 DO
      SELF.arrsf3[i] := initrng.generateWord();
    END;
    SELF.arrsf3[0] := Word.Or(initrng.generateWord(), 2_1);
    RETURN SELF;
  END Init;

(** Generates a new random Word.T; period at least 2^asf3 - 1;
 * uses both XOR and [- mod 2^wordsize] in the recurrence, hence may
 * avoid some of the known problems with each of these operations alone. *)
PROCEDURE Engine(SELF:T):Word.T=
  VAR
    x : Word.T;
  BEGIN
    DEC(SELF.isf3);
    DEC(SELF.jsf3);
    DEC(SELF.ksf3);
    DEC(SELF.msf3);
    IF SELF.isf3<0 THEN
      SELF.isf3 := asf3-1; (* wraparound *)
    ELSIF SELF.jsf3<0 THEN
      SELF.jsf3 := asf3-1; (* wraparound *)
    ELSIF SELF.ksf3<0 THEN
      SELF.ksf3 := asf3-1; (* wraparound *)
    ELSIF SELF.msf3<0 THEN
      SELF.msf3 := asf3-1; (* wraparound *)
    END;
    x := Word.Minus( SELF.arrsf3[SELF.msf3], Word.Xor( SELF.arrsf3[SELF.ksf3],
                   Word.Minus( SELF.arrsf3[SELF.isf3], SELF.arrsf3[SELF.jsf3] ) ) );
    SELF.arrsf3[SELF.isf3] := x;
    RETURN x;
  END Engine;

(*==========================*)
BEGIN
END RandomQuaternaryFibo.
