MODULE RandomSubtractiveFibo2;
(*Gnu CopyLefted.*)
(*
Abstract:
Pseudo-random number generator by Warren D. Smith.
*)

IMPORT RandomBasic,
       Word;
IMPORT RandomRep;

<*UNUSED*> CONST Module = "RandomSubtractiveFibo2.";
(*==========================*)

(*------------------*)
CONST
  asf2 = 95;
  bsf2 = 17;
(*
  TwoTo32Minus5 = 16_fffffffb; (* prime *)
  TwoTo32Minus1 = 16_ffffffff;
*)

REVEAL T = TPublic BRANDED OBJECT
    isf2 := asf2;
    jsf2 := bsf2;
    arrsf2 : ARRAY [0..asf2-1] OF INTEGER;
         (* initialize to random Word.Ts mod TwoTo32Minus5 *)
  OVERRIDES
    init:=Init;
    engine:=Engine;
  END;

PROCEDURE Init(SELF:T;initrng:RandomBasic.T):T=
  VAR
    BEGIN
    FOR i:=asf2-1 TO 0 BY -1 DO
      SELF.arrsf2[i] := initrng.generateWord();
    END;
    SELF.arrsf2[0] := Word.Or(initrng.generateWord(), 2_1);
    RETURN SELF;
  END Init;

(* Generates a new random word, mod 2^32 - 5 (a prime): *)
PROCEDURE Engine(SELF:T):Word.T=
  VAR
    x,y : Word.T;
  BEGIN
    <* ASSERT Word.Size = 32 *>
    DEC(SELF.isf2);
    DEC(SELF.jsf2);
    IF SELF.isf2<0 THEN
      SELF.isf2 := asf2-1; (* wraparound *)
    ELSIF SELF.jsf2<0 THEN
      SELF.jsf2 := asf2-1; (* wraparound *)
    END;
    y := SELF.arrsf2[SELF.isf2];
    x := Word.Minus(y, SELF.arrsf2[SELF.jsf2]);
    IF Word.GT(x, y) THEN (* subtraction "wrapped" *)
      x := x+5;
    END;
    SELF.arrsf2[SELF.isf2] := x;
    RETURN x;
  END Engine;

(*==========================*)
BEGIN
END RandomSubtractiveFibo2.
