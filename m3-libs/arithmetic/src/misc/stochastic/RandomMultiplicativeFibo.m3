MODULE RandomMultiplicativeFibo;
(*Gnu CopyLefted.*)
(*
Abstract:
Pseudo-random number generator by Warren D. Smith.
*)

IMPORT RandomBasic,
       Word;
IMPORT RandomRep;

<*UNUSED*> CONST Module = "RandomMultiplicativeFibo.";
(*==========================*)

(*------------------*)
CONST
  amf1 = 98;
  bmf1 = 27;

REVEAL T = TPublic BRANDED OBJECT
    imf1 := amf1;
    jmf1 := bmf1;
    arrmf1 : ARRAY [0..amf1-1] OF Word.T; (* initialize to random odd words *)
  OVERRIDES
    init:=Init;
    engine:=Engine;
  END;

PROCEDURE Init(SELF:T;initrng:RandomBasic.T):T=
  VAR
    BEGIN
    FOR i:=amf1-1 TO 0 BY -1 DO
      SELF.arrmf1[i] := Word.Or(initrng.generateWord(), 2_1);
    END;
    RETURN SELF;
  END Init;

(* Generates a new random word: *)
PROCEDURE Engine(SELF:T):Word.T=
  BEGIN
    DEC(SELF.imf1);
    DEC(SELF.jmf1);
    IF SELF.imf1<0 THEN
      SELF.imf1 := amf1-1; (* wraparound *)
    ELSIF SELF.jmf1<0 THEN
      SELF.jmf1 := amf1-1; (* wraparound *)
    END;
    SELF.arrmf1[SELF.imf1] := Word.Times(SELF.arrmf1[SELF.imf1], SELF.arrmf1[SELF.jmf1]);
    RETURN SELF.arrmf1[SELF.imf1];
  END Engine;

(*==========================*)
BEGIN
END RandomMultiplicativeFibo.
