MODULE RandomSubtractiveFibo1;
(*Gnu CopyLefted.*)
(*
Abstract:
Pseudo-random number generator by Warren D. Smith.
*)

IMPORT LongRealBasic AS R,
       RandomBasic;
IMPORT RandomRep;

<*UNUSED*> CONST Module = "RandomImprovedMcGill.";
(*==========================*)

CONST
  asf1 = 97;
  bsf1 = 34;

(*------------------*)
REVEAL T = TPublic BRANDED OBJECT
    isf1 := asf1;
    jsf1 := bsf1;
    arrsf1 : ARRAY [0..asf1-1] OF R.T; (* initialize to rands in [0,1) not all with LS Bit=0*)
  OVERRIDES
    init:=Init;
    engine:=Engine;
  END;

PROCEDURE Init(SELF:T;initrng:RandomBasic.T):T=
  VAR
    BEGIN
    FOR i:=asf1-1 TO 0 BY -1 DO
      SELF.arrsf1[i] := initrng.generateReal();
    END;
    RETURN SELF;
  END Init;

(* Generates a new random real in [0,1): *)
PROCEDURE Engine(SELF:T):R.T=
  VAR
    x : R.T;
  BEGIN
    DEC(SELF.isf1);
    DEC(SELF.jsf1);
    IF SELF.isf1<0 THEN
      SELF.isf1 := asf1-1; (* wraparound *)
    ELSIF SELF.jsf1<0 THEN
      SELF.jsf1 := asf1-1; (* wraparound *)
    END;
    x := SELF.arrsf1[SELF.isf1] - SELF.arrsf1[SELF.jsf1];
    IF x<R.Zero THEN x := x+R.One; END; (* subtraction mod 1 *)
    SELF.arrsf1[SELF.isf1] := x;
    RETURN x;
  END Engine;

(*==========================*)
BEGIN
END RandomSubtractiveFibo1.
