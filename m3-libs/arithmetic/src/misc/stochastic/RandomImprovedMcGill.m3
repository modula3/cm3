MODULE RandomImprovedMcGill;
(*Gnu CopyLefted.*)
(*
Abstract:
Pseudo-random number generator by Warren D. Smith.
*)

IMPORT LongRealBasic AS R,
       RandomBasic,
       Word;
IMPORT RandomRep;

<*UNUSED*> CONST Module = "RandomImprovedMcGill.";
(*==========================*)

CONST
  MULTmg = 69069;
  mgSIZE = 103;
  SCALEmg = (FLOAT(mgSIZE, R.T) / 4294967296.0D0);

(*------------------*)
REVEAL T = RandomBasic.TWord BRANDED OBJECT
    MultCongMg : Word.T;  (* initialize to a random odd word *)
    ShiftRegMg : Word.T; (* initialize to a random word with 7ff in LS 11 bits *)
    arrmg : ARRAY [0..mgSIZE-1] OF Word.T; (* initialize to random Word.Ts *)
    ymg : Word.T := 0;
  OVERRIDES
    engine:=Engine;
  END;

PROCEDURE New(initrng:RandomBasic.T):T=
  VAR
    SELF:=NEW(T);
  BEGIN
    FOR i:=mgSIZE-1 TO 0 BY -1 DO
      SELF.arrmg[i] := initrng.generateWord();
    END;
    SELF.MultCongMg := Word.Or(initrng.generateWord(), 2_1);
    SELF.ShiftRegMg := Word.Or(initrng.generateWord(), 16_7ff);
    RETURN SELF;
  END New;

(*********************************************************
 McGill "Super-duper" generator by G. Marsaglia, K. Ananthanarayana
& N. Paul; but with an improvement suggested by Marsaglia after observing that
the unmodified generator failed the "MTUPLE test on low order bits."
That generator was linear and optimized for speed rather than randomness;
hence not to be relied on. It was a combination
of a shift register generator and a linear congruential generator.
Not being confident that Marsaglia's improvement will fix the MTUPLE
test problem, I have added one further improvement to the McGill generator:
I combined it with the Bays-Durham shuffling algorithm. The resulting
generator ought to pass the full Marsaglia test battery and also should
have a larger period.
*********************************************************)
PROCEDURE Engine(SELF:T):Word.T=
  VAR
    r0, r1 : Word.T;
    j : CARDINAL;
  BEGIN
    <* ASSERT Word.Size = 32 *>
    r0 := Word.RightShift(SELF.ShiftRegMg, 15);
    r1 := Word.Xor( SELF.ShiftRegMg, r0 );
    r0 := Word.LeftShift(r1, 17);
    SELF.ShiftRegMg := Word.Xor(r0, r1);

    SELF.MultCongMg := Word.Times(MULTmg, SELF.MultCongMg);
    (** Marsaglia's improvement: we've changed Word.Xor --> Word.Plus: *)
    r1 := Word.Plus(SELF.MultCongMg, SELF.ShiftRegMg);

    (** My improvement: the normal McGill generator would just return r1 here,
     * but I feed it into a Bays-Durham shuffler. *)
    j := FLOOR( FLOAT(ybd, R.T) * SCALEmg );
    SELF.ymg := SELF.arrmg[j];
    SELF.arrmg[j] := r1;
    RETURN SELF.ymg;
  END Engine;

(*How could this work? BaysDurham is called nowhere. (Lemming)*)
CONST
  abd = 101;
VAR
  arrbd : ARRAY [0..abd-1] OF R.T; (* initialize to rands in [0,1) *)
  ybd : R.T := R.Zero;

<* UNUSED *>
(*it's crap, just to save the code*)
PROCEDURE NewBD(initrng:RandomBasic.T):T=
  VAR
    SELF:=NEW(T);
  BEGIN
    FOR i:=abd-1 TO 0 BY -1 DO
      arrbd[i] := initrng.generateReal();
    END;
    RETURN SELF;
  END NewBD;

(* Inputs a random real in [0,1), outputs a "more random" one: *)
<* UNUSED *>
PROCEDURE BaysDurhamShuffler(x : R.T) : R.T =
  VAR
    j : CARDINAL;
  BEGIN
    j := FLOOR( FLOAT(abd, R.T) * ybd );
    ybd := arrbd[j];
    arrbd[j] := x;
    RETURN ybd;
  END BaysDurhamShuffler;

(*==========================*)
BEGIN
END RandomImprovedMcGill.
