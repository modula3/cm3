MODULE RandomCombinedFast;
(*Gnu CopyLefted.*)
(*
Abstract:
Pseudo-random number generator by Warren D. Smith.
*)

IMPORT RandomBasic,
       LongRealBasic AS R,
       LongRealTrans AS RT,
       Word, Tick, TimeStamp;
IMPORT RandomRep;

CONST Module = "RandomCombinedFast.";
(*==========================*)
VAR InitDone:=FALSE;  (*is the big Init done yet?*)

(*---object wrappers---*)

(*------------------*)
REVEAL T = RandomBasic.T BRANDED OBJECT
  OVERRIDES
    engine:=Engine;
  END;

(** However, if your need for speed is so great that RandCombinedSlow
will not do, try the routines below, which only combine two of the
5 generators in RandCombinedSlow, selected for high speed and high randomness.
***************************************)
PROCEDURE FasterRandWord() : Word.T =
  BEGIN
    RETURN Word.Plus( SubtractiveFibo2(), MultiplicativeFibo1() );
  END FasterRandWord;

PROCEDURE FasterUni01() : R.T =
  VAR
    x : R.T;
  BEGIN
    x := R.Scalb(
         R.Scalb( FLOAT( MultiplicativeFibo1(), R.T ) , 6-Word.Size )
             + FLOAT( MultiplicativeFibo1(), R.T ), -Word.Size );
    (** note, those multiplications were really just bit shifts. How
      * do I get the compiler to know that?? *)
    <* ASSERT -0.5D0 <= x *>
    <* ASSERT x < 0.52D0 *>
    IF x < R.Zero THEN x := x+R.One; END;
    x := x - SubtractiveFibo1();
    IF x < R.Zero THEN x := x+R.One; END;
    <* ASSERT x >= R.Zero *>
    <* ASSERT x < R.One *>
    RETURN x;
  END FasterUni01;

CONST
   moduis = 9739.0D0 * 9719.0D0;
       (* = 94653341. Factors are each primes, 3 mod 4. *)
   DefaultSeed1 = 3145981;
   DefaultSeed2 = 2718280;
   DefaultXis = 243213.0D0;
VAR
   xis : R.T;
   seedbitind : INTEGER := 3;
   seed1, seed2 : Word.T;

(** Note: period of the bit sequence this produces is
 * only 23658471. However, that should be adequate for its
 * intended purpose, which is to initialize the state of
 * the other generators to something interesting.
 * (And if the seeds are nonzero, period will
 * generally be 2*Word.Size*23658471.)
 * If seed1 or seed2 are nonzero, then will add a perturbation to output
 * according to the bits of the seed words.
*********************************************)
PROCEDURE IteratedSquaring() : BOOLEAN =
  VAR
    perturb : Word.T;
  BEGIN
    DEC(seedbitind);
    IF seedbitind<0 THEN seedbitind := Word.Size * 2 - 1; END;
    IF seedbitind<Word.Size THEN
      perturb := Word.And(
                 Word.RightShift(seed1, seedbitind), 2_1 );
    ELSE
      perturb := Word.And(
            Word.RightShift(seed2, seedbitind-Word.Size), 2_1 );
    END;

    xis := (xis * xis) MOD moduis;

    RETURN ( (xis < (moduis-R.One)*0.5D0) = (perturb#0) );
  END IteratedSquaring;

(* Generates a longreal, bit by bit, using IteratedSquaring *)
PROCEDURE initlongreal() : R.T =
  VAR
    x : R.T := R.Zero;
  BEGIN
    FOR i:=0 TO 57 DO
      x := 0.5D0 * (x + FLOAT(ORD(IteratedSquaring()), R.T));
    END;
    <* ASSERT R.Zero <= x *>
    <* ASSERT x < R.One *>
    RETURN x;
  END initlongreal;

(* Generates a word, bit by bit, using IteratedSquaring *)
PROCEDURE initword() : Word.T =
  VAR
    x : Word.T := 0;
  BEGIN
    FOR i:=0 TO Word.Size DO
      x := Word.Plus( Word.LeftShift(x,1), ORD(IteratedSquaring()));
    END;
    RETURN x;
  END initword;

(*** Initializes all random number generators here. Quite slow.
If NonReproducible=TRUE (the default) will use the time as seed.
If FALSE will use a particular fixed seed.
*************************************************************)
PROCEDURE Init(NonReproducible : BOOLEAN := TRUE) =
  BEGIN
    (*---HGG 3/23/96: flag so objects don't repeat this proc---*)
    InitDone:=TRUE;
    (*----------------------------------------------------------*)

    IF NonReproducible THEN
      seed1 := TimeStamp.Hash(TimeStamp.New());
      seed2 := Tick.Now();
      xis := ABS( DefaultXis + FLOAT(seed1, R.T)
                      + FLOAT(seed1, R.T) ) MOD moduis;
    ELSE
      seed1 := DefaultSeed1; seed2 := DefaultSeed2; xis := DefaultXis;
    END;
    FOR i:=asf1-1 TO 0 BY -1 DO
      arrsf1[i] := initlongreal();
    END;
    FOR i:=abd-1 TO 0 BY -1 DO
      arrbd[i] := initlongreal();
    END;
    FOR i:=asf2-1 TO 0 BY -1 DO
      arrsf2[i] := initword();
    END;
    arrsf2[0] := Word.Or(initword(), 2_1);
    FOR i:=asf3-1 TO 0 BY -1 DO
      arrsf3[i] := initword();
    END;
    arrsf3[0] := Word.Or(initword(), 2_1);
    FOR i:=amf1-1 TO 0 BY -1 DO
      arrmf1[i] := Word.Or(initword(), 2_1);
    END;
    FOR i:=mgSIZE-1 TO 0 BY -1 DO
      arrmg[i] := initword();
    END;
    FOR i:=wolfnum-1 TO 0 BY -1 DO
      wolfarr[i] := initword();
    END;
    MultCongMg := Word.Or(initword(), 2_1);
    ShiftRegMg := Word.Or(initword(), 16_7ff);

    (* rev 'em up by 6000 calls to Uni01() *)
    FOR i:=0 TO 6000 DO
      EVAL Uni01();
    END;
  END Init;

(*----------------------------------------*)
PROCEDURE Test()=
BEGIN
  (*testing code: *)
(*
  IO.Put( Fmt.LongReal( Uni01() ) & "\n");
*)

  FOR i:=0 TO 10000000 DO
(*
    EVAL ComboGen();
    EVAL FasterGen();
*)
    EVAL SubtractiveFibo1();
    EVAL RT.Sin( FLOAT(i, R.T) );
    EVAL ImprovedMcGill();
    EVAL MultiplicativeFibo1();
    EVAL QuaternaryFibo();
    EVAL WolframCA();
    EVAL SubtractiveFibo2();
    EVAL FasterUni01();
    EVAL Uni01();
    EVAL FasterRandWord();
  END;
END Test;
(*==========================*)
BEGIN
END RandomCombinedFast.
