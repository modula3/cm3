MODULE RandomCombinedFast;
(*Gnu CopyLefted.*)
(*
Abstract:
Pseudo-random number generator by Warren D. Smith.
*)

IMPORT RandomBasic,
       LongRealBasic AS R,
       LongRealTrans AS RT,
       RandomIteratedSquaring   AS IterSqr,
       RandomSubtractiveFibo1   AS SubFibo1,
       RandomSubtractiveFibo2   AS SubFibo2,
       RandomMultiplicativeFibo AS MulFibo,
       Word;
IMPORT RandomRep;

CONST Module = "RandomCombinedFast.";
(*==========================*)

(*------------------*)
REVEAL T = RandomBasic.T BRANDED OBJECT
    subfibo1:SubFibo1.T;
    subfibo2:SubFibo2.T;
    mulfibo:MulFibo.T;
  OVERRIDES
    generateWord:=GenerateWord;
    generateReal:=GenerateReal;
  END;

(** However, if your need for speed is so great that RandCombinedSlow
will not do, try the routines below, which only combine two of the
5 generators in RandCombinedSlow, selected for high speed and high randomness.
***************************************)
PROCEDURE GenerateWord(SELF:T):Word.T =
  BEGIN
    RETURN Word.Plus( SELF.subfibo2.engine(), SELF.mulfibo.engine() );
  END GenerateWord;

PROCEDURE GenerateReal(SELF:T):R.T=
  VAR
    x : R.T;
  BEGIN
    x := R.Scalb(
         R.Scalb( FLOAT( SELF.mulfibo.engine(), R.T ) , 6-Word.Size )
             + FLOAT( SELF.mulfibo.engine(), R.T ), -Word.Size );
    (** note, those multiplications were really just bit shifts. How
      * do I get the compiler to know that?? *)
    <* ASSERT -RT.Half <= x *>
    <* ASSERT x < 0.52D0 *>
    IF x < R.Zero THEN x := x+R.One; END;
    x := x - SELF.subfibo1.engine();
    IF x < R.Zero THEN x := x+R.One; END;
    <* ASSERT x >= R.Zero *>
    <* ASSERT x < R.One *>
    RETURN x;
  END GenerateReal;

(*** Initializes all random number generators here. Quite slow.
If NonReproducible=TRUE (the default) will use the time as seed.
If FALSE will use a particular fixed seed.
*************************************************************)
PROCEDURE New(fixed : BOOLEAN := FALSE):T=
  VAR
    is:=IterSqr.New(fixed);
    SELF:=NEW(T,subfibo1:=SubFibo1.New(is),
                subfibo2:=SubFibo2.New(is),
                mulfibo :=MulFibo.New(is));
  BEGIN
    (* rev 'em up by 6000 calls to Uni01() *)
    FOR i:=0 TO 6000 DO
      EVAL SELF.generateReal();
    END;
    RETURN SELF;
  END New;

(*
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
*)
(*==========================*)
BEGIN
END RandomCombinedFast.
