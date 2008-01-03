MODULE RandomCombinedFast;
(* Gnu CopyLefted. *)
(*
Abstract:
Pseudo-random number generator by Warren D. Smith.
*)

IMPORT LongRealBasic            AS R,
       LongRealTrans            AS RT,
       RandomIteratedSquaring   AS IterSqr,
       RandomSubtractiveFibo1   AS SubFibo1,
       RandomSubtractiveFibo2   AS SubFibo2,
       RandomMultiplicativeFibo AS MulFibo,
       Word,
       FloatMode;
IMPORT RandomRep;

<* UNUSED *>
CONST
  Module = "RandomCombinedFast.";


REVEAL
  T = TPublic BRANDED OBJECT
        subfibo1: SubFibo1.T;
        subfibo2: SubFibo2.T;
        mulfibo : MulFibo.T;
      OVERRIDES
        init         := Init;
        generateWord := GenerateWord;
        generateReal := GenerateReal;
      END;

(** However, if your need for speed is so great that RandCombinedSlow
will not do, try the routines below, which only combine two of the
5 generators in RandCombinedSlow, selected for high speed and high randomness.
***************************************)
PROCEDURE GenerateWord (SELF: T; ): Word.T =
  BEGIN
    RETURN Word.Plus(SELF.subfibo2.engine(), SELF.mulfibo.engine());
  END GenerateWord;

PROCEDURE GenerateReal (SELF: T; ): R.T =
  <* FATAL FloatMode.Trap *>
  VAR
    x: R.T;
  BEGIN
    x := R.Scalb(R.Scalb(FLOAT(SELF.mulfibo.engine(), R.T), 6 - Word.Size)
                   + FLOAT(SELF.mulfibo.engine(), R.T), -Word.Size);
    (** note, those multiplications were really just bit shifts. How
      * do I get the compiler to know that?? *)
    <* ASSERT -RT.Half <= x *>
    <* ASSERT x < 0.52D0 *>
    IF x < R.Zero THEN x := x + R.One; END;
    x := x - SELF.subfibo1.engine();
    IF x < R.Zero THEN x := x + R.One; END;
    <* ASSERT x >= R.Zero *>
    <* ASSERT x < R.One *>
    RETURN x;
  END GenerateReal;

(*** Initializes all random number generators here. Quite slow.
If fixed=FALSE (the default) will use the time as seed.
If TRUE will use a particular fixed seed.
*************************************************************)
PROCEDURE Init (SELF: T; fixed: BOOLEAN := FALSE; ): T =
  VAR is := NEW(IterSqr.T).init(fixed);
  BEGIN
    SELF.subfibo1 := NEW(SubFibo1.T).init(is);
    SELF.subfibo2 := NEW(SubFibo2.T).init(is);
    SELF.mulfibo := NEW(MulFibo.T).init(is);
    (* rev 'em up by 60 calls to Uni01() *)
    FOR i := 0 TO 60 DO EVAL SELF.generateReal(); END;
    RETURN SELF;
  END Init;


BEGIN
END RandomCombinedFast.
