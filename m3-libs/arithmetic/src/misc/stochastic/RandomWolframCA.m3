MODULE RandomWolframCA;
(*Gnu CopyLefted.*)
(*
Abstract:
Pseudo-random number generator by Warren D. Smith.
*)

IMPORT RandomBasic,
       Word;
IMPORT RandomRep;

<*UNUSED*> CONST Module = "RandomWolframCA.";
(*==========================*)

(*------------------*)
CONST
  wolfnum = 5;
  MSbit = Word.LeftShift(2_1, Word.Size-1);

REVEAL T = RandomBasic.T BRANDED OBJECT
    wolfarr : ARRAY [0 .. wolfnum-1] OF Word.T; (* initialize with random bits *)
(*
  OVERRIDES
    engine:=Engine;
*)
  END;

(*************************************************************
S.Wolfram: Advances Applied Math 7 (1986) 123- had proposed the following
nonlinear "cellular automaton" random number generator:
Consider a 1D circular array of bits B[0..modulus-1].
At the t-th time step, you update according to
         Bnew[i] = Bold[i-1] XOR ( Bold[i] OR Bold[i+1] )
where the subscripts have circular wraparound. (Somehow,
I don't think a 1-line formula involving 3 bits published in 1986
is out of the public domain.) The time-series
B[0] form a random-appearing bit sequence, according to a large
number of empirical tests by Wolfram. Unfortunately you only get
1 bit at a time. An equivalent formula in the bit-complement universe is
         Bnew[i] = Bold[i-1] XOR ( Bold[i] AND Bold[i+1] )
and this also suggests the new idea of replacing the bits B by nonnegative
integers Y mod 2^wordsize and then
         Ynew[i] = Yold[i-1] + ( Yold[i] * Yold[i+1] )
would be the same as Wolfram on its LS bits, but will generate a full word
at a time.
*********************************************************)

PROCEDURE Engine(SELF:T) : BOOLEAN =
  VAR
    origcarry, carry, borrow : BOOLEAN;
    x, a, b : Word.T;
  BEGIN
    borrow    :=  ( Word.And( SELF.wolfarr[0], 2_1 ) # 0 );
    origcarry :=  ( Word.And( SELF.wolfarr[LAST(SELF.wolfarr)], MSbit ) # 0 );
    FOR i:=LAST(SELF.wolfarr) TO FIRST(SELF.wolfarr) BY -1 DO
      x := SELF.wolfarr[i]; (* old word *)
      IF i>0 THEN (* get carry from word below [borrow is from word above] *)
        carry :=  ( Word.And( SELF.wolfarr[i-1], MSbit ) # 0 );
      ELSE
        carry := origcarry;
      END;
      a := Word.RightShift(x,1);
      a := Word.Or( a, Word.LeftShift(ORD(borrow), Word.Size-1) );
      b := Word.LeftShift(x,1);
      b := Word.Or( b, ORD(carry) );
      (* CA update formula -> new word: *)
      SELF.wolfarr[i] := Word.Xor(a, Word.Or(x, b));
      (* get borrow from old word for next time: *)
      borrow :=  ( Word.And( x, 2_1 ) # 0 );
    END;
    RETURN borrow;
  END Engine;

(*==========================*)
BEGIN
END RandomWolframCA.
