MODULE RandomNumber02;
(*Gnu CopyLefted.*)
(*
Abstract:
Pseudo-random number generator by Warren D. Smith.

Goal is to combine several random number generators to get at least the
randomness of the strongest of them, but to do it in such a way as
to get nearly the speed of the fastest of them.

NOTE: I use a lot of global variables here to save state,
necessary in modula3 since C's "static" feature does not exist.
Of course these variables are local to the current module.
For that reason, please keep this a separate module from others,
e.g. the derived routines for NormalDeviate, etc. should not be in
same module as these underlying routines for uniform deviates.

Some component generators:
Following G.Marsaglia: A current view of RNGs, pages 3-10 in
Computer Science and Statistics, the interface, Elsevier 1985,
a fair number of my generators are of the FIBO(a,b,%) type, where
a>b>0 are integers such that x^a+x^b+1 is a primitive trinomial
mod 2 and % is a binary operation.

Suitable values of (b,a) are tabulated in D.Knuth: Seminumerical methods,
Addison-Wesley 1981, page 28, which is extracted from larger tables in
N.Zierler & J.Brillhart: Information and Control 13 (1968) 541-554;
14 (1969) 566-569; 15 (1969) 67-69. Some pairs with "a" prime which
go beyond the Knuth table, which you can use if you *really*
want randomness, include:
  (7,127), (15,127), (30,127), (63,127),    (32,521), (158,521),
  (105,607), (147,607), (273,607),     (216,1279), (418,1279)

The binary operation % could be: [- mod 1] for floating point values,
[- mod M] or [* mod M] for some integer M,
or [this NOT recommended; it is same as - only mod 2] XOR.

Then the meta-procedure Fibo(a,b,%) is as follows (it
requires an auxiliary arr : ARRAY [0..a-1], initially filled with
random seed values, and initially ia=a and ja=b:

MetaRandGen() : Type =
  BEGIN
    DEC(i);
    DEC(j);
    IF i<0 THEN
      i := a-1; (* wraparound *)
    ELSIF j<0 THEN
      j := a-1; (* wraparound *)
    END;
    arr[i] := arr[i] % arr[j];
    RETURN arr[i];
  END MetaRandGen;

Period [Marsaglia & Tsay:  Lin. Alg. and its
   Appl. 67, 147-155, 1985]: If % is (- mod 2^wordsize),
then if at least one of the arr[] is odd
the period will be maximal = (2^a - 1) * 2^(wordsize-1) if
the characteristic axa matrix T of the recurrence has full order
J=(2^a - 1) in mod 2 arithmetic (this assured by the trinomial condition)
and 2*J in mod 4 and 4*J in mod 8 arithmetic. That is, if you square
T a times, you get back T in mod 2, but do NOT get back T in mod 4,
arithmetic, and if you square T a+1 times, you do not get back T
in mod 8 arithmetic. In fact, this criterion will work for any
matrix T, not necessarily the fibo(-) one...

If % is [* mod 2^wordsize], which may be implemented in m3 by Word.Times(),
then arr[] must be all odd integers, and by considering discrete logs
we see that the period is maximal = (2^a - 1) * 2^(wordsize-3) if
the F(a,b,+ mod 2^wordsize) has maximal period
(2^a - 1) * 2^(wordsize-1).

Marsaglia's test results for Fibo(a,b,%) generators with
(a,b)=(17,5), (31,13) and (55,24):
 XOR: With %=XOR the fibo fail seven of the tests in Marsaglia's battery.
As Marsaglia says in his conclusion "never use XOR." Despite this,
these generators keep resurfacing, for example the "r250" Fibo(250,147,XOR)
generator of S. Kirkpatrick and E.  Stoll, Journal of Computational
Physics, 40, p. 517 (1981) and W. L. Maier, "A Fast Pseudo Random
Number Generator", Dr. Dobb's Journal #176.
 Subtraction: With %=-, among the tests in Marsaglia's DIEHARD battery
only the "birthday spacings test" (of frequencies of spacings in sorted sets
of deviates) is failed, a symptom both of the linear structure
and also of the specific subtractive structure of these generators.
Note that the Fibo(55,24,-) generator (which fails this test) is to be found
in Knuth's book and also is distributed with the DEC SRC implementation of
modula-3. Also note that postprocessing Fibo(55,24,-) sequence with a
Bays-Durham shuffler, recommended in Press et al. "Numerical Recipes" to
fix suspicious generators, will NOT work to make it now pass this test, since
the birthday spacings test does not depend upon the ordering of the
deviates only their values.
 Multiplication: With %=* mod 2^32, these FIBO gens passed all the tests in
Marsaglia's battery.

Note that the -,+ and XOR gens are "linear" and hence theoretically
bad and will always fail "empty slab tests" see below.

Note that shift register, Fibonacci() generators with +,- or XOR,
and linear congruential generators x <-- a*x+b mod M, AND linear
combinations of such possibly to different moduli, are all
"linear" and thus generate d-tuples of random numbers lying on
AT MOST M^(1/d) hyperplanes in d-space. Consequently,
the nonrandomness of a linear RNG with period M is in principle
detectable by the "largest empty slab" test in .4*logM dimensions after only N,
  N > e^(5/2) * (.4*logM)^2.5 * loglogM,
random numbers have been generated. Hence:
Please do not rely on a linear generator.
If you are going, foolishly, to use a linear congruential generator, though,
you want a "good multiplier" a mod M. (Bad multipliers
will result in even fewer hyperplanes, for example IBM's
infamous RANDU generator generated points lying on only 15 planes in 3-space.
Even with fairly good multipliers we still get test failures, e.g. Marsaglia's
spectrally good multiplier 69069 mod 2^32 fails his OPSO test, as does
the Berkeley PASCAL 62605 mod 2^29.) It will suffice, for
comparatively good behavior in d-space, that
   (a^d mod M)/M
have only small partial quotients in its continued fraction expansion.
This should be tested for d=1,2,..,8 at least.

Thus for example, consider the prime modulus M=2^35 - 849.
(M-1)/2 is also prime. I found the generator g=145683 by computer
search. The continued fraction expansions
CF(g/p)=[235852,1,3,6,1,1,5,1,1,2,1,1,1,1,6,2]
CF(g^2/p)=[1,1,1,1,1,1,1,1,21,2,7,1,8,1,2,1,1,2,5,1,1,1,2,1,2,3,12,2]
are rather nice, and the CF( (g^x mod p)/p ) for x=3,4,5,6,7,8,9 are
also not bad (no partial quotient larger than 23):
 [2,1,1,1,1,1,1,1,1,16,1,3,5,1,2,1,1,3,1,1,1,21,1,8,10,5]
 [8,4,3,1,1,3,4,2,7,1,22,22,9,1,12,1,2,2]
 [1,1,1,20,1,23,1,4,2,2,1,1,1,8,1,1,2,1,2,2,1,22,1,2,1,3]
 [1,1,15,1,1,10,2,2,2,1,4,5,1,2,1,1,5,7,7,2,2,4,2]
 [1,1,5,2,2,1,1,1,7,11,3,4,1,9,1,6,3,1,8,1,3,1,1,1,2]
 [1,13,1,7,8,1,1,2,16,2,2,1,1,6,4,1,3,1,1,3,15,2]
 [2,1,2,1,15,2,1,5,1,8,1,1,2,1,4,3,1,3,1,1,1,1,2,1,3,1,3,5]
so we conclude that using g as a multiplier should exhibit comparatively
good behavior in dimensions 2-9. This particular (g,M) pair
has the virtue that IEEE doubles can represent integers up to and including
2^53 - 1 exactly, so that the modula-3 statement x := (g*x) MOD M;
will evaluate it exactly.

As another example, the Marsaglia multiplier
69069 mod 2^32, while spectrally good in
dimensions 2-5, is bad in dimension 6, as is revealed by the spectral
test directly but also simply by noticing that the CF expansion of
69069^6 / 2^32 is [1, 75, 1, 2, 2, 1, 20, 10, 3, 10, 2, 2, 1, 12, 9]
which contains the large number 75 early on.

You also probably want
full period M, which happens if [thm page 16 Knuth]
  (1) GCD(b,M)=1;
  (2) a-1 is a multiple of p for every prime p dividing M;
  (3) a-1 is a multiple of 4 if 4 divides M.
If M is prime and b=0, you get maximal period M-1 if a primitive mod M.
If M>=16 is power of 2 and b=0, get maximal period M/4 if a=3 or 5 mod 8.

Other Fibo generators:
instead of just using one lag term and binary operation %, you could combine
with TWO lag terms via some TERNARY operation, or THREE lag terms
via a QUATERNARY operation, etc. I suggest the new generator
   QuaternaryFibo(a,b,c,d, x0 - (x2 XOR (x1 - x3) mod M) mod M ).
If M is 2^wordsize and x^a+x^b+x^c+x^d+1, a>b>c>d>0, is a primitive
polynomial mod 2, then this generator's period will be at least 2^a - 1
simply by considering the LS bit, which will follow a DeBruijn sequence
and thus exhibits good randomness in <=a dimensions. My idea is that by using
both XOR and +, we hope to avoid the weak behavior of either operation
alone, e.g. with respect to the birthday spacings test. This may also
make the generator "nonlinear" (i.e. not outputting a lattice), a point
I am unsure about. Ternary generators of this type do not seem to exist since
4-term primitive polynomials x^a+x^b+x^c+1 mod 2 do not exist (you
need an odd number of terms, as can easily be shown).
For tables of examples of primitive polynomials mod 2, see E.J.Watson: Math. of
Comput. 16 (1962) 368-9 but the ones given are the lexically first examples,
bad for our purposes.
Hence I constructed my own small table below of examples of primitive
polynomials P=x^a+x^b+x^c+x^d+1 mod 2, where a is prime and N=2^a - 1
is a Mersenne prime. For such a, P is primitive if P^N divides x^N-1 mod 2,
which you can test by the algorithm
  Q := x;
  FOR i:=1 TO a-1 do
    Q := Q*Q*x mod P;
  END;
  IF Q=1 THEN "P is primitive."
Table:
x^19 + x^9 + x^4 + x^3 + 1
x^19 + x^6 + x^2 + x^1 + 1
x^31 + x^22 + x^20 + x^8 + 1
x^31 + x^16 + x^11 + x^6 + 1
x^61 + x^5 + x^2 + x^1 + 1
x^61 + x^57 + x^2 + x^1 + 1
x^61 + x^19 + x^16 + x^9 + 1
x^89 + x^36 + x^2 + x^1 + 1
x^89 + x^57 + x^14 + x^5 + 1
x^89 + x^28 + x^26 + x^9 + 1
x^89 + x^37 + x^5 + x^1 + 1
x^107 + x^53 + x^45 + x^12 + 1
x^107 + x^93 + x^68 + x^2 + 1
x^127 + x^29 + x^17 + x^7 +
x^127 + x^67 + x^65 + x^17 + 1
x^127 + x^60 + x^22 + x^9 + 1
x^521 + x^46 + x^38 + x^37 + 1
x^521 + x^249 + x^92 + x^87 + 1
x^521 + x^353 + x^258 + x^6 + 1
x^607 + x^66 + x^60 + x^30 + 1
x^607 + x^247 + x^83 + x^71 + 1
x^607 + x^483 + x^449 + x^298 + 1
x^607 + x^442 + x^113 + x^23 + 1

It is a disgusting fact that in modula3 as well as most high level languages,
the machine language multiply instruction that computes a*b to double
precision is NOT ACCESSIBLE. Ditto "add with carry". This makes an
efficient implementation of high precision multiplication (& modular
version) obnoxiously difficult and inefficient. And you really
do need multiple precision to get a decent period with a lincong
or iterated squaring generator.
[Note the SQUARE ROOT of the period must be unreachably large for
good randomness, NOT the period, a common error.]
Here is a MetaAlgorithm which works
if numbers 0..modulus*2-2 are representable...:
  MetaModularMultiply(x,y, modulus: NumType) : NumType =
  VAR
    q : NumType := x;
  BEGIN
    FOR b = bits of y in MS-->LS order starting
        at the bit AFTER the first 1 bit DO
      q := q+q;
      IF q>=modulus THEN q := q-modulus; END;
      IF b=1 THEN
        q := q+x;
        IF q>=modulus THEN q := q-modulus; END;
      END;
    END;
    RETURN q;
  END MetaModularMultiply;

One way to improve the randomness of a generator is
C.Bays and S.D. Durham's shuffling algorithm, ACMTOMS 2 (1976)
59-64, also described in D.Knuth: Seminumerical algorithms.
Knuth says this will output a "considerably more random" sequence.
However, the set of values will still be the same (although ordered
differently) hence, e.g., the subtractive and XOR fibo gens will
STILL fail Marsaglia's birthday spacings test even after improvement
by shuffling. However, linear congruential generators are probably
substantially improved by shuffling since the lattice structure is
destroyed.

L.Blum, M.Blum, M. Shub: A simple unpredictable psuedo-random
number generator, SIAM J. Comput 15,2 (1986) 364-
following
A.Yao: Theory and applications of trapdoor fns, 23rd SFOCS (1982) 80-91
and
W.Alexi, B.Chor, O.Goldreich, C.P.Schnorr: 25th SFOCS (1984) 449-457.
point out that iterated squaring generator
  x <--- x*x mod M
where M=p*q, p, q primes that are 3 mod 4, will have these properties
(assuming it is computationally infeasible to compute the factorization of M)
(1) infeasible to predict the PREVIOUS x.
(2) infeasible to predict the least signif bit of the
    previous x with correctness prob > 1/2 + 1/polynomial.
(3) infeasible to predict the boolean "x<(M-1)/2" for the
    previous x with correctness prob > 1/2 + 1/polynomial.
consequence of (2) is: no polynomial time statistical test
can invalidate the randomness of the sequence of LS bits of
the iterated squaring generator.

The full iterated squaring gen, not just its LS bits,
is a lot faster and (conjecturally!) just as random.
Its period is (p-1)*(q-1)/4.

Example: Here is a product of two primes both 3 mod 4:
 94906247 * 94906219 = 9007193062250093 = 2^53 - 6192490899.
Here is a smaller example:
    M = 9739 * 9719 = 94653341.
This modulus has the virtue that iterated squaring mod M
may be accomplished exactly in IEEE double arithmetic using
 x := x*x MOD M, since M^4 < 2^53 - 1.
An even smaller modulus suitable for 32-bit unsigned arithmetic is
    M = 239*251; (* = 59989; 239 and 251 are each primes and 3 mod 4 *).

More generally you could use iterated cubing (subject to the conjecture
it is infeasible to break any bit of the RSA cryptosystem) or in fact
any fixed exponent you want (under same conjecture) and in fact almost
any fixed integer linear operation on the discrete logarithms of a vector
will by the same reasoning be immune to any polytime statistical test,
subject to RSA-type conjectures. In particular, we conjecture that the
Fibo[a,b,*] generator ought to be immune to polytime statistical tests
if the modulus is the product of two suitable large primes... which
explains the results of the Marsaglia test battery above.



3/17/96  Warren Smith    Initial version
3/23/96  Harry George    Added object wrappers, which also required
                         "InitDone" flag in Init proc.
*)

IMPORT LongRealBasic AS R,
       Word, Math, Tick, TimeStamp;
FROM RandomBasic IMPORT RandomGen;
IMPORT RandomRep;

CONST Module = "RandomNumber02.";
(*==========================*)
VAR InitDone:=FALSE;  (*is the big Init done yet?*)

(*---object wrappers---*)

(*------------------*)
REVEAL slow = RandomGen BRANDED OBJECT
  OVERRIDES
    init:=slow_init;
    engine:=slow_engine;
  END;

PROCEDURE slow_init(self:RandomGen;
                    seed:[FIRST(INTEGER)..-1]:=-1
                       ):RandomGen=
VAR
  t:=NARROW(self,slow);
BEGIN
  IF NOT InitDone THEN
    IF seed = -1 THEN
      (*default init*)
      Init(FALSE);
    ELSE
      (*use time to make it nonreproducible*)
      Init(TRUE);
    END;
  END;

  RETURN t;
END slow_init;

PROCEDURE slow_engine(<*UNUSED*>self:slow):R.T=
CONST
  ftn= Module & "slow_engine";
BEGIN
  RETURN Uni01();
END slow_engine;

(*------------------*)
REVEAL fast = RandomGen BRANDED OBJECT
  OVERRIDES
    init:=fast_init;
    engine:=fast_engine;
  END;

PROCEDURE fast_init(self:RandomGen;
                    seed:[FIRST(INTEGER)..-1]:=-1
                       ):RandomGen=
VAR
  t:=NARROW(self,fast);
BEGIN
  IF NOT InitDone THEN
    IF seed = -1 THEN
      (*default init*)
      Init(FALSE);
    ELSE
      (*use time to make it nonreproducible*)
      Init();
    END;
  END;

  RETURN t;
END fast_init;

PROCEDURE fast_engine(<*UNUSED*>self:fast):R.T=
CONST
  ftn= Module & "fast_engine";
BEGIN
  RETURN FasterUni01();
END fast_engine;




(*=============================================*)
CONST
  asf1 = 97;
  bsf1 = 34;
VAR
  isf1 := asf1;
  jsf1 := bsf1;
  arrsf1 : ARRAY [0..asf1-1] OF LONGREAL; (* initialize to rands in [0,1) not all with LS Bit=0*)

(* Generates a new random real in [0,1): *)
PROCEDURE SubtractiveFibo1() : LONGREAL =
  VAR
    x : LONGREAL;
  BEGIN
    DEC(isf1);
    DEC(jsf1);
    IF isf1<0 THEN
      isf1 := asf1-1; (* wraparound *)
    ELSIF jsf1<0 THEN
      jsf1 := asf1-1; (* wraparound *)
    END;
    x := arrsf1[isf1] - arrsf1[jsf1];
    IF x<0.0D0 THEN x := x+1.0D0; END; (* subtraction mod 1 *)
    arrsf1[isf1] := x;
    RETURN x;
  END SubtractiveFibo1;

CONST
  asf2 = 95;
  bsf2 = 17;
(*
  TwoTo32Minus5 = 16_fffffffb; (* prime *)
  TwoTo32Minus1 = 16_ffffffff;
*)
VAR
  isf2 := asf2;
  jsf2 := bsf2;
  arrsf2 : ARRAY [0..asf2-1] OF INTEGER;
       (* initialize to random Word.Ts mod TwoTo32Minus5 *)

(* Generates a new random word, mod 2^32 - 5 (a prime): *)
PROCEDURE SubtractiveFibo2() : Word.T =
  VAR
    x,y : Word.T;
  BEGIN
    <* ASSERT Word.Size = 32 *>
    DEC(isf2);
    DEC(jsf2);
    IF isf2<0 THEN
      isf2 := asf2-1; (* wraparound *)
    ELSIF jsf2<0 THEN
      jsf2 := asf2-1; (* wraparound *)
    END;
    y := arrsf2[isf2];
    x := Word.Minus(y, arrsf2[jsf2]);
    IF Word.GT(x, y) THEN (* subtraction "wrapped" *)
      x := x+5;
    END;
    arrsf2[isf2] := x;
    RETURN x;
  END SubtractiveFibo2;

CONST
  amf1 = 98;
  bmf1 = 27;
VAR
  imf1 := amf1;
  jmf1 := bmf1;
  arrmf1 : ARRAY [0..amf1-1] OF Word.T; (* initialize to random odd words *)

(* Generates a new random word: *)
PROCEDURE MultiplicativeFibo1() : Word.T =
  BEGIN
    DEC(imf1);
    DEC(jmf1);
    IF imf1<0 THEN
      imf1 := amf1-1; (* wraparound *)
    ELSIF jmf1<0 THEN
      jmf1 := amf1-1; (* wraparound *)
    END;
    arrmf1[imf1] := Word.Times(arrmf1[imf1], arrmf1[jmf1]);
    RETURN arrmf1[imf1];
  END MultiplicativeFibo1;

CONST
  asf3 = 89;
  bsf3 = 57;
  csf3 = 14;
  dsf3 = 5;   (* x^89 + x^57 + x^14 + x^5 + 1 primitive mod 2 *)
VAR
  isf3 := asf3;
  jsf3 := bsf3;
  ksf3 := csf3;
  msf3 := dsf3;
  arrsf3 : ARRAY [0..asf3-1] OF Word.T; (* initialize to random words, not all even *)

(** Generates a new random Word.T; period at least 2^asf3 - 1;
 * uses both XOR and [- mod 2^wordsize] in the recurrence, hence may
 * avoid some of the known problems with each of these operations alone. *)
PROCEDURE QuaternaryFibo() : Word.T =
  VAR
    x : Word.T;
  BEGIN
    DEC(isf3);
    DEC(jsf3);
    DEC(ksf3);
    DEC(msf3);
    IF isf3<0 THEN
      isf3 := asf3-1; (* wraparound *)
    ELSIF jsf3<0 THEN
      jsf3 := asf3-1; (* wraparound *)
    ELSIF ksf3<0 THEN
      ksf3 := asf3-1; (* wraparound *)
    ELSIF msf3<0 THEN
      msf3 := asf3-1; (* wraparound *)
    END;
    x := Word.Minus( arrsf3[msf3], Word.Xor( arrsf3[ksf3],
                   Word.Minus( arrsf3[isf3], arrsf3[jsf3] ) ) );
    arrsf3[isf3] := x;
    RETURN x;
  END QuaternaryFibo;

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
CONST
  wolfnum = 5;
  MSbit = Word.LeftShift(2_1, Word.Size-1);
VAR
  wolfarr : ARRAY [0 .. wolfnum-1] OF Word.T; (* initialize with random bits *)

PROCEDURE WolframCA() : BOOLEAN =
  VAR
    origcarry, carry, borrow : BOOLEAN;
    x, a, b : Word.T;
  BEGIN
    borrow    :=  ( Word.And( wolfarr[0], 2_1 ) # 0 );
    origcarry :=  ( Word.And( wolfarr[LAST(wolfarr)], MSbit ) # 0 );
    FOR i:=LAST(wolfarr) TO FIRST(wolfarr) BY -1 DO
      x := wolfarr[i]; (* old word *)
      IF i>0 THEN (* get carry from word below [borrow is from word above] *)
        carry :=  ( Word.And( wolfarr[i-1], MSbit ) # 0 );
      ELSE
        carry := origcarry;
      END;
      a := Word.RightShift(x,1);
      a := Word.Or( a, Word.LeftShift(ORD(borrow), Word.Size-1) );
      b := Word.LeftShift(x,1);
      b := Word.Or( b, ORD(carry) );
      (* CA update formula -> new word: *)
      wolfarr[i] := Word.Xor(a, Word.Or(x, b));
      (* get borrow from old word for next time: *)
      borrow :=  ( Word.And( x, 2_1 ) # 0 );
    END;
    RETURN borrow;
  END WolframCA;

CONST
  MULTmg = 69069;
  mgSIZE = 103;
  SCALEmg = (FLOAT(mgSIZE, LONGREAL) / 4294967296.0D0);
VAR
  MultCongMg : Word.T;  (* initialize to a random odd word *)
  ShiftRegMg : Word.T; (* initialize to a random word with 7ff in LS 11 bits *)
  arrmg : ARRAY [0..mgSIZE-1] OF Word.T; (* initialize to random Word.Ts *)
  ymg : Word.T := 0;

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
PROCEDURE ImprovedMcGill(): Word.T =
  VAR
    r0, r1 : Word.T;
    j : CARDINAL;
  BEGIN
    <* ASSERT Word.Size = 32 *>
    r0 := Word.RightShift(ShiftRegMg, 15);
    r1 := Word.Xor( ShiftRegMg, r0 );
    r0 := Word.LeftShift(r1, 17);
    ShiftRegMg := Word.Xor(r0, r1);

    MultCongMg := Word.Times(MULTmg, MultCongMg);
    (** Marsaglia's improvement: we've changed Word.Xor --> Word.Plus: *)
    r1 := Word.Plus(MultCongMg, ShiftRegMg);

    (** My improvement: the normal McGill generator would just return r1 here,
     * but I feed it into a Bays-Durham shuffler. *)
    j := FLOOR( FLOAT(ybd, LONGREAL) * SCALEmg );
    ymg := arrmg[j];
    arrmg[j] := r1;
    RETURN ymg;
  END ImprovedMcGill;

CONST
  abd = 101;
VAR
  arrbd : ARRAY [0..abd-1] OF LONGREAL; (* initialize to rands in [0,1) *)
  ybd : LONGREAL := 0.0D0;

(* Inputs a random real in [0,1), outputs a "more random" one: *)
<* UNUSED *>
PROCEDURE BaysDurhamShuffler(x : LONGREAL) : LONGREAL =
  VAR
    j : CARDINAL;
  BEGIN
    j := FLOOR( FLOAT(abd, LONGREAL) * ybd );
    ybd := arrbd[j];
    arrbd[j] := x;
    RETURN ybd;
  END BaysDurhamShuffler;

(******************************************************
The random words output by this generator ought to be extremely
random since this is a combination of 5 generators, each pretty good by
itself, and all 5 work according to different principles. Its only
disadvantage is it is too slow for applications which do
a very small amount of computing per random number.
  On a 100 MHz Pentium, I generated 10^7 random words in 47
seconds, i.e. 4.7 microseconds per rand, i.e. (assuming roughly 1 instruction
per clock) 470 instructions are needed to generate a rand, via RandWord().
Rough number of pentium clocks per call for various routines using the
parameters in the CONST declarations (Optimizer on. Asserts on.):
  SubtractiveFibo2      35
  MultiplicativeFibo1   40
  QuaternaryFibo        50
  SubtractiveFibo1      60  (code looks the fastest, but LONGREALs not Word.T)
  ImprovedMcGill       100
  WolframCA            130  (with wolfnum=5)
    RandWord           470  (yes, I know the sum isn't 470; timings crude!)
    Uni01             1000
    FasterUni01         80
    FasterRandWord      80
  Math.sin             130  (for comparison)
**********************************************)
PROCEDURE RandWord() : Word.T =
  BEGIN
    RETURN Word.Plus(
               Word.Plus(
         Word.Plus( SubtractiveFibo2(), MultiplicativeFibo1() ),
         Word.Plus( QuaternaryFibo(), ImprovedMcGill() ) ),
              ORD(WolframCA()) );
  END RandWord;

PROCEDURE Uni01() : LONGREAL =
  VAR
    x : LONGREAL;
  BEGIN
    x := Math.ldexp(
         Math.ldexp( FLOAT( RandWord(), LONGREAL ) , 6-Word.Size )
             + FLOAT( RandWord(), LONGREAL ), -Word.Size );
    <* ASSERT -0.5D0 <= x *>
    <* ASSERT x < 0.52D0 *>
    IF x < 0.0D00 THEN x := x+1.0D0; END;
    x := x - SubtractiveFibo1();
    IF x < 0.0D00 THEN x := x+1.0D0; END;
    <* ASSERT x >= 0.0D0 *>
    <* ASSERT x < 1.0D0 *>
    RETURN x;
  END Uni01;

(** However, if your need for speed is so great that RandWord() and Uni01()
above will not do, try the routines below, which only combine two of the
5 generators in RandWord(), selected for high speed and high randomness.
***************************************)
PROCEDURE FasterRandWord() : Word.T =
  BEGIN
    RETURN Word.Plus( SubtractiveFibo2(), MultiplicativeFibo1() );
  END FasterRandWord;

PROCEDURE FasterUni01() : LONGREAL =
  VAR
    x : LONGREAL;
  BEGIN
    x := Math.ldexp(
         Math.ldexp( FLOAT( MultiplicativeFibo1(), LONGREAL ) , 6-Word.Size )
             + FLOAT( MultiplicativeFibo1(), LONGREAL ), -Word.Size );
    (** note, those multiplications were really just bit shifts. How
      * do I get the compiler to know that?? *)
    <* ASSERT -0.5D0 <= x *>
    <* ASSERT x < 0.52D0 *>
    IF x < 0.0D00 THEN x := x+1.0D0; END;
    x := x - SubtractiveFibo1();
    IF x < 0.0D00 THEN x := x+1.0D0; END;
    <* ASSERT x >= 0.0D0 *>
    <* ASSERT x < 1.0D0 *>
    RETURN x;
  END FasterUni01;

CONST
   moduis = 9739.0D0 * 9719.0D0;
       (* = 94653341. Factors are each primes, 3 mod 4. *)
   DefaultSeed1 = 3145981;
   DefaultSeed2 = 2718280;
   DefaultXis = 243213.0D0;
VAR
   xis : LONGREAL;
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

    RETURN ( (xis < (moduis-1.0D0)*0.5D0) = (perturb#0) );
  END IteratedSquaring;

(* Generates a longreal, bit by bit, using IteratedSquaring *)
PROCEDURE initlongreal() : LONGREAL =
  VAR
    x : LONGREAL := 0.0D0;
  BEGIN
    FOR i:=0 TO 57 DO
      x := 0.5D0 * (x + FLOAT(ORD(IteratedSquaring()), LONGREAL));
    END;
    <* ASSERT 0.0D0 <= x *>
    <* ASSERT x < 1.0D0 *>
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
      xis := ABS( DefaultXis + FLOAT(seed1, LONGREAL)
                      + FLOAT(seed1, LONGREAL) ) MOD moduis;
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
(*  (*testing code: *)
   IO.Put( Fmt.LongReal( Uni01() ) & "\n");

   FOR i:=0 TO 10000000 DO
     EVAL ComboGen();
     EVAL SubtractiveFibo1();
     EVAL Math.sin( FLOAT(i, LONGREAL) );
     EVAL FasterGen();
     EVAL ImprovedMcGill();
     EVAL MultiplicativeFibo1();
     EVAL QuaternaryFibo();
     EVAL WolframCA();
     EVAL SubtractiveFibo2();
     EVAL FasterUni01();
     EVAL Uni01();
     EVAL FasterRandWord();
  END;
*)
END Test;
(*==========================*)
BEGIN
END RandomNumber02.
