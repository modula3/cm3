GENERIC MODULE FourierTransform(R, RT, C, CT);
(* By Warren D.  Smith, May 1985, March 1996.  Gnu Copylefted. *)
(*
Abstract: Fast Fourier Transforms (FFT's)

3/18/96  Warren Smith    Initial version

3/23/96  Harry George    Tweaked to fit m3na library
*)

IMPORT Word;

<* UNUSED *>
CONST
  Module = "FourierTransform.";

(*********************************************************
"In place" FFT routine with array length:  N = a power of 2.

 a{output}[k] = sum from m=0 to N-1 of a{input}[m]*exp(2 pi i direction m k/N)

where direction = +1 or -1.
You have to do any scaling (by 1/N or 1/sqrt(N)) or zeroing of a[] by
yourself.

Uses 2NlgN+2N+O(lgN)
[real] multiplications, and 3NlgN+2N+O(lgN) [real] additions.
(And hopefully the compiler optimizes the subscripting in the inner loop so
that only 2NlgN total subscripting ops are needed.)

The test routine shown yields
error after 1 forward and one backward use
at least 3 decimal places better than
slow FT has after only one use!

The basic FFT idea (for N a power of 2) is from J.Cooley&J.Tukey:
An algorithm for the Machine Calculation of Complex Fourier Series,
MOC 19 (1965) 297-301. (Knuth, Henrici, and Dahlquist/Bjorck also discuss
the FFT.) The basic idea is that to calculate the DFT
    A[k]  =  sum(j=0..N-1)of  a[j]*W^(k*j)  for k=0..N-1
  (where N = a power of 2, N>1, and W = a principal Nth root of unity,
   e.g. W = exp(2*i*PI/N) in the complex numbers)
we may write
    A[k]  = g[k] + h[k] * W^k
where
      g[k] = sum(0<=2j<N)of   a[2j]   * (W^2)^(k*j)
      h[k] = sum(0<=2j<N)of   a[2j+1] * (W^2)^(k*j)
are FFTs of half the size (on the even and odd indexed a[]'s, respectively)
note that g[k] and h[k] are periodic in k with period N/2, since W^(N/2) = -1,
so that if T(N)=time to calculate FFT of size N, then T(N)=2T(N/2)+O(N) and so
T(N)=O(NlgN). The inverse transform is
    a[k]  =  Ninverse * sum(j=0..N-1)of  a[j]*Winverse^(k*j)  for k=0..N-1
also an FFT and is calculable by the same method. (Ninverse*N=1. Winverse*W=1.)
Cooley-Tukey also works for general highly composite N; a short and elegant
implementation is:
[Warren E.Ferguson: A simple derivation of glassman's general N FFT,
Computers&Math. with Applics. 8 (1982) 401-411].
For an efficient implementation of the FFT, further refinements are desired.
These include: 1.Removal of the recursion by "reverse binary permuting" the
original data; 2."In place" implementation with no extra storage requirement;
3.Calculation of the W^k's (k=0,1..) by efficient and stable recurrences
(thus avoiding the need for transcendental functions); 4. Possibly use
"fast complex multiplication"; 5.In specific but common applications,
some further savings may be possible. Thus when doing a fast convolution
of two arrays of N reals using an FFT, the two FFTs may be accomplished by ONE
N point complex FFT, followed by linear time uncombining/termwise
multiplication step, followed by a N/2 point reverse FFT. The reverse binary
permuting stages may be avoided completely. The basic symmetry here is that
the FFT of a real array is "conjugate even", e.g.
    A[k] = CompConjugate(A[N-k])    if a[0..n-1] is real.

R.C.Singleton [e.g. see his algorithm for fast circular convolutions,
CACM 12,3 (March 1969) 179; his FFT2 algorithm, CACM 11 (Nov 1968) 773-779;
and his article CACM 10 (Oct 1967) 647-654] has suggested a second difference
method [which keeps the real and imaginary parts of exp(ick) uncoupled] for
evaluating exp(ick):    exp(ic(k+1)) = exp(ick) + I[k+1];
    I[k+1] = -4sin^2(c/2)exp(ikc) + I[k];   I[0] = 2isin(c/2) exp(-ic/2)
which is both faster and experimentally far more stable (typically
yielding 500 times smaller error for 128 point transform+inverse transform)
than the straightforward "multiply by exp(ic)" method. (It's more stable
because of the small multiplier -4sin^2(c/2), as opposed to 1.)

However a better idea which I recently thought of is to use this recurrence:
    exp(ic(k+1)) = exp(ic(k-1)) + 2isin(c) exp(ick)
which requires only 2*,2+ per complex exponential (Singleton is 2*,4+;
naive method is 4*,2+ and isn't stable) and is also stable since it involves
the small multiplier 2isin(c). (In fact, this method runs faster than
Singleton, is easier to program, and even yielded slightly better accuracy
in the test program below, too!) Therefore this modification of Singleton
is the method I've used.

Incidentally, Press et al. in their NR book use the Singleton algorithm
but neglect to credit Singleton!

By a trivial modification of my code, one could generate the sines and
cosines by repeated application of the bisection identities
    cos(t/2)=sqrt(0.5*(1.0+cos(t))),  sin(t/2)=0.5*sin(t)/cos(t/2),
starting from the values with t=pi and t=pi/2 as special cases, thus
avoiding trig subroutine calls entirely.
OK, I've now done this; now using precomputed table.

The time savings introduced by either of these is small, however.
Finally, "radix 8" transforms are experimentally the most efficient,
typically 20% faster than "radix 2" routines like this one, although far
more complicated; and anyway I suspect the advantage is <20% in the modern
cache-memory system world, since I suspect the radix 2 algorithm
has better cache locality. However, that has not been tested.

Also you could take advantage of 1's and 0's to save a little time at
the expense of considerably more space.

Another idea which I have chosen not to implement is the fact that two
complex numbers may be multiplied in 3 real multiplications:
  thus  E+iF = (a+bi)*(c+di)  may be accomplished via the instructions
   bpa = b+a; bma = b-a; E = a*(c+d); F = bma*c+E; E -= bpa*d;
and if bpa and bma are precomputed, this is a 3*,3+ method for a complex
multiplication. This idea may be used to reduce (?) the box score from
the present  2NlgN+2N+O(lgN) mults, 3NlgN+2N+O(lgN) adds  to
1.5NlgN+2N+O(lgN) mults  and  3.5NlgN+4N+O(lgN) adds. If a floating point
multiplication is M times slower than a floating point addition,
this idea pays iff lgN>4/(M-1). On PDP-11/44 C, however, rough timing
has shown that M=1.08 (but with considerable standard deviation... it does
about 5*10^4 additions/sec.) so this idea is not worth it unless N is
enormous.

Some similar, but worse, ideas have been suggested by Buneman [If c=cos(m),
s=sin(m), then precompute  t1 = (1-c)/s = s/(1+c)= tan(m/2)  and
t2 = (1+s)/c = c/(1-s). Then X+iY = (a+bi)*(c+is) may be found in 3*,3+ by:
  if(|t1|<|t2|){ X = a-t1*b; Y = b+s*X; X -= t1*Y; }
  else{ X = b+t2*a; Y = c*X-a; X -= t2*Y; }
tans may be updated by  tan(x+y)-tan(x-y) = 2*tan(y)/(1-(tan(x)*tan(y))^2)),
but the extra overhead seems not to be worth it] and also there is a
symmetric 3*,5+ (4+ with precomputation) formula for  E+iF = (a+bi)*(c+di):
  E = a*c-b*d; F = (a+b)*(c+d)-a*c-b*d.

Other FFT algorithms: Winograd has shown how to design FFTs with N prime
(as opposed to the Cooley-Tukey approach which works for N highly composite)
that run in O(NlgN) time and even with only O(N) multiplications;
the latter figure is optimal. [S.Winograd: Math. of Comput. 32 (1978) 175-179;
Advs in Math 32 (1979) 83-117].
Winograd's approach is based on a theorem that allows him (by a permutation
of the original and transformed variables) to express FFTs for N prime
in terms of a circular convolution of N-1 elements, plus some additions.
He then shows how circular convolutions of k elements (for certain small k)
may be computed in a small number of arithmetic operations, (for k=2..6,
the number of multiplications Winograd uses is 2,4,5,10,8; for k prime,
Winograd shows that a (2k-2)* scheme for CC(k) always exists) and further, how
CC(n1) and CC(n2) algorithms may be composed to make a CC(n1*n2) algorithm, IF
n1 and n2 are relatively prime, that uses mult(n1)*mult(n2) multiplications.
Also, he shows how FFT(n1*n2) may be computed via FFT(n1) and FFT(n2)
in mult(n1)*mult(n2) multiplications, IF n1,n2 relatively prime, and
also gives methods for FFT(prime power). He gives two appendices
containing optimized CC(2..6) and FFT(2..9) algorithms. Winograd's methods
don't appear suitable for general N, but if N is specified in advance, they
make it possible to do considerable fine tuning at the expense of
large algorithm complexity.

Meanwhile, Nussbaumer [H.J.Nussbaumer: Fast Polynomial Transform algorithms
for digital convolutions, IEEE Transactions on Audio, Speech, Signal Processing
28,2 (April 1980) 205-215 (this article has many references to other FFT
schemes); see also Knuth 2: 503, 652-653] has found another way to do circular
convolutions of arrays of (N=a power of 2) reals without any NTTs, FFTs, trig,
or complex numbers. His approach is based on viewing circular convolutions
as polynomial multiplications modulo certain simple polynomials, factoring the
modular polynomials, divide and conquer, chinese remainder thm.
His approach uses roughly NlgN *,
NlgNlglgN +, is fairly complicated to program, and requires extra space.

[Nussbaumer&Quandalle: IBM JResDev 22 (1978) 134-144] show how some
particularly efficient CC and FFT schemes for N in the range 10-3000
may be constructed; their approach is based on some novel ways to combine
efficient small CC schemes that is rather like the NTT (Number theoretic
transform) only in rings of polynomials rather than in the integers.

However even the best known arithmetic op count methods only improve on my
method by perhaps 30%, and at the cost of considerable complexity.
C.H.Papadimitriou [Optimality of the FFT, JACM 26 (1979) 95-102 and its refs]
has shown that in some models of computation O(NlgN) is
optimal for the FFT, while Patterson et al have shown a lower
bound of O(NlgN/lglgN) for integer multiplication on multitape
Turing machines, see Knuth 2.

FFTs in a finite field (if the ring ZmodK, called "number theoretic
transforms") are discussed in Aho,Hopcoft,Ullman: The Design and Analysis
of Computer algorithms, Addison-Wesley 1974. They recommend using W=2,
N=a power of 2, do all arithmetic in the ring of integers modulo 2^(N/2)+1
(in which W is an Nth root of unity, and in which the convolution theorem
    C[i]=A[i]B[i]  <==>   c[i] = sum(j=0..N-1)of a[j]*b[i-j mod N]
[Which makes possible the calculation of discrete convolutions in NlgN time]
still holds). [See also R.Agrawal&C.Burrus: NTTs to implement fast digital
convolutions, ProcIEEE 63 (1975) 550; articles by H.Nussbaumer on "Fermat"
and "Mersenne" transforms, IBMJR&D 21 (1976) 282 and 498.]
These FFFFTs are of use in applications where it is desirable
to completely eliminate roundoff error and floating point operations,
e.g. all-integer convolutions. However, as you can see, NTTs have severe
word length and transform length limitations; the need for high precision
modular arithmetic can be a major stumbling block. On the other hand,
multiplications by W=2 are easy, while modular arithmetic modulo a
Fermat number is not that hard. Thus using N=16, modulo 65537 arithmetic,
W=2 [left shift and modulo], Winverse=32769 [right shift; modular
addition correction if inexact], and all numbers in 0..89 allows
computation of CC(16) in 16*, many bit shifts and additions.

Rabiner,Schafer,Rader: The Chirp-Z transform and its Applications,
BSTJ 48,3 (1969) 1249-1292 show how DFTs (for any N) may be calculated in
NlgN time by using fast convolutions; the method also works for an extension
of FFTs (to W=any complex number, not just the principal Nth root of unity):
    A[k]  =  sum(j=0..N-1)of  a[j]*W^(k*j)  for k=0..N-1
may be calculated in NlgN time by a fast convolution by the "Chirp-Z
transform" identity
    A[k] = W^(k*k/2) * sum(j=0..N-1)of { W^(-((j-k)^2)/2) * W^(j*j/2)*b[j] }.
Aho,Stieglitz,Ullman: Evaluating Polynomials at fixed sets of points,
SIAMJComp 4,4 (Dec 1975) 533-539, demonstrate that a polynomial and all its
derivatives at one point (or equivalently, an origin shift of an Nth
degree polynomial) may be calculated in NlgN time by a fast convolution
via the (binomial theorem) identity
    sum(j=0..N-1)of c[j]*(x+q)^j  =  sum(r=0..N-1)of x^r * d[r]/r!
  where
    d[r] = sum(j=r..N-1)of c[j]*j! * q^(j-r)/(j-r)!  .

Some other applications of FFTs are:

Fast multiplication and division of N digit integers may be done in
(roughly) NlgN time by using fast convolutions followed by a carry step.
(See Aho-Hopcoft-Ullman: Design and Analysis of Computer algorithms, for
further discussion.)

Base conversion of an N digit number may be done in N(lgN)^2 time by
divide and conquer (convert the left and right half of the number recursively,
then do a fast multiplication and addition to combine them).

Fast polynomial multiplication and division by fast convolutions in NlgN
time are also discussed in AHU. (This may also be done for Chebyshev
series...) Given the N roots of a polynomial, you can find its
coefficients (as Chebyshev or as regular) in N(lgN)^2 time by fast
polynomial multiplications on a binary tree. On the other hand,
you can perform a "root squaring" transformation on a polynomial
in (Chebyshev or power form)
   P(y) = -P(x)*P(-x)  ,  y = x^2
in NlgN time by a fast multiplication, or alternatively can implement
a Henrici-Gargantini or Korsak-Pease (or other) simultaneous all root iteration
step, in N(lgN)^2 time by a fast multipoint evaluator, see below.

All shifted correlations of vectors (and/or autocorrelations) may be
calculated in NlgN time by fast convolutions; this has application in
signal processing, 1D pattern recognition, Electrical engineering.

Fast polynomial multiplication/division/remaindering and a divide and
conquering of the Lagrange interpolation formula may be used to do fast
Nth degree polynomial interpolation and N-point evaluation, as was
shown by Borodin&Moenck [JCompSystSci 1974]. I have extended B&M's
results to Chebyshev polynomials and less successfully to other
polynomials.

Fast algorithms exist for power-series to continued fraction interconversion;
these may also be generalized to Chebyshev series.

Fast polynomial evaluation/interpolation at special point sets
(e.g. Z^k for some Z) may be accomplished in NlgN time by the Chirp-Z and
FF transforms; this also carries over to Chebyshev. Thus fast Taylor
and Chebyshev series calculations.

Fast composition of Taylor series -
O((NlgN)^(3/2)) is also possible, as was discovered by Brent&Kung,
via a "block Horner" approach. This may also be extended to Chebyshev.

Fast Elliptic linear PDE solvers (by finite differences or spectrally):
there are many schemes based on FFTs that run in NlgN time, N=size of
output.

A complete list of FFT applications is far too huge to discuss here...
************************************************************************)

CONST TrigTabSize = 40;

VAR                              (* CONST after initialization: *)
  (* cos( pi / 2^k ): *)
  PrecomputedCos: ARRAY [0 .. TrigTabSize - 1] OF R.T;

  (* sin( pi / 2^k ): *)
  PrecomputedSin: ARRAY [0 .. TrigTabSize - 1] OF R.T;

(***********************************************************
Uses repeated application of the bisection identities
    cos(t/2)=sqrt(0.5*(1.0+cos(t))),  sin(t/2)=0.5*sin(t)/cos(t/2),
starting from the values with t=pi and t=pi/2 as special cases.
We see no reason to trust trig routines sin() and cos()
although we will trust sqrt() and division.
**************************************************************)
PROCEDURE PreComputeTrigTables () =
  BEGIN
    PrecomputedCos[0] := R.MinusOne;
    PrecomputedCos[1] := R.Zero;
    PrecomputedSin[0] := R.Zero;
    PrecomputedSin[1] := R.One;
    FOR k := 2 TO TrigTabSize - 1 DO
      PrecomputedCos[k] :=
        RT.SqRt(RT.Half * (R.One + PrecomputedCos[k - 1]));
      PrecomputedSin[k] :=
        RT.Half * PrecomputedSin[k - 1] / PrecomputedCos[k];
    END;
  END PreComputeTrigTables;

(*************************************************
Reorders array a[0..n-1] so that element[i] is
swapped with element[reverse-bit-order[i]].
Two successive calls are identity.
The algorithm below runs in O(n) time and is based on implementing a
reverse-binary counter. The forward counter f increments 0..n/2-1 step 2
and if r, 0<=r<n/2 is the bit-reverse of f,
we swap(f+1,n/2+r) with no test needed and we do
swap(f,r) if r>f and also swap(n-1-f,n-1-r) if f,r both <n/2.
***************************************************)
PROCEDURE ReOrder (VAR a: ARRAY OF C.T; ) =
  (* a[] overwritten by permutation. *)
  <* INLINE *>
  PROCEDURE Swap (x, y: CARDINAL; ) =
    VAR tmp: C.T;
    BEGIN
      (*ASSERT(isbitreverse(x,y,n))*)
      tmp := a[x];
      a[x] := a[y];
      a[y] := tmp;
    END Swap;
  VAR
    r    : CARDINAL;
    j    : Word.T;
    n    : CARDINAL := NUMBER(a);
    nb2  : CARDINAL := n DIV 2;
    nb2m1: CARDINAL := nb2 - 1;
    nb4  : CARDINAL := n DIV 4;
    nm1  : CARDINAL := n - 1;
  BEGIN
    <* ASSERT n > 0 *>
    <* ASSERT Word.And(n - 1, n) = 0 *>
    (** n must be a power of 2. **)
    r := 0;
    FOR f := 0 TO nb2m1 BY 2 DO
      <* ASSERT f + 1 < nb2 + r *>
      Swap(f + 1, nb2 + r);
      IF f < r THEN
        Swap(f, r);
        IF r < nb2 THEN Swap(nm1 - f, nm1 - r); END;
      END;
      (** increment the reverse binary counter r;
       * while loop executes O(1) times on average: *)
      j := nb4;
      WHILE Word.And(r, j) # 0 DO j := Word.RightShift(j, 1); END;
      r := Word.And(r + j + Word.LeftShift(j, 1), nb2m1);
    END;
  END ReOrder;

(***************************************
 direction = +1 for inverse FFT, -1 for forward.  (See FFT defn
 above.)
 a[] overwritten by transform.
NOTE: You must call ReOrder(a) before calling this routine,
because this routine assumes it has re-ordered a[]s as its input.
To do an FFT of some data, therefore, we would call
  ReOrder(data); FFTwithWrongOrderedInput(data);
I have separated the routines this way because I want to be able to
avoid the ReOrder when computing convolutions and correlations.
****************************************)
PROCEDURE FFTwithWrongOrderedInput
  (VAR a: ARRAY OF C.T; direction: [-1 .. 1]; ) =
  VAR
    n                         : CARDINAL := NUMBER(a);
    nm1                       : CARDINAL := n - 1;
    ur, ui, wr, wi, tr, ti, zz: R.T;
    k, j, L2, L, ip           : CARDINAL;
    dir                                  := FLOAT(direction, R.T);
  BEGIN
    <* ASSERT direction # 0 *>
    <* ASSERT n > 0 *>
    <* ASSERT Word.And(n - 1, n) = 0 *>
    (** n must be a power of 2 and n>=1. **)

    (* Now for FFT main loop *)
    L := 1;
    k := 0;
    WHILE L < n DO
      L2 := L + L;
      ur := R.One;
      ui := R.Zero;
      wr := PrecomputedCos[k];
      zz := PrecomputedSin[k] * dir;
      wi := -zz;
      zz := zz + zz;
      j := 0;
      LOOP
        FOR i := j TO nm1 BY L2 DO
          (** Press et al. refer to below as the "Danielson-Lanzcos
           * formula", since it was published by them in 1942 - much prior
           * to Cooley & Tukey. Typically, despite the fact that it was
           * known to D&L and even to Gauss (! see Goldstine: History of
           * Numerical Analysis), I'm told the FFT has been
           * patented. Of course, we pay no attention to this "patent". *)
          ip := i + L;
          tr := a[ip].re * ur - a[ip].im * ui;
          ti := a[ip].im * ur + a[ip].re * ui;
          a[ip].re := a[i].re - tr;
          a[ip].im := a[i].im - ti;
          a[i].re := a[i].re + tr;
          a[i].im := a[i].im + ti;
        END;
        INC(j);
        IF j >= L THEN EXIT; END;
        (* And here is my superior recurrence to calculate the trig: *)
        tr := ur;
        ti := ui;
        ur := wr - zz * ti;
        ui := wi + zz * tr;
        wr := tr;
        wi := ti;
      END;
      L := L2;
      INC(k);
    END;
  END FFTwithWrongOrderedInput;

(************************************************************
Given two real vectors x[0..N-1] and y[0..N-1], the scaled circular convolution
z[0..] is defined by
   z[j] = scale * SUM(k=0..N-1)of x[j-k] * y[k].
If N is a power of 2, the below routine will compute the circular
convolution of x[] and y[] where it is assumed that x[] had been
copied into the real, and y[] into the imaginary, parts of
complex array a[], on input. On output, z will occupy the real part
of a[]. A non-circular convolution is got by use of the same routine,
but with the large-indexed part of the x[] and y[] arrays zeroed
so that the wraparound terms are all 0. To compute correlations,
you can do a convolution with the y[] array in reverse order.
***********************************
PROCEDURE CircularConvolution(a: ARRAY OF C.T; scale:R.T;)
NOT IMPLEMENTED YET
*********************************)

(***************************************************
Slow FT routine, useful for debugging fast one.
 b[k] = sum from m=0 to N-1 of a[m]*exp(2 pi i direction m k/N)
where direction = +1 or -1.
*****************************************************)
PROCEDURE SlowFT (READONLY a: ARRAY OF C.T; direction: [-1 .. 1]; ):
  REF ARRAY OF C.T =
  VAR
    n        := NUMBER(a);
    b        := NEW(REF ARRAY OF C.T, n);
    sum: C.T;
    dir      := FLOAT(direction, R.T);
    kn : R.T;
  BEGIN
    <* ASSERT direction # 0 *>
    FOR k := 0 TO n - 1 DO
      sum := C.Zero;
      kn := dir * RT.TwoPi * FLOAT(k, R.T) / FLOAT(n, R.T);
      FOR m := 0 TO n - 1 DO
        sum := C.Add(sum, C.Mul(a[m], CT.ExpI(kn * FLOAT(m, R.T))));
      END;
      b[k] := sum;
    END;
    RETURN b;
  END SlowFT;

(**** Test driver. ****)
PROCEDURE Test () =
  VAR
    a           := NEW(REF ARRAY OF C.T, 128);
    b           := NEW(REF ARRAY OF C.T, 128);
    n           := NUMBER(a^);
    x: CARDINAL;
  BEGIN
    <* ASSERT NUMBER(b^) = n *>

    (* initialize a[] to psu-random complex numbers... *)
    x := 432531;
    FOR j := LAST(a^) TO FIRST(a^) BY -1 DO
      x := x * 57 MOD 1048583;   (* 57 is generator, mod this prime *)
      a[j].re := FLOAT(x, R.T);
      x := x * 57 MOD 1048583;   (* 57 is generator, mod this prime *)
      a[j].im := FLOAT(x, R.T);
    END;

    (* make copy b of a: *)
    b^ := a^;
    ReOrder(a^);
    ReOrder(a^);
    (* check reordering twice yields identity: *)
    FOR j := LAST(a^) TO FIRST(a^) BY -1 DO
      <* ASSERT CT.Norm1(C.Sub(a[j], b[j])) < FLOAT(0.000000001D0, R.T) *>
    END;

    (* forward transform of 'a' in place: *)
    ReOrder(a^);
    FFTwithWrongOrderedInput(a^, 1);

    VAR c := SlowFT(b^, 1);

    BEGIN
      (* check slow and fast give same result: *)
      FOR j := LAST(a^) TO FIRST(a^) BY -1 DO
        <* ASSERT CT.Norm1(C.Sub(a[j], c[j])) < FLOAT(0.0001D0, R.T) *>
      END;
    END;

    (* backward: *)
    ReOrder(a^);
    FFTwithWrongOrderedInput(a^, -1);
    FOR j := LAST(a^) TO FIRST(a^) BY -1 DO
      a[j] := C.Scale(a[j], R.One / FLOAT(n, R.T));
    END;

    (* check get original back: *)
    FOR j := LAST(a^) TO FIRST(a^) BY -1 DO
      <* ASSERT CT.Norm1(C.Sub(a[j], b[j])) < FLOAT(0.0000001D0, R.T) *>
    END;
  END Test;


BEGIN
  PreComputeTrigTables();
END FourierTransform.
