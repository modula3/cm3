MODULE RandomBasic EXPORTS RandomBasic,RandomRep;
(*Copyright (c) 1996, m3na project

Abstract: Random number generators

3/16/96  Harry George    Initial version (basic structure)
3/17/96  Warren Smith    Gamma, Gaussian, Dirichlet deviates
*)

IMPORT LongFloat;
IMPORT LongRealBasic AS R,
       LongRealTrans AS RT,
       LongReal      AS RSpec,
       LongRealIntegerPower AS RP,
       Word AS W;
FROM xUtils IMPORT Error,Err;

CONST Module = "RandomBasic.";

(*======================================*)
REVEAL
  TBoolean = TBooleanPublic BRANDED OBJECT
  OVERRIDES
    generateBoolean:=GenerateBooleanFromBoolean;
    generateWord:=GenerateWordFromBoolean;
    generateReal:=GenerateRealFromBoolean;
  END;

<*INLINE*>
PROCEDURE GenerateBooleanFromBoolean(SELF:TBoolean) : BOOLEAN =
  BEGIN
    RETURN SELF.engine();
  END GenerateBooleanFromBoolean;

(* Generates a word, bit by bit *)
PROCEDURE GenerateWordFromBoolean(SELF:TBoolean) : W.T =
  VAR
    x : W.T := 0;
  BEGIN
    FOR i:=0 TO W.Size DO
      x := W.Plus( W.LeftShift(x,1), ORD(SELF.engine()));
    END;
    RETURN x;
  END GenerateWordFromBoolean;

(* Generates a longreal, bit by bit *)
PROCEDURE GenerateRealFromBoolean(SELF:TBoolean) : R.T =
  VAR
    x : R.T := R.Zero;
  BEGIN
    FOR i:=0 TO RSpec.Precision-1 DO
      x := RT.Half * (x + FLOAT(ORD(SELF.engine()), R.T));
    END;
    <* ASSERT R.Zero <= x *>
    <* ASSERT x < R.One *>
    RETURN x;
  END GenerateRealFromBoolean;

(*======================================*)
REVEAL
  T = TPrivate BRANDED OBJECT
  OVERRIDES
    uniform:=Uniform;
    exponential:=Exponential;
    gaussian:=NormalDev;
    gamma:=GammaDev;
    dirichlet:=Dirichlet;
(*
    poisson:=Poisson;
*)
    binomial:=Binomial;
  END;
(*========================================*)
(*-------------------*)
PROCEDURE Uniform(SELF:T;
                  min:R.T:=R.Zero;  (*from min*)
                  max:R.T:=R.One;   (*up to but not including max*)
                  ):R.T             (*return uniform deviate*)
                  (* RAISES{xUtils.Error}  using this here and in the methods declarations let the compiler believe that procedure and method definitions do not match and thus it leads to the error: "procedure redefined (Uniform)" *)
                  =
VAR
  t:R.T;
BEGIN
  t:=SELF.generateReal();
  IF min=Min AND max=Max THEN RETURN t; END;

  IF min>=max THEN
    RAISE Error(Err.out_of_range);
  END;

  RETURN min + t*(max-min);
END Uniform;
(*-------------------*)
PROCEDURE Exponential(SELF:T):R.T=
(*exponential, mean=1 *)
BEGIN
  RETURN -RT.Ln(SELF.generateReal());
END Exponential;
(*-------------------*)
(**********************
PROCEDURE Gaussian1(SELF:T):R.T=
(*gaussian, mean=0, var=1 *)
(*based on NR92*)
VAR
  v1,v2,Rsq,tmp,result:R.T;
BEGIN
  IF NOT SELF.start THEN
    SELF.start:=TRUE;
    RETURN SELF.gauss_y;
  END;

  REPEAT
    v1:=R.Two*SELF.generateReal() - R.One;
    v2:=R.Two*SELF.generateReal() - R.One;
    Rsq:=v1*v1 + v2*v2;
  UNTIL (Rsq > R.Zero) AND (Rsq < R.One);
  tmp:=R.sqrt(-R.Two*R.log(Rsq))/Rsq;
  result:=v1*tmp;
  SELF.gauss_y:=v2*tmp;
  SELF.start:=FALSE;
  RETURN result;
END Gaussian1;
*********************************)
(*---------------------------*)
(*---Warren Smith's Normal---*)
(**Generates a normal (Gaussian) deviate with mean 0 and variance 1.
 * The "series method" [Devroye page 170] is buggy, so I am
 * using Marsaglia-Bray method on page 390 Devroye, see
 * G.Marsaglia & T.A. Bray: A convenient method for
 * generating normal random variables, SIAM Review 6 (1964) 260-264.**)
PROCEDURE NormalDev(SELF:T) : R.T =
  VAR
    v,u,w,x,sum : R.T;
  BEGIN
    u := SELF.uniform();
    IF u <= 0.8638D0 THEN
      v := SELF.uniform(-R.One, R.One);
      w := SELF.uniform(-R.One, R.One);
      x := 2.3153508D0 * u - R.One + v + w;
      RETURN x;
    ELSIF u <= 0.9745D0 THEN
      v := SELF.uniform();
      x := 1.5D0 * (v-R.One + 9.0334237D0 * (u - 0.8638D0));
      RETURN x;
      (* we only get here with probability 0.0255: *)
    ELSIF u > 0.9973002D0 THEN
      REPEAT
        v := SELF.uniform();
        w := SELF.uniform();
        x := 4.5D0 - RT.Ln(w);
      UNTIL x*v*v <= 4.5D0;
      x := LongFloat.CopySign( RT.SqRt(x+x) , u - 0.9986501D0 );
      RETURN x;
    ELSE
      REPEAT
        x := SELF.uniform(-3.0D0, 3.0D0);
        u := SELF.uniform();
        v := ABS(x);
        w := 3.0D0-v;
        w := 6.6313339D0 * w*w;
        sum := R.Zero;
        IF v < 1.5D0 THEN sum := 6.0432809D0 * (1.5D0 - v); END;
        IF v < R.One THEN sum := sum + 13.2626678D0 * (3.0D0 - v*v) - w; END;
      UNTIL u <= 49.0024445D0 * RT.Exp(-v*v*0.5D0) - sum - w;
      RETURN x;
    END;
  END NormalDev;
(*-------------------*)
(***************************************
PROCEDURE Gamma1(SELF:T;
                event:[1..LAST(INTEGER)]):R.T=
(*gamma, waiting time for event in Poisson process, mean=1*)
(*based on NR92*)
CONST
  cutoff=7;
VAR
  x,v1,v2,tanU,a0,x0,ratio:R.T;
BEGIN
  IF event < cutoff THEN
    x:=R.One;
    FOR i:=1 TO event DO
      x:=x*SELF.generateReal();
    END;
    x:=-R.log(x);
  ELSE
    x0:=FLOAT(event-1,R.T);
    a0:=R.sqrt(R.Two*x0+R.One);
    REPEAT
      REPEAT
        REPEAT
          v1:=R.Two*SELF.generateReal()-R.One;
          v2:=SELF.generateReal();
        UNTIL (v1*v1+v2*v2) <= R.One; (*within unit half-circle*)
        tanU:=v2/v1;
        x:=a0*tanU+x0;
      UNTIL x > R.Zero;  (*within positive probabilities*)
      ratio:=(R.One+tanU*tanU)*R.exp(x0*R.log(x/x0) - a0*tanU);
    UNTIL SELF.generateReal() > ratio;
  END;
  RETURN x;
END Gamma1;
***********************************)
(*-------------------*)
(** Returns a Gamma deviate with parameter a>=0.
 * Density(x) = x^(a-1) * exp(-x) / GAMMA(a)  for x>=0.
 * mean = a. variance = a.
 *
 * Cheng's algorithm
 * [Devroye page 413] if a>=1 and Berman's algorithm [Devroye page 419]
 * if a<=1 would have done the job, but they both have bugs. Other possible
 * algorithms in Devroye include Wilson-Haferty page 141 for a>=0.5,
 * Vaduva algorithm page 415 for a<1, and algorithms GS and RGS for a<1
 * pages 425, 426.
 *
 * Present code is based on code by Steve Omohundro based on
 * Brian D. Ripley: Stochastic Simulation, John Wiley and Sons, NY 1987,
 * p88-90. It appears to work now
 * according to mean and variance tests at a=.3,.5,.6,.9,1,2,3.
***************************************)
PROCEDURE GammaDev(SELF:T;
                   a : R.T) : R.T =
  BEGIN
    <* ASSERT a>R.Zero *>
    IF a<R.One THEN
      VAR
        u0,u1,x : R.T;
      BEGIN
        LOOP
          u0 :=  SELF.uniform();
          u1 :=  SELF.uniform();
          IF (a+RT.E)*u0>RT.E THEN
            x := -RT.Ln((a+RT.E)*(R.One-u0)/(a*RT.E));
            IF u1 <= RT.Pow(x, a-R.One) THEN
              <* ASSERT x>=R.Zero *>
              RETURN x;
            END;
          ELSE
            x := RT.Pow((a+RT.E)*u0/RT.E, R.One/a);
            IF u1<=RT.Exp(-x) THEN
              <* ASSERT x>=R.Zero *>
              RETURN x;
            END;
          END;
        END; (*LOOP*)
      END;
    ELSIF a>R.One THEN
      (* Cheng+Feast algorithm [CACM 23,7 (1980) 389-394?] for a>1: *)
      VAR
        c1 := a-R.One;
        c2 := (a-R.One/(6.0D0*a))/c1;
        c3 := 2.0D0/c1;
        c4 := c3+2.0D0;
        c5 := R.One/RT.SqRt(a);
        u1, u2, w: R.T;
      BEGIN
        LOOP
          REPEAT
            u1 :=  SELF.uniform();
            u2 :=  SELF.uniform();
            IF a>2.5D0 THEN
              u1 := u2 + c5*(R.One-1.86D0*u1);
            END;
          UNTIL R.Zero<u1 AND u1<R.One;
          w := c2*u2/u1;
          IF c3*u1+w+R.One/w <= c4
             OR
             c3*RT.Ln(u1) - RT.Ln(w) + w < R.One THEN
            w := w*c1;
            <* ASSERT w>=R.Zero *>
            RETURN w;
          END;
        END; (*LOOP*)
      END;
    ELSE (* a=1, just use exponential: *)
      RETURN -RT.Ln( SELF.uniform() );
    END;
  END GammaDev;
(*----------------------*)
(** Will generate a sample from a Dirichlet distribution
 * with parameters p[].
 * Follows L.Devroye: Non-uniform random variate generation,
 * Springer 1986.   p[] is overwritten by the Dirichlet deviate.
 *)
PROCEDURE Dirichlet(SELF:T;
                    p:R.Array) =
  VAR
    t, sum : R.T;
    n1:=FIRST(p^); nn:=LAST(p^);
  BEGIN
    sum := R.Zero;
    FOR n:=nn TO n1 BY -1 DO
      t := GammaDev(SELF, p[n] );
      p[n] := t;
      sum := sum + t;
    END;
    t := R.One/sum;
    FOR n:=nn TO n1 BY -1 DO
      p[n] := p[n] * t;
    END;
  END Dirichlet;

(*
(*-------------------*)
PROCEDURE Poisson(SELF:T;
                     m:R.T    (*mean*)
                     ):R.T=
(*Poisson, integer returned as real*)
<*UNUSED*> CONST ftn = Module & "Poisson";
BEGIN
END Poisson;
*)
(*-------------------*)
PROCEDURE Binomial(SELF:T;
                     p:R.T;
                     n:CARDINAL
                     ):CARDINAL=

  PROCEDURE Calc(p,q:R.T):CARDINAL =
  VAR
    qp:=q/p;
    prob:=RP.Power(p,n);
    rnd:=SELF.generateReal();
    den:=R.Zero;
    num:=FLOAT(n,R.T);
    k:CARDINAL:=0;
  BEGIN
    WHILE prob<rnd DO
      rnd:=rnd-prob;
      den:=den+R.One;
      prob:=prob*qp*num/den;
      num:=num-R.One;
      INC(k);
    END;
    RETURN k;
  END Calc;

<*UNUSED*> CONST ftn = Module & "Binomial";
BEGIN
  IF n=0 THEN
    RETURN 0;
  ELSIF p<RT.Half THEN
    RETURN n-Calc(R.One-p,p);
  ELSE
    RETURN   Calc(p,R.One-p);
  END;
END Binomial;

(*==========================*)
BEGIN
END RandomBasic.
