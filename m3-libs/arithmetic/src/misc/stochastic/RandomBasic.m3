MODULE RandomBasic EXPORTS RandomBasic,RandomRep;
(*Copyright (c) 1996, m3na project

Abstract: Random number generators

3/16/96  Harry George    Initial version (basic structure)
3/17/96  Warren Smith    Gamma, Gaussian, Dirichlet deviates
*)

IMPORT Math, LongFloat;
IMPORT xReal64 AS R;
FROM xReal64 IMPORT REAL64;
FROM xUtils IMPORT Error,Err;

CONST Module = "RandomBasic.";
CONST
  EulerE =     2.71828182845904523536028747135266249776D0; (* e *)


<* UNUSED *>
CONST
  LN4 =        1.38629436111989061883446424291635313615D0; (* ln(4) *)
  Root2ByE =   0.85776388496070679648018964127877247812D0; (* sqrt(2/e) *)
  Root2 =      1.41421356237309504880168872420969807857D0; (* sqrt(2) *)

(*======================================*)
REVEAL
  RandomGen = PrivateRandomGen BRANDED OBJECT
  OVERRIDES
    init:=NIL;
    engine:=NIL;
    uniform:=Uniform;
    exponential:=Exponential;
    gaussian:=NormalDev;
    gamma:=GammaDev;
    dirichlet:=Dirichlet;
    poisson:=Poisson;
    binomial:=Binomial;
  END;
(*========================================*)
(*-------------------*)
PROCEDURE Uniform(self:RandomGen;
                  min:REAL64:=0.0D0;  (*from min*)
                  max:REAL64:=1.0D0   (*up to but not including max*)
                  ):REAL64            (*return uniform deviate*)
                  (* RAISES{xUtils.Error}  using this here and in the methods declarations let the compiler believe that procedure and method definitions do not match and thus it leads to the error: "procedure redefined (Uniform)" *)
                  =
VAR
  t:REAL64;
BEGIN
  t:=self.engine();
  IF min=Min AND max=Max THEN RETURN t; END;

  IF min>=max THEN
    RAISE Error(Err.out_of_range);
  END;

  RETURN min + t*(max-min);
END Uniform;
(*-------------------*)
PROCEDURE Exponential(self:RandomGen):REAL64=
(*exponential, mean=1 *)
BEGIN
  RETURN -R.log(self.engine());
END Exponential;
(*-------------------*)
(**********************
PROCEDURE Gaussian1(self:RandomGen):REAL64=
(*gaussian, mean=0, var=1 *)
(*based on NR92*)
VAR
  v1,v2,Rsq,tmp,result:REAL64;
BEGIN
  IF NOT self.start THEN
    self.start:=TRUE;
    RETURN self.gauss_y;
  END;

  REPEAT
    v1:=R.Two*self.engine(self) - R.One;
    v2:=R.Two*self.engine(self) - R.One;
    Rsq:=v1*v1 + v2*v2;
  UNTIL (Rsq > R.Zero) AND (Rsq < R.One);
  tmp:=R.sqrt(-R.Two*R.log(Rsq))/Rsq;
  result:=v1*tmp;
  self.gauss_y:=v2*tmp;
  self.start:=FALSE;
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
PROCEDURE NormalDev(self:RandomGen) : REAL64 =
  VAR
    v,u,w,x,sum : REAL64;
  BEGIN
    u := self.uniform();
    IF u <= 0.8638D0 THEN
      v := self.uniform(-1.0D0, 1.0D0);
      w := self.uniform(-1.0D0, 1.0D0);
      x := 2.3153508D0 * u - 1.0D0 + v + w;
      RETURN x;
    ELSIF u <= 0.9745D0 THEN
      v := self.uniform();
      x := 1.5D0 * (v-1.0D0 + 9.0334237D0 * (u - 0.8638D0));
      RETURN x;
      (* we only get here with probability 0.0255: *)
    ELSIF u > 0.9973002D0 THEN
      REPEAT
        v := self.uniform();
        w := self.uniform();
        x := 4.5D0 - Math.log(w);
      UNTIL x*v*v <= 4.5D0;
      x := LongFloat.CopySign( Math.sqrt(x+x) , u - 0.9986501D0 );
      RETURN x;
    ELSE
      REPEAT
        x := self.uniform(-3.0D0, 3.0D0);
        u := self.uniform();
        v := ABS(x);
        w := 3.0D0-v;
        w := 6.6313339D0 * w*w;
        sum := 0.0D0;
        IF v < 1.5D0 THEN sum := 6.0432809D0 * (1.5D0 - v); END;
        IF v < 1.0D0 THEN sum := sum + 13.2626678D0 * (3.0D0 - v*v) - w; END;
      UNTIL u <= 49.0024445D0 * Math.exp(-v*v*0.5D0) - sum - w;
      RETURN x;
    END;
  END NormalDev;
(*-------------------*)
(***************************************
PROCEDURE Gamma1(self:RandomGen;
                event:[1..LAST(INTEGER)]):REAL64=
(*gamma, waiting time for event in Poisson process, mean=1*)
(*based on NR92*)
CONST
  cutoff=7;
VAR
  x,v1,v2,tanU,a0,x0,ratio:REAL64;
BEGIN
  IF event < cutoff THEN
    x:=R.One;
    FOR i:=1 TO event DO
      x:=x*self.engine(self);
    END;
    x:=-R.log(x);
  ELSE
    x0:=FLOAT(event-1,REAL64);
    a0:=R.sqrt(R.Two*x0+R.One);
    REPEAT
      REPEAT
        REPEAT
          v1:=R.Two*self.engine(self)-R.One;
          v2:=self.engine(self);
        UNTIL (v1*v1+v2*v2) <= R.One; (*within unit half-circle*)
        tanU:=v2/v1;
        x:=a0*tanU+x0;
      UNTIL x > R.Zero;  (*within positive probabilities*)
      ratio:=(R.One+tanU*tanU)*R.exp(x0*R.log(x/x0) - a0*tanU);
    UNTIL self.engine(self) > ratio;
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
PROCEDURE GammaDev(self:RandomGen;
                   a : REAL64) : REAL64 =
  BEGIN
    <* ASSERT a>0.0D0 *>
    IF a<1.0D0 THEN
      VAR
        u0,u1,x : REAL64;
      BEGIN
        LOOP
          u0 :=  self.uniform();
          u1 :=  self.uniform();
          IF (a+EulerE)*u0>EulerE THEN
            x := -Math.log((a+EulerE)*(1.0D0-u0)/(a*EulerE));
            IF u1 <= Math.pow(x, a-1.0D0) THEN
              <* ASSERT x>=0.0D0 *>
              RETURN x;
            END;
          ELSE
            x := Math.pow((a+EulerE)*u0/EulerE, 1.0D0/a);
            IF u1<=Math.exp(-x) THEN
              <* ASSERT x>=0.0D0 *>
              RETURN x;
            END;
          END;
        END; (*LOOP*)
      END;
    ELSIF a>1.0D0 THEN
      (* Cheng+Feast algorithm [CACM 23,7 (1980) 389-394?] for a>1: *)
      VAR
        c1 := a-1.0D0;
        c2 := (a-1.0D0/(6.0D0*a))/c1;
        c3 := 2.0D0/c1;
        c4 := c3+2.0D0;
        c5 := 1.0D0/Math.sqrt(a);
        u1, u2, w: REAL64;
      BEGIN
        LOOP
          REPEAT
            u1 :=  self.uniform();
            u2 :=  self.uniform();
            IF a>2.5D0 THEN
              u1 := u2 + c5*(1.0D0-1.86D0*u1);
            END;
          UNTIL 0.0D0<u1 AND u1<1.0D0;
          w := c2*u2/u1;
          IF c3*u1+w+1.0D0/w <= c4
             OR
             c3*Math.log(u1) - Math.log(w) + w < 1.0D0 THEN
            w := w*c1;
            <* ASSERT w>=0.0D0 *>
            RETURN w;
          END;
        END; (*LOOP*)
      END;
    ELSE (* a=1, just use exponential: *)
      RETURN -Math.log( self.uniform() );
    END;
  END GammaDev;
(*----------------------*)
(** Will generate a sample from a Dirichlet distribution
 * with parameters p[].
 * Follows L.Devroye: Non-uniform random variate generation,
 * Springer 1986.   p[] is overwritten by the Dirichlet deviate.
 *)
PROCEDURE Dirichlet(self:RandomGen;
                    p:R.Array) =
  VAR
    t, sum : REAL64;
    n1:=FIRST(p^); nn:=LAST(p^);
  BEGIN
    sum := 0.0D0;
    FOR n:=nn TO n1 BY -1 DO
      t := GammaDev(self, p[n] );
      p[n] := t;
      sum := sum + t;
    END;
    t := 1.0D0/sum;
    FOR n:=nn TO n1 BY -1 DO
      p[n] := p[n] * t;
    END;
  END Dirichlet;


(*-------------------*)
PROCEDURE Poisson(self:RandomGen;
                     m:REAL64    (*mean*)
                     ):REAL64=
(*Poisson, integer returned as real*)
CONST ftn = Module & "Poisson";
BEGIN
  RAISE Error(Err.not_implemented);
  RETURN R.Zero;
END Poisson;
(*-------------------*)
PROCEDURE Binomial(self:RandomGen;
                     p:REAL64;  (*probability*)
                     n:INTEGER  (*trials*)
                     ):REAL64=
(*Binomial, returned as real*)
CONST ftn = Module & "Binomial";
BEGIN
  RAISE Error(Err.not_implemented);
  RETURN R.Zero;
END Binomial;

(*==========================*)
BEGIN
END RandomBasic.
