INTERFACE RandomBasic;
(*Copyright (c) 1996, m3na project

Abstract: Ramdom number generators


3/16/96  Harry George    Initial version (basic structure)
3/17/96  Warren Smith    Gamma, Gaussian (Normal), and Dirichlet deviates

To do: (Lemming)
 - Check if the Error exception of the 'uniform' method
   can be catched in the procedures which make use of 'uniform'
 - implement Geometric, Poisson distribution
*)
IMPORT LongRealBasic AS R,
       LongRealTrans AS RT,
       Word AS W;
FROM xUtils IMPORT Error;

(*==========================*)
CONST
  (*---safe boundaries for 0.0 .. 1.0 range---*)
  Min  = RT.Eps*5.0D0;
  Max  = R.One - Min;

TYPE
  T <: TPublic;
  TPublic = OBJECT
  METHODS
(*
    init(seed  :[FIRST(INTEGER)..-1]:=-1
              ):T RAISES {Error};
*)
    (*generate different types of random values,
      the routines may convert types from the actual ones
      of the engine behind*)
    generateBoolean():BOOLEAN;
    generateWord():W.T;
    generateReal():R.T;

    uniform(min:R.T:=Min;  (*from min*)
            max:R.T:=Max   (*to max*)
              ):R.T
              RAISES{Error};        (*return uniform deviate*)

    exponential():R.T;   (*exponential, mean=1 *)

    (*-----------------------------------------*)
    (* Gaussian or Normal distributions have   *)
    (* density = 1/sqrt(2*pi) * exp(-x*x/2).   *)
    (* To get mean=m and stddeviation=s:       *)
    (*     value:=m + s*rand.gaussian();       *)
    (*-----------------------------------------*)
    gaussian():R.T;     (*gaussian, mean=0, var=1 *)

    gamma(a : R.T) : R.T;

    dirichlet(p:R.Array);

(*
    poisson(m:R.T    (*mean*)
           ):R.T;    (*Poisson, integer returned as real*)
*)

    (*slow implementation*)
    binomial(p:R.T;       (*probability of successful trial*)
             n:CARDINAL;  (*number of trials*)
             ):CARDINAL;  (*number of successful trials*)
  END;

  TBoolean <: TBooleanPublic;
  TBooleanPublic = T OBJECT
  METHODS
    engine():BOOLEAN;
  END;

  TWord <: TWordPublic;
  TWordPublic = T OBJECT
  METHODS
    engine():W.T;
  END;

  TReal <: TRealPublic;
  TRealPublic = T OBJECT
  METHODS
    engine():R.T; (*raw engine; returns Min..Max*)
  END;

(*==========================*)
END RandomBasic.
