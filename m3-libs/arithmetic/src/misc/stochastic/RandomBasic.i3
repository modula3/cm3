INTERFACE RandomBasic;
(*Copyright (c) 1996, m3na project

Abstract: Ramdom number generators


3/16/96  Harry George    Initial version (basic structure)
3/17/96  Warren Smith    Gamma, Gaussian (Normal), and Dirichlet deviates

To do: (Lemming)
 - Check if the Error exception of the 'uniform' method
   can be catched in the procedures which make use of 'uniform'
*)
IMPORT LongRealBasic AS R,
       LongRealTrans AS RT,
       xUtils;
FROM xUtils IMPORT Error;

(*==========================*)
CONST
  (*---safe boundaries for 0.0 .. 1.0 range---*)
  Min  = RT.Eps*5.0D0;
  Max  = 1.0D0 - Min;

TYPE
  RandomGen <: PublicRandomGen;
  PublicRandomGen = OBJECT
  METHODS
    init(seed  :[FIRST(INTEGER)..-1]:=-1
              ):RandomGen RAISES {Error};

    engine():R.T; (*raw engine; returns Min..Max*)

    uniform(min:R.T:=Min;  (*from min*)
            max:R.T:=Max   (*to max*)
              ):R.T
              (*RAISES{xUtils.Error}*);        (*return uniform deviate*)


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

    poisson(m:R.T    (*mean*)
           ):R.T;    (*Poisson, integer returned as real*)

    binomial(p:R.T;  (*probability*)
             n:INTEGER  (*trials*)
            ):R.T;   (*Binomial, returned as real*)
  END;


(*==========================*)
END RandomBasic.
