INTERFACE RandomBasic;
(*Copyright (c) 1996, m3na project

Abstract: Ramdom number generators


3/16/96  Harry George    Initial version (basic structure)
3/17/96  Warren Smith    Gamma, Gaussian (Normal), and Dirichlet deviates

To do: (Lemming)
 - Check if the Error exception of the 'uniform' method
   can be catched in the procedures which make use of 'uniform'
*)
FROM xReal64 IMPORT REAL64;
IMPORT xReal64 AS R, xUtils;
FROM xUtils IMPORT Error;

(*==========================*)
CONST
  (*---safe boundaries for 0.0 .. 1.0 range---*)
  Min  = R.EPS*5.0D0;
  Max  = 1.0D0 - Min;

TYPE
  RandomGen <: PublicRandomGen;
  PublicRandomGen = OBJECT
  METHODS
    init(seed  :[FIRST(INTEGER)..-1]:=-1
              ):RandomGen RAISES {Error};

    engine():REAL64; (*raw engine; returns Min..Max*)

    uniform(min:REAL64:=Min;  (*from min*)
            max:REAL64:=Max   (*to max*)
              ):REAL64
              (*RAISES{xUtils.Error}*);        (*return uniform deviate*)


    exponential():REAL64;   (*exponential, mean=1 *)

    (*-----------------------------------------*)
    (* Gaussian or Normal distributions have   *)
    (* density = 1/sqrt(2*pi) * exp(-x*x/2).   *)
    (* To get mean=m and stddeviation=s:       *)
    (*     value:=m + s*rand.gaussian();       *)
    (*-----------------------------------------*)
    gaussian():REAL64;     (*gaussian, mean=0, var=1 *)

    gamma(a : REAL64) : REAL64;

    dirichlet(p:R.Array);

    poisson(m:REAL64    (*mean*)
           ):REAL64;    (*Poisson, integer returned as real*)

    binomial(p:REAL64;  (*probability*)
             n:INTEGER  (*trials*)
            ):REAL64;   (*Binomial, returned as real*)
  END;


(*==========================*)
END RandomBasic.
