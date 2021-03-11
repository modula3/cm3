(* $Id$ *)

INTERFACE SXTimer;
IMPORT SXLongReal;

(* similar to facilities of SXTime but uses only a single thread *)

TYPE
  T <: Public; 

  Private <: SXLongReal.T;  

  Public = Private OBJECT METHODS
    init(granularity := 1.0d0) : T;

    (* initialize a T that updates about every granularity seconds 
       with the current value of XTime.Now().

       It will initially hold the time of XTime.Now() as of some time
       during the call to init().

       Thus it CANNOT raise SX.Uninitialized when inspected.
    *)
  END;

CONST Brand = "SXTimer";

END SXTimer.

