INTERFACE UnitTestSignal;

IMPORT UnitTestNumeric;

IMPORT LongRealBasic AS R, LongRealSignal AS S, Random;


TYPE
  T <: Public;
  Public = UnitTestNumeric.T OBJECT
           METHODS
             signalMatch (x, y: S.T; tol: R.T; ): BOOLEAN;
           END;

PROCEDURE RandomSignal (rnd: Random.T; first: INTEGER; number: CARDINAL; ):S.T;

END UnitTestSignal.
