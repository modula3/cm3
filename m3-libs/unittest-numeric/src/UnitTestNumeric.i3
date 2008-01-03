INTERFACE UnitTestNumeric;

IMPORT UnitTestAtom;

IMPORT LongRealBasic  AS R,
       LongRealVector AS V,
       LongRealMatrix AS M;


TYPE
  T <: Public;
  Public = UnitTestAtom.T OBJECT
           METHODS
             scalarMatch (x, y: R.T; tol: R.T; ): BOOLEAN;
             vectorMatch (x, y: V.T; tol: R.T; ): BOOLEAN;
             matrixMatch (x, y: M.T; tol: R.T; ): BOOLEAN;
           END;

END UnitTestNumeric.
