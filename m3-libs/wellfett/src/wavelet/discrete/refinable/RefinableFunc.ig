GENERIC INTERFACE RefinableFunc(R, M, Eigen, S);

IMPORT NADefinitions AS NA;

PROCEDURE RadicBandMatrix (mask: S.T; shift: CARDINAL := 2): M.T;
PROCEDURE TransitionMatrix (mask: S.T; shift: CARDINAL := 2): M.T;

PROCEDURE TransitionEV (mask: S.T): Eigen.EV RAISES {NA.Error};
PROCEDURE TransitionSpecRad (mask: S.T): R.T RAISES {NA.Error};

PROCEDURE Refine (start, mask: S.T; levels: CARDINAL; shift: CARDINAL := 2):
  S.T;

END RefinableFunc.
