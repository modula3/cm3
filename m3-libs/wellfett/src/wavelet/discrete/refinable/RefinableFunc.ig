GENERIC INTERFACE RefinableFunc(M, S);

PROCEDURE RadicBandMatrix (mask: S.T; shift: CARDINAL := 2): M.T;
PROCEDURE TransitionMatrix (mask: S.T; shift: CARDINAL := 2): M.T;

PROCEDURE Refine (start, mask: S.T;
                  numLevels  : CARDINAL;
                  shift      : CARDINAL   := 2): S.T;

END RefinableFunc.
