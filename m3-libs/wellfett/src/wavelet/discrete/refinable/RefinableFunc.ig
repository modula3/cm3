GENERIC INTERFACE RefinableFunc(M, S);

PROCEDURE RadicBandMatrix (mask: S.T; r: CARDINAL := 2): M.T;
(*Generate a r-adic band matrix, that is a square matrix where each row
   contains 'mask' shifted by 'r' relatively to the row above.*)

PROCEDURE TransitionMatrix (mask: S.T; r: CARDINAL := 2): M.T;
(*Generate the r-adic band matrix of the autocorrelation of 'mask'*)

PROCEDURE Refine (start, mask: S.T;
                  numLevels  : CARDINAL := 1;
                  r          : CARDINAL := 2; ): S.T;
(*Refine the mask 'start' 'numLevel' times with the mask 'mask' and
   up-sampling by factor 'r'.*)

END RefinableFunc.
