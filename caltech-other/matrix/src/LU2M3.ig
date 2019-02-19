GENERIC INTERFACE LU2M3(M);

PROCEDURE BackSubstitute(READONLY m : M.M; 
                         READONLY indx : REF ARRAY OF INTEGER; 
                         VAR b : M.V);

END LU2M3.
