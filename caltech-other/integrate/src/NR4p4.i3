INTERFACE NR4p4;
IMPORT LRFunction AS Function;

EXCEPTION TooManyIters;
  
PROCEDURE Qromo(func   : Function.T;
                a, b   : LONGREAL;
                choose : Chooser) : LONGREAL RAISES { TooManyIters };
  (* 
     qromo from NRF77 para 4.4

     Romberg integration (for improper integrals)
  *)
  
TYPE Chooser = PROCEDURE (f     : Function.T;
                          a, b  : LONGREAL;
                          VAR s : LONGREAL;
                          n     : CARDINAL);
     
PROCEDURE Midpoint(f     : Function.T;
                   a, b  : LONGREAL;
                   VAR s : LONGREAL; (* OUT *)
                   n     : CARDINAL);
  (* 
     midpnt from NRF77 para 4.4 
     
     Radix-3 midpoint integration algorithm
  *)

PROCEDURE QromoMidpoint(func   : Function.T;
                        a, b   : LONGREAL) : LONGREAL RAISES { TooManyIters };
  (* 
     Qromo( ... , Midpoint) 

     Combination of the above
  *)
  
CONST Brand = "Romberg";

END NR4p4.
     
