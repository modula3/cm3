UNSAFE INTERFACE NewUOA;

<*EXTERNAL newuoa_*>
PROCEDURE Call(N, NPT         : ADDRESS;
               X              : ADDRESS;
               RHOBEG, RHOEND : ADDRESS;
               IPRINT         : ADDRESS;
               MAXFUN         : ADDRESS;
               W              : ADDRESS;
               CALFUN         : Func;
               F              : ADDRESS;
               INFO           : ADDRESS;
               ITAG           : ADDRESS);

TYPE
  Func = PROCEDURE(N : ADDRESS; X : ADDRESS; ITAG : ADDRESS) : LONGREAL;
  
END NewUOA.
  
