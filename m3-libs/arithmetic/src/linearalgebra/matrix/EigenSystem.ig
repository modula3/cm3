GENERIC INTERFACE EigenSystem(RT, V, M, LA);
(*Arithmetic for Modula-3, see doc for details

   Abstract: Routines to solve eigenvalue problems. *)

FROM NADefinitions IMPORT Error;

TYPE
  EigenPair = RECORD
                value : RT.T;
                vector: V.T;
              END;

PROCEDURE PowerMethod (A      : M.T;
                       tol               := RT.Eps * FLOAT(100, RT.T);
                       maxiter: CARDINAL := 100;                       ):
  EigenPair RAISES {Error};

PROCEDURE SquareMethod (A      : M.T;
                        tol               := RT.Eps * FLOAT(100, RT.T);
                        maxiter: CARDINAL := 100;                       ):
  EigenPair RAISES {Error};

TYPE
  EVFlag = LA.EVFlag;
  EVFlagSet = LA.EVFlagSet;

  EV = LA.EV;

CONST EigenValues = LA.EigenValues;


(* Unoptimised translations from

   Wilkinson+Reinsch, Linear Algebra, Grundlehren der mathematischen
   Wissenschaften in Einzeldarstellungen, Band 186, Springer Verlag

   contributed by Thomas Brupbacher *)

EXCEPTION
  ArrayTooSmall;
  NoConvergence;
  ArraySizesDontMatch;

PROCEDURE Jacobi (VAR a        : M.T;
                      dim      : INTEGER;
                  VAR d        : V.T;
                  VAR vects    : M.T;
                  VAR nrot     : INTEGER;
                      eigenvals            := FALSE)
  RAISES {ArrayTooSmall};

PROCEDURE EigenSort (VAR vects: M.T; VAR vals: V.T)
  RAISES {ArraySizesDontMatch};

PROCEDURE Tred1 (n: CARDINAL; VAR a: M.T; VAR d, e, e2: V.T)
  RAISES {ArraySizesDontMatch};

PROCEDURE Tred2 (n: CARDINAL; VAR a: M.T; VAR d, e: V.T)
  RAISES {ArraySizesDontMatch};

PROCEDURE Trbak1 (    n     : CARDINAL;
                      a     : M.T;
                      d, e  : V.T;
                  VAR z     : M.T;
                      m1, m2: CARDINAL  ) RAISES {ArraySizesDontMatch};

PROCEDURE Trbak3 (    n     : CARDINAL;
                      a     : V.T;
                      d, e  : V.T;
                  VAR z     : M.T;
                      m1, m2: CARDINAL  ) RAISES {ArraySizesDontMatch};

PROCEDURE Tql1 (VAR d, e: V.T) RAISES {ArraySizesDontMatch, NoConvergence};

PROCEDURE Tql2 (VAR d, e: V.T; VAR z: M.T)
  RAISES {ArraySizesDontMatch, NoConvergence};

END EigenSystem.
