(* -*- Modula-3 -*-                                                      *)
(*                                                                       *)
(* Routines to solve eigenvalue problems.                                *)
(* Unoptimised translations from                                         *)
(* Wilkinson+Reinsch, Linear Algebra, Grundlehren der mathematischen     *)
(* Wissenschaften in Einzeldarstellungen, Band 186, Springer Verlag      *)
(*                                                                       *)
(* contributed by Thomas Brupbacher                                      *)
(*                                                                       *)
(*                                                                       *)

GENERIC INTERFACE EigenSystem(M,V,RT);

FROM NADefinitions IMPORT Error;

EXCEPTION
  ArrayTooSmall;
  NoConvergence;
  ArraySizesDontMatch;

PROCEDURE PowerMethod(     A       : M.T;
                       VAR v       : V.T;
					       tol     := RT.Eps*FLOAT(100,RT.T);
                           maxiter : CARDINAL := 100;
						         ) : RT.T RAISES {Error};

PROCEDURE SquareMethod(     A       : M.T;
                       VAR v       : V.T;
					       tol     := RT.Eps*FLOAT(100,RT.T);
                           maxiter : CARDINAL := 100;
						         ) : RT.T RAISES {Error};

PROCEDURE Jacobi( VAR a         :M.T;
                  dim           :INTEGER;
                  VAR d         :V.T;
                  VAR vects     :M.T;
                  VAR nrot      :INTEGER;
                  eigenvals     := FALSE)
    RAISES {ArrayTooSmall};
PROCEDURE EigenSort(VAR vects: M.T;
                    VAR vals: V.T)
    RAISES {ArraySizesDontMatch};
PROCEDURE Tred1(n: CARDINAL;
                VAR a: M.T;
                VAR d,e,e2: V.T)
    RAISES {ArraySizesDontMatch};

PROCEDURE Tred2(n: CARDINAL;
                VAR a: M.T;
                VAR d,e: V.T)
    RAISES {ArraySizesDontMatch};

PROCEDURE Trbak1(n: CARDINAL;
                 a: M.T;
                 d,e: V.T;
                 VAR z: M.T;
                 m1,m2: CARDINAL)
    RAISES {ArraySizesDontMatch};

PROCEDURE Trbak3(n: CARDINAL;
                 a: V.T;
                 d,e: V.T;
                 VAR z: M.T;
                 m1,m2: CARDINAL)
    RAISES {ArraySizesDontMatch};

PROCEDURE Tql1(VAR d,e: V.T)
    RAISES {ArraySizesDontMatch,NoConvergence};

PROCEDURE Tql2(VAR d,e: V.T;
               VAR z: M.T)
    RAISES {ArraySizesDontMatch,NoConvergence};

END EigenSystem.
