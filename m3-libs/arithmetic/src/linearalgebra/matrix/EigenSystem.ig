GENERIC INTERFACE EigenSystem(RT, V, CV, M);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Routines to solve eigenvalue problems. *)

FROM Arithmetic IMPORT Error;

TYPE
  EigenPair = RECORD
                value : RT.T;
                vector: V.T;
              END;

PROCEDURE PowerMethod
  (A: M.T; tol := RT.Eps * FLOAT(100, RT.T); maxiter: CARDINAL := 100; ):
  EigenPair RAISES {Error};
(* May raise Arith.ErrorNoConvergence *)

PROCEDURE SquareMethod
  (A: M.T; tol := RT.Eps * FLOAT(100, RT.T); maxiter: CARDINAL := 100; ):
  EigenPair RAISES {Error};
(* May raise Arith.ErrorNoConvergence *)


TYPE
  EVFlag = {SchurVectors};
  EVFlagSet = SET OF EVFlag;

  EV = RECORD
         eigenvalues: CV.T;
         upperTri   : M.T;
         schur: M.T;             (* initalized if schurVector flag is
                                    set *)
       END;

(* Unoptimised translations from

   Wilkinson+Reinsch, Linear Algebra, Grundlehren der mathematischen
   Wissenschaften in Einzeldarstellungen, Band 186, Springer Verlag

   contributed by Thomas Brupbacher *)


PROCEDURE Jacobi (VAR a        : M.T;
                      dim      : CARDINAL;
                  VAR d        : V.T;
                  VAR vects    : M.T;
                  VAR nrot     : CARDINAL;
                      eigenvals             := FALSE; );
(*
   Solve the real symmetric eigenvalue problem by the algorithm of Jacobi.
   The routine has been tested against the first example given in
   Wilkinson/Reinsch and gives the same results.

  It must hold

  NUMBER(a^) >= n AND NUMBER(a[0]) >= n
  NUMBER(d^) >= n
  NUMBER(v^) >= n AND NUMBER(v[0]) >= n
*)




PROCEDURE EigenSort (VAR vects: M.T; VAR vals: V.T; );

PROCEDURE Tred1 (n: CARDINAL; VAR a: M.T; VAR d, e, e2: V.T; );

PROCEDURE Tred2 (n: CARDINAL; VAR a: M.T; VAR d, e: V.T; );

PROCEDURE Trbak1
  (n: CARDINAL; a: M.T; d, e: V.T; VAR z: M.T; m1, m2: CARDINAL; );

PROCEDURE Trbak3
  (n: CARDINAL; a: V.T; d, e: V.T; VAR z: M.T; m1, m2: CARDINAL; );

PROCEDURE Tql1 (VAR d, e: V.T; ) RAISES {Error};
(* May raise Arith.ErrorNoConvergence *)

PROCEDURE Tql2 (VAR d, e: V.T; VAR z: M.T; ) RAISES {Error};
(* May raise Arith.ErrorNoConvergence *)

END EigenSystem.
