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

INTERFACE EigenSystem;

EXCEPTION
  ArrayTooSmall;
  NoConvergence;
  ArraySizesDontMatch;

(*FROM Ctypes IMPORT int;*)
PROCEDURE Jacobi( VAR a         :REF ARRAY OF ARRAY OF LONGREAL;
                  dim           :INTEGER;
                  VAR d         :REF ARRAY OF LONGREAL;
                  VAR vects     :REF ARRAY OF ARRAY OF LONGREAL;
                  VAR nrot      :INTEGER;
                  eigenvals     := FALSE)
    RAISES {ArrayTooSmall};
PROCEDURE EigenSort(VAR vects: REF ARRAY OF ARRAY OF LONGREAL;
                    VAR vals: REF ARRAY OF LONGREAL)
    RAISES {ArraySizesDontMatch};
PROCEDURE Tred1(n: CARDINAL;
                VAR a: REF ARRAY OF ARRAY OF LONGREAL;
                VAR d,e,e2: REF ARRAY OF LONGREAL)
    RAISES {ArraySizesDontMatch};

PROCEDURE Tred2(n: CARDINAL;
                VAR a: REF ARRAY OF ARRAY OF LONGREAL;
                VAR d,e: REF ARRAY OF LONGREAL)
    RAISES {ArraySizesDontMatch};

PROCEDURE Trbak1(n: CARDINAL;
                 a: REF ARRAY OF ARRAY OF LONGREAL;
                 d,e: REF ARRAY OF LONGREAL;
                 VAR z: REF ARRAY OF ARRAY OF LONGREAL;
                 m1,m2: CARDINAL)
    RAISES {ArraySizesDontMatch};

PROCEDURE Trbak3(n: CARDINAL;
                 a: REF ARRAY OF LONGREAL;
                 d,e: REF ARRAY OF LONGREAL;
                 VAR z: REF ARRAY OF ARRAY OF LONGREAL;
                 m1,m2: CARDINAL)
    RAISES {ArraySizesDontMatch};

PROCEDURE Tql1(VAR d,e: REF ARRAY OF LONGREAL)
    RAISES {ArraySizesDontMatch,NoConvergence};

PROCEDURE Tql2(VAR d,e: REF ARRAY OF LONGREAL;
               VAR z: REF ARRAY OF ARRAY OF LONGREAL)
    RAISES {ArraySizesDontMatch,NoConvergence};

END EigenSystem.
