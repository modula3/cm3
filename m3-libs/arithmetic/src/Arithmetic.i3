INTERFACE Arithmetic;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Global definitions used by all arithmetic modules. *)

IMPORT AtomList;

EXCEPTION Error(ErrorRoot);

TYPE
  (* Error objects may carry more information than a simple enumeration
     value.  Especially the LAPACK wrappers may benefit from the extended
     facilities.  In future we might add a dynamic method to the objects
     which generates an error message text or similar services. *)

  ErrorRoot = AtomList.T BRANDED OBJECT
              METHODS
                init (msg: TEXT := ""; oldErr: ErrorRoot := NIL; ):
                      ErrorRoot := ErrorInit;
              END;
  ErrorBadParameters = ErrorRoot BRANDED OBJECT END;
  ErrorOperationAborted = ErrorRoot BRANDED OBJECT END;

  ErrorOutOfRange =
    ErrorBadParameters BRANDED OBJECT END; (*parameter is out of range*)
  ErrorDivisionByZero =
    ErrorBadParameters BRANDED OBJECT END; (*x/0 condition detected*)
  ErrorSizeMismatch =
    ErrorBadParameters BRANDED OBJECT END; (*e.g., vector sizes
                                              mismatched*)
  ErrorUnitMismatch =
    ErrorBadParameters BRANDED OBJECT END; (*physical units of values
                                              didn't meet the requirements
                                              of the operation*)
  ErrorNeedMoreData =
    ErrorBadParameters BRANDED OBJECT END; (*e.g., more data points in
                                              statistics*)
  ErrorNotBracketed =
    ErrorBadParameters BRANDED OBJECT END; (*given x1,x2 do not bracket
                                              root*)

  ErrorOverflow = ErrorOperationAborted BRANDED OBJECT END;
  ErrorIndivisible =
    ErrorOperationAborted BRANDED OBJECT END; (*division is not possible in
                                                 the considered field,
                                                 DivMod should always
                                                 work*)
  ErrorAlmostZero =
    ErrorOperationAborted BRANDED OBJECT END; (*in tridiagonal, rewrite for
                                                 n-1 eqns*)
  ErrorNoConvergence =
    ErrorOperationAborted BRANDED OBJECT END; (*e.g., eps or maxiter too
                                                 small*)

PROCEDURE ErrorInit (err   : ErrorRoot;
                     msg   : TEXT        := "";
                     oldErr: ErrorRoot   := NIL; ): ErrorRoot;

END Arithmetic.
