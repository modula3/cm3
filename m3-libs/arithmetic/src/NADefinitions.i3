INTERFACE NADefinitions;
(*Copyright (c) 1996, Harry George

   Abstract: Global definitions used by all m3na modules. *)

EXCEPTION Error(Err);

TYPE
  Err = {bad_size                (*e.g., vector sizes mismatched*)
         , b1_too_small          (*in tridiagonal, rewrite for n-1 eqns*)
         , divide_by_zero        (*x/0 condition detected*)
         , indivisible           (*division is not possible in the
                                    considered field, DivMod should always
                                    work*)
         , need_more_data        (*e.g., more data points in statistics*)
         , not_bracketed         (*given x1,x2 do not bracket root*)
         , not_converging        (*e.g., eps or maxiter too small*)
         , not_implemented       (*it's just a stub*)
         , out_of_range          (*parameter is out of range*)
         , unit_mismatch         (*physical units of values didn't meet the
                                    requirements of the operation*)
        };

  ErrorRoot = OBJECT END;
  ErrorBadParameters = ErrorRoot OBJECT END;
  ErrorOperationAborted = ErrorRoot OBJECT END;

  ErrorOutOfRange = ErrorBadParameters OBJECT END;
  ErrorDivideByZero = ErrorBadParameters OBJECT END;
  ErrorSizeMismatch = ErrorBadParameters OBJECT END;
  ErrorUnitMismatch = ErrorBadParameters OBJECT END;

  ErrorOverflow = ErrorOperationAborted OBJECT END;
  ErrorIndivisible = ErrorOperationAborted OBJECT END;
  ErrorNotConverging = ErrorOperationAborted OBJECT END;

(*IMPORT Wr, Thread;*)

VAR verbosity: [0 .. 3];

PROCEDURE debug (level: [0 .. 3]; ftn, str: TEXT);
(* RAISES {Thread.Alerted, Wr.Failure};

   These exceptions may be raised but because debug output shouldn't be
   present in the final product there is no sense to adapt all procedure
   signatures to the additional exceptions. *)

PROCEDURE err (ftn: TEXT; code: Err; errmsg: TEXT := NIL) RAISES {Error};

END NADefinitions.
