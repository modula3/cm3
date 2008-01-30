(*------------------------------------------------------------------------*)
INTERFACE Confirmation;

(*------------------------------------------------------------------------*)
TYPE
  Closure = OBJECT
  METHODS
    okay(t : TEXT) : BOOLEAN;
    (* intended semantic: display TEXT `t' and ask the user for 
       confirmation. Return TRUE is she/he confirms, FALSE otherwise.
    *)
  END;

  StdIOClosure = Closure OBJECT
  OVERRIDES
    okay := OkayStdio;
    (* default is a dialog on stderr/stdin *)
  END;

  ExternalClosure = Closure OBJECT
    cmd : TEXT;
  OVERRIDES
    okay := OkayExternal;
    (* default is a dialog on stderr/stdin *)
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE OkayStdio(self : StdIOClosure; msg : TEXT) : BOOLEAN;
  (* Display `msg' on stderr and return TRUE if the user answers 
     `y' or `yes'.
  *)

(*---------------------------------------------------------------------------*)
PROCEDURE OkayExternal(self : ExternalClosure; msg : TEXT) : BOOLEAN;
  (* Display `msg' using `cmd' and return TRUE if the user answers 
     `y' or `yes'.
  *)

(*---------------------------------------------------------------------------*)
PROCEDURE Get(msg : TEXT) : BOOLEAN;
  (* Get confirmation using the default method. *)

(*---------------------------------------------------------------------------*)
PROCEDURE SetDefault(cl : Closure);
  (* Define the default way to get a confirmation. *)

END Confirmation.

