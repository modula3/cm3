INTERFACE ProcessExponential;

IMPORT Signal;

PROCEDURE Do (length: CARDINAL; halflife: LONGREAL; ): Signal.RefArray;

TYPE
  T = Signal.T OBJECT
        factor, elongation: LONGREAL;  (* Internal variables, don't touch! *)
      METHODS
        init (halflife: LONGREAL; ): T := Init;
      OVERRIDES
        get := Get;
      END;

(* Don't call them, they are private *)
PROCEDURE Init (SELF: T; halflife: LONGREAL; ): T;

PROCEDURE Get (SELF: T; ): LONGREAL;

END ProcessExponential.
