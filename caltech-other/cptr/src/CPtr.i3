INTERFACE CPtr;
IMPORT Ctypes;
IMPORT Thread, Wr; (* Exceptions *)
IMPORT Word;

CONST
  Brand = "CPtr version 1.00";

TYPE
  T <: Public;

  Public = OBJECT
  METHODS
    get() : Ctypes.void_star;
  END;

(* this routine will FIRST lock clientMu, THEN lock its own mu *)
PROCEDURE Wrap( x : Ctypes.void_star; 
                destroy : PROCEDURE( x : Ctypes.void_star );
                clientMu : MUTEX;
                VAR hadIt : BOOLEAN) : T;

PROCEDURE PrintMems() RAISES { Thread.Alerted, Wr.Failure };

(* Hash and Equal are provided so that *)
(* CPtr may be used in generic interfaces *)

PROCEDURE Hash(self : T) : Word.T;
PROCEDURE Equal(a,b : T) : BOOLEAN;

END CPtr.
  
