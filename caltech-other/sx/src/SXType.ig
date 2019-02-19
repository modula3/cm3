(* $Id$ *)

GENERIC INTERFACE SXType(Elem);

FROM SX IMPORT Uninitialized;
IMPORT SXRoot;
IMPORT Time;

(* 
   "Spreadsheet expressions" generic interface.

   The interface Elem must export a type T and PROCEDUREs Equal and
   Compare, as well as a constant Brand, according to the usual rules.

   locking order: SX.mu must be locked after self.mu in all cases.

   See SXRoot.i3 for more (Elem-type-independent) details.
*)

TYPE 
  Base = Elem.T;

  T <: Public;

  Public = SXRoot.T OBJECT METHODS
    value() : Elem.T RAISES { Uninitialized };

    waitFor(val : Elem.T);

    update(newValue : Elem.T; when : Time.T) : BOOLEAN;
    (* 
       used only by routines implementing a node;
       DOES NOT propagate up the tree.
       Returns TRUE if value was changed, FALSE otherwise. 

       self.mu may not be locked when calling this
    *)

    updateLocked(newValue : Elem.T; when : Time.T) : BOOLEAN;
    (* same as update, but self.mu must be locked *)

    (* numUpdates, attachName, initialized methods moved to SXRoot.T *)

    setValidator(validator : Validator);
    (* the validator can validate the value settings.  This is really
       only for debugging, we don't do anything in this module with the
       return value of the validator *)
  END;

  Var <: PublicVar;
  
  PublicVar = T OBJECT METHODS
    set(newValue : Elem.T; when := FIRST(Time.T));
    (* call from outside LOCK self.mu --- is this really right? *)

    setLocked(newValue : Elem.T; when := FIRST(Time.T));
    (* call this is you are already holding self.mu. *)

    initVal(initValue : Elem.T) : Var; 
    (* initialize with value, and updates = 1 *)
    
  END;

  Const <: PublicConst;

  PublicConst = T OBJECT METHODS
    init(value : Elem.T) : Const;
  END;

  Validator = OBJECT METHODS
    validQ(new : Elem.T) : BOOLEAN; 
    (* validQ is called on every set, whether the value of the Var is
       changed or not.  It is called with SX.mu and v.mu already locked. *)
  END;

CONST Brand = "SXType(" & Elem.Brand & ")";

CONST BaseEqual = Elem.Equal;

PROCEDURE BaseCompare(a, b : Base) : INTEGER;
  (* same as Elem.Compare, but type-wrapped *)

PROCEDURE NewConst(value : Elem.T) : Const;
  (* make a new constant *)

END SXType.

