(* $Id$ *)

INTERFACE SXRoot;
(* just defines the SX.T type *)

TYPE 
  T <: Public;
  
  Public = OBJECT 

  METHODS
    init() : T;

    wait();
    (* routine will eventually return if the value has changed.
       Note that immediate (synchronous) return can't be guaranteed
       with this method. *)

    destroy();  (* a freeing mechanism, might want WeakRef instead *)

    uninitialize(); 
    (* set so that value() RAISES Uninitialized *)

    numUpdates() : CARDINAL;
    (* how many times updated? *)

    initialized() : BOOLEAN;
    (* TRUE if numUpdates > 0 *)

    attachName(name : TEXT);
    (* attach a name for debugging purposes *)

    getName() : TEXT;

    dependsOn() : Iterator;
    (* return dependencies, for debugging purposes *)

    type() : Type;
    (* more debugging *)

    debugInfo() : TEXT;
    (* type-dependent debugging *)
  END;

  Type = { Tree, Const, Var };

  Iterator = OBJECT METHODS next(VAR a : T) : BOOLEAN END;

CONST TypeNames = ARRAY Type OF TEXT { "Tree", "Const", "Var" } ;

(*
   note that this code can be very tricky!

   LOCK t DO t.wait(); < use t.value() >  END(*LOCK*);

   will NOT see a use of t.value() every time t is updated.
*)
END SXRoot.
