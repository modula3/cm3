(* $Id: CompilerHooks.i3,v 1.1 2009/06/28 11:38:26 mika Exp $ *)

INTERFACE CompilerHooks;
FROM Scheme IMPORT E;
IMPORT SchemeEnvironment;

TYPE 
  Frame = ARRAY OF REFANY;

  LocalProcedure = PROCEDURE (VAR args : ARRAY OF REFANY;
                              g : Getter; s : Setter; 
                              c : Copier; a : Adapter) : LocalProcedureResult 
    RAISES { E };

PROCEDURE CheckLengthAtLeast(READONLY a : Frame; len : CARDINAL) RAISES { E };

PROCEDURE CheckLengthAtMost(READONLY a : Frame; len : CARDINAL) RAISES { E };

(* the gyrations necessary to minimize heap allocations *)

TYPE
  Getter = PROCEDURE (up, pos : CARDINAL) : REFANY;
  (* get a variable with the stated offset from current frame *)

  Setter = PROCEDURE (up, pos : CARDINAL; to : REFANY);
  (* set a variable with the stated offset from current frame *)

  Copier = PROCEDURE (up : CARDINAL) : REF Frame;
  (* copy frames, starting "up" frames up from current frame, all the
     way to the root *)

  Adapter = PROCEDURE (up : CARDINAL) : SchemeEnvironment.T;
  (* convert frames, starting "up" frames up from current frame, all
     the way to the root, to a SchemeEnvironment.T *)

(* all the following routines work similarly.

   If frame is non-NIL, use frame and thread through frame[0] to get the
   next frame up.

   If frame is NIL, use g, s, etc. 
*)

PROCEDURE GenericGet(READONLY args  :     Frame; 
                              frame : REF Frame; 
                              g     :     Getter;
                              up, pos :   CARDINAL) : REFANY;


PROCEDURE GenericSet(VAR      args  :     Frame; 
                              frame : REF Frame; 
                              s     :     Setter;
                              up, pos :   CARDINAL;
                              to    : REFANY);

PROCEDURE GenericCopy(READONLY args  :     Frame;
                               frame : REF Frame;
                               c     :     Copier;
                               up    :     CARDINAL) : REF Frame;

PROCEDURE GenericAdapt(READONLY args  :     Frame;
                                frame : REF Frame;
                                a     :     Adapter;
                                up    :     CARDINAL) : SchemeEnvironment.T;

CONST MaxTailArgs = 10;

TYPE
  LocalProcedureResult = RECORD
    (* 
       a local procedure result is either:

       (1) a tail call to a local procedure
       (2) a result
       (3) a tail call elsewhere?
    *)
    
    tailProc   : LocalProcedure;

    tailArgsN  : [ -1 .. MaxTailArgs ];
    tailArgs   : ARRAY [0 .. MaxTailArgs-1] OF REFANY;

    tailArgsP  : REF ARRAY OF REFANY := NIL;

    result     : REFANY;
  END;
    
    
END CompilerHooks.
