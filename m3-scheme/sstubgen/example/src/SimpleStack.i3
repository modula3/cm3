(* $Id: SimpleStack.i3,v 1.1 2009/06/28 11:38:26 mika Exp $ *)

INTERFACE SimpleStack;
IMPORT SchemeEnvironment;

TYPE T <: REFANY;

PROCEDURE NewRoot(parent : SchemeEnvironment.T) : T;
  
PROCEDURE NewFrame(parent : T) : T;

(**********************************************************************)

PROCEDURE PushArg(frame : T; arg : REFANY);

PROCEDURE PopArg(frame : T) : REFANY;

PROCEDURE Args(frame : T) : CARDINAL;

(**********************************************************************)

TYPE Procedure = REFANY;

PROCEDURE PushProcedure(frame : T; proc : Procedure);
  
PROCEDURE PopProcedure(frame : T) : Procedure;

(**********************************************************************)
  
PROCEDURE MakeLocals(frame : T; n : CARDINAL);

PROCEDURE GetLocal(frame : T; idx : CARDINAL) : REFANY;

PROCEDURE SetLocal(frame : T; idx : CARDINAL; to : REFANY);

(**********************************************************************)

PROCEDURE ReplaceFrame(frame : T);
  (* replace parent frame with this frame *)

CONST Brand = "SimpleStack";

END SimpleStack.

  
  
