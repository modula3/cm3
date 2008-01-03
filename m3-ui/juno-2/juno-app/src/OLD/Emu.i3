(* Copyright (C) 1992, Digital Equipment Corporation *)
(* Last modified on Fri Aug 7 21:51:54 PDT 1992 by myers *)
<* PRAGMA LL *>


(* This interface describes the mini-executor which the prototype uses to
   do its drawing.  It accepts a severely limited version of Juno-2:


|S ::= [{Id ~ (Num, Num),} Id ~ (Num, Num) :: ] [TRUE ->] {Cm ;} Cm
   | *nothing*
|Cm ::= PS.MoveTo(Id) | PS.LineTo(Id) |
|       PS.CurveTo(Id,Id,Id) | PS.Stroke() | PS.Fill()

   An "Emu.T" is an emulator object.  It contains the state of the
   emulator, such as the current environment and the current stack.

   *)

INTERFACE Emu;

IMPORT JunoAST AS AST, AtomJVTbl, JunoScope AS Scope;

EXCEPTION StackEmpty;

PROCEDURE Run (prog: AST.T; scp: Scope.T): AtomJVTbl.T;
(* Execute an AST.  Unbound procedures referenced in the AST are resolved
   using the environment.  The return value of "Run" maps variable names
   representing points onto their positions. *)

PROCEDURE Arg (i: CARDINAL): REFANY;
(* Return the "n + 1 - i"'th argument of the currently executing procedure
   call, where "n" is the total number of "in" parameters. *)

END Emu.
