(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Dec  8 16:17:00 PST 1993 by kalsow     *)
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *)

INTERFACE M3CG_Check;

IMPORT M3CG;

PROCEDURE New (child: M3CG.T;
               clean_jumps, clean_stores: BOOLEAN;
               nested_calls, nested_procs: BOOLEAN): M3CG.T;
(* returns a fresh, initialized code generator that passes its calls
   to 'child' and checks that the stream obeys the M3CG restrictions.
   If 'clean_jumps', it also checks that the stack is empty at every
   branch point.  If 'clean_stores', it also checks that the stack is
   empty on every store instruction.  If 'nested_calls' is false, it
   checks that function calls are not nested.  If 'nested_procs' is false,
   it checks that function bodies do not overlap. *)

END M3CG_Check.
