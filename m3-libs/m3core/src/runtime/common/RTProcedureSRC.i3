(*| Copyright (C) 1990, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*| Last modified on Thu Jun  3 16:55:08 PDT 1993 by kalsow     *)
(*|      modified on Mon Feb 22 09:59:59 PST 1993 by jdd        *)
(*|      modified on Tue Oct  9 21:54:08 1990 by muller         *)

(* "RTProcedureSRC" is an implementation-dependent extension to
   "RTProcedure". *)

INTERFACE RTProcedureSRC;

FROM RTProcedure IMPORT Proc;

TYPE
  Name = ADDRESS;
  (* a C-style null terminated string *)

PROCEDURE NumProcedures (): CARDINAL;
(* Returns the number of global procedures registered in the runtime. *)

PROCEDURE FromPC (pc: ADDRESS; VAR p: Proc; VAR file, name: Name);
(* Returns in (p, name) the address and name of the procedure that seems to
   contain pc.  (i.e.  the first registered procedure before pc) Note that
   this procedure may require a linear search of the registered
   procedures. *)

END RTProcedureSRC.
