(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ValueProc.i3                                          *)
(* Last Modified On Tue Oct 27 12:39:44 PST 1992 by owicki     *)

INTERFACE ValueProc;

IMPORT Type, Value;

PROCEDURE ToText(v: Value.T; type: Type.T): TEXT;
(* Return a textual representation of the value v of type T. 
   The text will be suitable for inclusion in program source in
   a context where T is defined. *)

END ValueProc.
