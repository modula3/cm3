(* Copyright (C) 1993, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Dec  9 11:36:37 PST 1993 by mcjones *)

(* "OSError.E" is an exception raised by a number of operating system
   interfaces such as "File", "FS", and "Process".
   \index{errors, operating system}
   \index{operating system errors}
*)

INTERFACE OSError;

IMPORT AtomList;

TYPE Code = AtomList.T;

EXCEPTION E(Code);

END OSError.

(* "E(code)" is raised by a number of methods and procedures in the
   operating system interfaces to signal any of an open-ended class
   of failures. *)
