(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
 
(* Last modified on Wed Jan 16 12:22:33 PST 1991 by mjordan    *)

INTERFACE MProperty;

IMPORT Property, Thread;

TYPE
  Set <: Set_public;
  Set_public = Property.Set OBJECT
    m: Thread.Mutex;
  END;

(* The methods for an MProperty.Set are equivalent to 
     'LOCK s.m DO Property.Set.method(s, ...) END;
*)

PROCEDURE New(): Set;
(* Create the empty monitored set. *)

END MProperty.
