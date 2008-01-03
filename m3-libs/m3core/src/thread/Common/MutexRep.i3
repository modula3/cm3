(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)


(* In this implementation of Modula-3, the builtin type "MUTEX" is
   an object with "acquire" and "release" methods.  Overriding
   these methods may cause dire results. *)

UNSAFE INTERFACE MutexRep;

TYPE
  Public = OBJECT METHODS
    acquire ();
    release ();
  END;

REVEAL
  MUTEX <: Public;

END MutexRep.
