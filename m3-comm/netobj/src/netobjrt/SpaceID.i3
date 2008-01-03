(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* SpaceID.i3 *)
(* Last modified on Mon Jul 19 13:50:29 PDT 1993 by wobber  *)
(*      modified on Thu Nov 19 21:28:19 1992 by gnelson *)
(*      modified on Wed Jun 10 16:56:10 PDT 1992 by owicki *)

(* The "SpaceID" interface is used to generate values which uniquely 
   identify address space instances across space and time. *)

INTERFACE SpaceID;

IMPORT Fingerprint;

TYPE T = Fingerprint.T;

(* A "SpaceID.T" is a value which is sufficiently distinct to
   identify the address space which generated it among the set of all
   such address spaces.  Each value contains both an address and
   a time component, but the exact format is implementation dependent.
*)

PROCEDURE Mine() : T;

(* "Mine" returns the "T" value for the current address space.  It is
    distinct from all other such values in other address spaces.

   Implementation notes:

   The current implementation generates unique values by concatentating
   the local hardware Ethernet address and the real time clock.

   Any given implementation should be able to support aggregate
   "SpaceID.T" generation at sustained rate of at least one per second
   per Ethernet address.
   
   More detailed specification as to how values are generated
   is required to ensure uniqueness across multiple implementations.
*)

END SpaceID.
