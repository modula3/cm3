(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

(* This module maintains a per-user, persistent table of key-value
   pairs. *)

INTERFACE UserState;

PROCEDURE Get (key: TEXT): TEXT;
(* Returns the value, if any, associated with "key".  Otherwise,
   it returns "NIL". *)

PROCEDURE Put (key, value: TEXT);
(* Associates the pair ("key", "value") and updates the persistent
   storage. *)

PROCEDURE Init (dir: TEXT);
(* Initialize the persistent table from the contents of "dir". *)

END UserState.



