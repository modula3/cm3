(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jan  3 16:03:07 PST 1995 by najork                   *)
(*       Created on Wed Feb 16 16:13:24 PST 1994 by najork                   *)


INTERFACE GroupGOPrivate;

IMPORT GO;

FROM GroupGO IMPORT T, Public;

REVEAL 
  T <: Private; 

TYPE
  Private = Public BRANDED OBJECT
    children : REF ARRAY OF GO.T;
    last     : INTEGER;   (* children[0 .. last] contains good data *)
  END;

END GroupGOPrivate.
