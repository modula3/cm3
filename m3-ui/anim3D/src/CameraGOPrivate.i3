(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Sat Jul 15 00:29:18 PDT 1995 by najork                   *)
(*       Created on Mon Feb 14 17:07:31 PST 1994 by najork                   *)


INTERFACE CameraGOPrivate;

IMPORT GO;

FROM CameraGO IMPORT T;

REVEAL T <: Private;

TYPE 
  Private = GO.T BRANDED OBJECT
    flag : BOOLEAN;   (* TRUE if camera has been drawn *)
  METHODS
    init () : T;
  END;

END CameraGOPrivate.
