(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jul 18 15:09:05 PDT 1994 by najork                   *)
(*       Created on Fri May 20 14:34:27 PDT 1994 by najork                   *)


INTERFACE AnimRequestQueuePrivate;

IMPORT AnimHandle, Prop;

FROM AnimRequestQueue IMPORT T, Public;

REVEAL
  T <: Private;

TYPE 
  Private = Public OBJECT
    list : List;
    ah   : AnimHandle.T;
  END;

TYPE
  List = REF RECORD
    req  : Prop.Request;
    next : List;
  END;

END AnimRequestQueuePrivate.
