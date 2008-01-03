(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue May 24 13:52:01 PDT 1994 by najork                   *)
(*       Created on Mon Feb 21 15:35:23 PST 1994 by najork                   *)


INTERFACE AnimHandlePrivate;

IMPORT AnimRequestQueue, Thread;

FROM AnimHandle IMPORT T, Public;

REVEAL
  T <: Private;

TYPE 
  Private = Public OBJECT
    mu        : MUTEX;
    activated : BOOLEAN;
    starttime : LONGREAL;
    endtime   : LONGREAL;
    cv        : Thread.Condition;
  METHODS
    attach (q : AnimRequestQueue.T);
  END;

END AnimHandlePrivate.
