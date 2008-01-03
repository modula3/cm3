(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Mon Aug 10 14:10:21 PDT 1992 by ramshaw*)

INTERFACE HullInput;

IMPORT AlgTypes, HullAlgClass, Thread;

EXCEPTION Failure;

PROCEDURE GetSites(alg: HullAlgClass.T;
                   VAR nSites: INTEGER): AlgTypes.Sites RAISES
                   {Failure, Thread.Alerted};

END HullInput.

