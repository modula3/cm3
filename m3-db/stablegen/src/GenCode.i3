(* Copyright (C) 1991, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Created by Susan Owicki, rewritten by Carsten Weich               *)
(* Last modified on Mon Sep 26 19:26:49 PDT 1994 by weich     *)

(* Main code generating module. "Do()" produces the implementation
   of a stable subtype of its parameter "qid".
*)
INTERFACE GenCode;

IMPORT Type, M3Context, StablegenError;

PROCEDURE Do (c                        : M3Context.T;
              qid                      : Type.Qid;
              reveal, implName, repName: TEXT         )
  RAISES {StablegenError.E};
(* Generate implementation for the object which type is
   named by "qid".  "c" is the current compilation context.
   An exception is raised if an error is detected.  "reveal"
   is the name of the interface containing the most specific
   revealation of "qid".  "implName" is the name of the
   implementation module for the stable subtype.  "repName"
   is the module name of the generic part of the
   implementation of the stable object (usually like
   "implName" with a ``Rep'' appended).  The "repName"
   module is made by instatiating the generic "StableRep"
   module. *)

END GenCode.
