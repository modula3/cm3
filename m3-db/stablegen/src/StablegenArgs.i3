(* Copyright (C) 1991, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Created by Susan Owicki, rewritten by Carsten Weich        *)
(* Last modified on Mon Sep 26 19:17:29 PDT 1994 by weich     *)

(* The module "Args" parses the command line using "M3Args" of
   the toolkit.
*)

INTERFACE StablegenArgs;

IMPORT Type, StablegenError;

PROCEDURE Get(VAR object: Type.Qid; VAR reveal, impl, rep: TEXT)
  RAISES {StablegenError.E};
(* Parse the command line and return the type to be made stable in
   "object" and the name of the module containing the implementation
   of the stable subtype in "impl". "reveal" is the name of the
   interface containing the most specific revealation of
   "object". "rep" is the name of the instatiated "StableRep" generic
   module. The exception is raised if the parameters are unusable
   (i.e.\ invalid or ``-help'') *)
  
END StablegenArgs.
