(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemePrimitive;
IMPORT SchemeEnvironment, SchemeProcedure;

(* Routines for installing Scheme primitives in a Scheme environment *)

TYPE 
  T <: Public;

  Public = SchemeProcedure.T OBJECT METHODS
    init(id : CARDINAL; 
         definer : Definer;  (* used for extension primitives *)
         minArgs, maxArgs : CARDINAL) : T;

    getMinArgs() : CARDINAL;
    getMaxArgs() : CARDINAL;
    getId() : CARDINAL;
  END;

  Definer <: PubDefiner;

  PubDefiner = OBJECT METHODS
    installPrimitives(env : SchemeEnvironment.T) : SchemeEnvironment.T;
  END;

  DefaultExtendedDefiner <: Definer; (* some "normal" extensions *)
  DefaultDefiner <: Definer;  (* all the normal stuff *)
  SandboxDefiner <: Definer;  (* no ability to open files *)

  ExtDefiner <: PubExtensibleDefiner;

  PubExtensibleDefiner = DefaultDefiner OBJECT METHODS
    init() : ExtDefiner;

    addPrim(name : TEXT; 
            proc : SchemeProcedure.T; 
            minArgs, maxArgs : CARDINAL);
    (* add a user-defined primitive *)
  END;

CONST Brand = "SchemePrimitive";

(**********************************************************************)
(* The following routines of interest to those defining their own Definers *)

PROCEDURE InstallSandboxPrimitives(dd : Definer;
                            env : SchemeEnvironment.T) : SchemeEnvironment.T;
(* a basic set of Scheme primitives that don't let you escape the system *)

PROCEDURE InstallFileIOPrimitives(dd : Definer;
                            env : SchemeEnvironment.T) : SchemeEnvironment.T;
(* local file I/O *)

PROCEDURE InstallNorvigPrimitives(dd : Definer;
                            env : SchemeEnvironment.T) : SchemeEnvironment.T;
(* Norvig's extensions (not fully implemented) *)

PROCEDURE InstallDefaultExtendedPrimitives(dd : Definer;
                            env : SchemeEnvironment.T) : SchemeEnvironment.T;
(* a few locally defined primitives: random, normal, ... *)

PROCEDURE InstallDefaultPrimitives(dd : Definer;
                            env : SchemeEnvironment.T) : SchemeEnvironment.T;
(* all of the above *)

PROCEDURE EDInstallPrimitives(ed : ExtDefiner; 
                              env : SchemeEnvironment.T) : SchemeEnvironment.T;
(* machinery for an extensible Definer *)

END SchemePrimitive.
