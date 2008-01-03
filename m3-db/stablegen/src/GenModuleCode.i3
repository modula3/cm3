(* Copyright (C) 1991, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Created by Susan Owicki, rewritten by Carsten Weich        *)
(* Last modified on Fri Sep 23 14:53:42 PDT 1994 by weich     *)

(* Produce Modula-3 code for the stable object's
   implementation. The exception "StablegenError.E" is raised if
   some error in the users data structures is uncoverd. In general, this
   should not occur at this point.
*)
INTERFACE GenModuleCode;

IMPORT Formatter, ImportList, StablegenError, Type;

PROCEDURE Header (modWr     : Formatter.T;
                  implName  : TEXT;
                  methods   : ImportList.MethodList;
                  importList: ImportList.T           );
(* Produce a module (with name "implName") header together
   with the "IMPORT" statements (produced out of
   "importList".  After that a declaration of an enumeration
   type is added.  It enumerates all update methods (using
   the list "methods").  Code is written to "modWr". *)

PROCEDURE Revealation (modWr  : Formatter.T;
                       repName: TEXT;
                       methods: ImportList.MethodList) 
  RAISES {StablegenError.E};
(* Produce the code for the revealation of the generic type
   "Stable.T".  Code is written to "modWr", "repName" is the filename of
   the instatiation of the generic part of the implementation,
   "methods" is a list of its update methods *)

PROCEDURE Surrogates (modWr  : Formatter.T;
                      name   : Type.Qid;
                      repName: TEXT;
                      methods: ImportList.MethodList)
  RAISES {StablegenError.E};
(* Produce the code of the methods that log their parameters.  Code is
   written to "modWr", "name" is the object's name for which a stable
   implementation is produced, "repName" is the name of the
   instantiated generic part of the generated code, "methods" is a
   list of its update methods *)

PROCEDURE Dispatcher (modWr  : Formatter.T;
                      methods: ImportList.MethodList)
  RAISES {StablegenError.E};
(* Produce code for procedure which reads the log and starts all the
   redo stubs.  Code is written to "modWr", "methods" is a list of its
   update methods *)

PROCEDURE ReplayStubs (modWr  : Formatter.T;
                       name   : Type.Qid;
                       methods: ImportList.MethodList)
  RAISES {StablegenError.E};
(* Produce code for the redo stubs.  Again "modWr" is the writer that
   takes the code, "type" is the object and "methods" is the list of
   the update methods *)

PROCEDURE Checkpoint(modWr: Formatter.T; repName: TEXT);
(* Generate the call to the procedure that does the checkpoint (which
   is in the generic part of the implementation of stable objects "repName") *)

END GenModuleCode.
