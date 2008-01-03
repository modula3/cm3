(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Mon Aug 29 15:45:20 PDT 1994 by bharat *)
(* modified on Fri Jul 2 16:33:31 PDT 1993 by mhb *)
<* PRAGMA LL *>

(* All components (forms/frames/widgets) in "NodeVBT" are instances *)
(* of NodeVBT *)

INTERFACE GenerateObliq;

IMPORT FormsVBT;

VAR
  callbackTemplate : TEXT;
  sessionConstructor := "CreateEachFormOnce(LOCAL);\n";
  globalCode : TEXT := "";
  serverSideCode : TEXT := "";

PROCEDURE GenerateCode(fv : FormsVBT.T; progname: TEXT:= NIL;
  bundled: BOOLEAN := TRUE): TEXT;
(* 
   Generates specified executable if bundled else a set of executables
   for greater readability.
   WARNING : progname should be alphanumeric - no underscores or whitespace 
   If progname is NIL then no code is written to file
   Returns the entire program as a text or the session module if not bundled
*)

PROCEDURE Initialize();
(* This loads in all the relevant templates into its internal
   datastructure *)

PROCEDURE SlashQuotes( string: TEXT) : TEXT;
PROCEDURE SlashSlashes(string: TEXT) : TEXT;
(* useful when embedding strings in an Obliq file *)

END GenerateObliq.













