(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

INTERFACE GlueObj;

IMPORT LibFile;

PROCEDURE Gen (dll_name, entry: TEXT;  std_call: BOOLEAN;
               ordinal, n_parms, n_checks: INTEGER;
               READONLY checks: ARRAY OF INTEGER;
               direct_calls: BOOLEAN): LibFile.Obj;
(* Generates and returns the glue .OBJ file that connects to
   the procedure "entry" with "n_parms" bytes of parameters
   in DLL "dll_name".  Parameters at the byte offsets specified
   in "checks[0..n_checks-1]" will be probed to ensure that
   the collector isn't hiding them.  *)

PROCEDURE GenHeader1 (dll_name: TEXT): LibFile.Obj;
(* Generates and returns the first of three special .OBJ files that
   help construct a program's import table for the DLL named "dll_name". *)

PROCEDURE GenHeader2 (dll_name: TEXT): LibFile.Obj;
(* Generates and returns the second of three special .OBJ files that
   help construct a program's import table for the DLL named "dll_name". *)

PROCEDURE GenHeader3 (dll_name: TEXT): LibFile.Obj;
(* Generates and returns the third of three special .OBJ files that
   help construct a program's import table for the DLL named "dll_name". *)
 
END GlueObj.
