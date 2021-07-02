(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jul 18 13:11:25 PDT 1994 by kalsow     *)

INTERFACE M3Path;

IMPORT Compiler;

TYPE
  T = RECORD
    dir  : TEXT;
    base : TEXT;
    kind : Kind;
  END;

(*CONST SlashText = ARRAY Compiler.OS OF TEXT{"/", "\\"}[Compiler.ThisOS];*)
CONST SlashText = "/";

TYPE
  Kind = { Unknown, 
           I3,     (* M3 source for an interface. *) 
           IB,     (* llvm bitcode for an interface. *) 
           IC,     (* Front end output for an interface. *) 
           IS,     (* Assembly code for an interface. *) 
           IO,     (* Object module for an interface*) 
           M3,     (* M3 source for a module. *) 
           MB,     (* llvm bitcode for a module. *) 
           MC,     (* Front end output for a module. *) 
           MS,     (* Assembly code for a module. *) 
           MO,     (* Object module for a module. *)
           IG,     (* M3 source for a generic interface. *) 
           MG,     (* M3 source for a generic module. *) 
           C,      (* C source file. *) 
           H,      (* C header file. *) 
           B,      (* llvm bitcode, not from M3 code. *) 
           S,      (* Assembly code for non-M3 source. *) 
           O,      (* Object module for non-M3 source. *) 
           M3LIB,  (* Library for M3 code. *) 
           LIB,    (* Library for non-M3 code. *) 
           LIBX,   (* m3linker data for a M3 library. *) 
           PGM,    (* Executable. *) 
           PGMX,   (* m3linker data for a M3 program. *) 
           TMPL    (* Quake template code. *) 
           };

  OSKind = { Unix, GrumpyUnix, Win32 };

CONST
  OSKindStrings = ARRAY OSKind OF TEXT
  { "Unix",
    "GrumpyUnix",
    "Win32" };

PROCEDURE SetTargetOS (os: OSKind);
(* Set the conventions for the target platform *)

PROCEDURE New (a, b, c, d: TEXT := NIL): TEXT;
(* Return "a/b/c/d" using the host naming conventions *)

PROCEDURE Join (dir, base: TEXT;  k: Kind): TEXT;
(* Build and return the full path name. *)

PROCEDURE Parse (nm: TEXT): T;
(* Parse 'nm' into its pieces using the target platform's conventions. *)

PROCEDURE DefaultProgram (): TEXT;
(* Return the default program name for the target platform. *)

PROCEDURE ProgramName (base: TEXT): TEXT;
(* Return "base" as a program name on the target platform. *)

PROCEDURE LibraryName (base: TEXT): TEXT;
(* Return "base" as a library name on the target platform. *)

PROCEDURE Convert (nm: TEXT): TEXT;
(* Return 'nm' with and backslashes changed to forward slashes. *)

PROCEDURE IsEqual (a, b: TEXT): BOOLEAN;
(* Return "TRUE" if "a" and "b" name the same path on
   the host operating system. (i.e. ignore case on Win32) *)

PROCEDURE MakeRelative (VAR path: TEXT;  full, rel: TEXT): BOOLEAN;
(* If "full" is a prefix of "path", replace "full" with "rel" and
   return "TRUE".  Otherwise, return "FALSE". *)

PROCEDURE FixPath (VAR p: ARRAY OF CHAR): TEXT;
(* exposed only for testing *)

END M3Path.
