(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jul 18 13:11:25 PDT 1994 by kalsow     *)

INTERFACE M3Path;

TYPE
  T = RECORD
    dir  : TEXT;
    base : TEXT;
    kind : Kind;
  END;

VAR (* possibly changed at initialization and then CONST *)
  SlashText := "/";

TYPE
  Kind = { Unknown, I3, IC, IS, IO, M3, MC, MS, MO,
           IG, MG, C, H, S, O, M3LIB, LIB, LIBX, PGM, PGMX, TMPL };

  OSKind = { Unix, GrumpyUnix, Win32 };

CONST
  OSKindStrings = ARRAY OSKind OF TEXT
  { "Unix",
    "GrumpyUnix",
    "Win32" };

PROCEDURE SetOS (os: OSKind;  host: BOOLEAN);
(* Set the conventions for the specifed platform *)

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
