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

TYPE
  Kind = { Unknown, I3, IC, IS, IO, M3, MC, MS, MO,
           IG, MG, C, H, S, O, M3LIB, LIB, LIBX, PGM, PGMX, TMPL };

TYPE
  OSKind = { Unix, GrumpyUnix, Win32 };

PROCEDURE SetOS (os: OSKind;  host: BOOLEAN);
(* Set the conventions for the specifed platform *)

PROCEDURE New (a, b, c, d: TEXT := NIL): TEXT;
(* Return "a/b/c/d" using the host naming conventions *)

PROCEDURE Join (dir, base: TEXT;  k: Kind;  host: BOOLEAN): TEXT;
(* Build and return the full path name. *)

PROCEDURE Parse (nm: TEXT;  host: BOOLEAN): T;
(* Parse 'nm' into its pieces using the specified platform's conventions. *)

PROCEDURE DefaultProgram (host: BOOLEAN): TEXT;
(* Return the default program name for the specified platform. *)

PROCEDURE ProgramName (base: TEXT;  host: BOOLEAN): TEXT;
(* Return "base" as a program name on the specified platform. *)

PROCEDURE LibraryName (base: TEXT;  host: BOOLEAN): TEXT;
(* Return "base" as a library name on the specified platform. *)

PROCEDURE Convert (nm: TEXT;  host: BOOLEAN): TEXT;
(* Convert the slashes in 'nm' to match the specified platform. *)

PROCEDURE Escape (nm: TEXT): TEXT;
(* Return 'nm' with and embedded backslashes doubled. *)

PROCEDURE IsEqual (a, b: TEXT): BOOLEAN;
(* Return "TRUE" if "a" and "b" name the same path on
   the host operating system. (i.e. ignore case on Win32) *)

PROCEDURE MakeRelative (VAR path: TEXT;  full, rel: TEXT): BOOLEAN;
(* If "full" is a prefix of "path", replace "full" with "rel" and
   return "TRUE".  Otherwise, return "FALSE". *)

END M3Path.
