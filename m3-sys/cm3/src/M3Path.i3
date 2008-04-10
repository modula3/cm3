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

TYPE
  OSKind = { Unix, GrumpyUnix, Win32 };

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

PROCEDURE Escape (nm: TEXT): TEXT;
(* Return 'nm' with and embedded backslashes doubled. *)

PROCEDURE IsEqual (a, b: TEXT): BOOLEAN;
(* Return "TRUE" if "a" and "b" name the same path on
   the host operating system. (i.e. ignore case on Win32) *)

PROCEDURE MakeRelative (VAR path: TEXT;  full, rel: TEXT): BOOLEAN;
(* If "full" is a prefix of "path", replace "full" with "rel" and
   return "TRUE".  Otherwise, return "FALSE". *)

PROCEDURE PathLooselyConvertUserInputToHost_ArrayToText (VAR p: ARRAY OF CHAR) : TEXT;

PROCEDURE PathLooselyConvertUserInputToHost_TextToText (text: TEXT) : TEXT;

(* Is a in PathVariableNames? *)
PROCEDURE IsPathVariableName(a: TEXT): BOOLEAN;

(* Ideally this would hide behind a function but that cannot be done efficiently.
These are Quake and Environment variables that we allow to be specified in
either Win32 or Unix and we loosely interpret the value and convert to the host form.
This way users of NT386 and NT386GNU can pick one form of them. *)

CONST PathVariableNames = ARRAY OF TEXT {
(* not all of these presently have meaning but they are suggested synonyms *)
(*      0123456789012345 *)
(* 8*) "CM3_ROOT",   (* root of source tree *)
(* 8*) "M3CONFIG",
(*10*) "CM3_CONFIG", (* new unimplemented synonym for M3CONFIG *)
(*11*) "CM3_INSTALL",
(*11*) "INSTALLROOT",
(*12*) "INSTALL_ROOT",
(*14*) "CM3_SOURCEROOT",  (* new unimplemented synonym for CM3_ROOT *)
(*15*) "CM3_INSTALLROOT",
(*15*) "CM3_SOURCE_ROOT", (* new unimplemented synonym for CM3_ROOT *)
(*16*) "CM3_INSTALL_ROOT"
(*      0123456789012345 *)
};

END M3Path.
