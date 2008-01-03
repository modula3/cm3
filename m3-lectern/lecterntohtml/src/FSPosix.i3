(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(* Last modified on Tue Aug 30 09:42:38 PDT 1994 by mcjones        *)

(* POSIX-specific extensions to FS. *)

INTERFACE FSPosix;

IMPORT File, OSError, Pathname;

VAR (*CONST*) SymbolicLinkFileType: File.Type;
(* Equal to {\tt Atom.FromText(\char'42SymbolicLink\char'42)}. *)

PROCEDURE LinkStatus(p: Pathname.T): File.Status RAISES {OSError.E};
(* Return information about the file, directory, or symbolic link named by "p". *)

(* "LinkStatus(p)" behaves like "FS.Status(p)" except when "p" names a
   symbolic link.  In this case, "LinkStatus" returns information about the link
   while "FS.Status" returns information about the file or directory referenced
   by the link.  *)

END FSPosix.
