(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* "M3Directory" provides a directory iterator for Modula-3 files *)

INTERFACE M3Directory;

IMPORT M3Extension, Pathname, OSError;

TYPE
  Iter <: Public;
  Public = OBJECT
  METHODS
    next(VAR (*out*) name: TEXT; VAR (*out*) ext: M3Extension.T): BOOLEAN;
    close();
  END;

PROCEDURE NewIter(
    d: Pathname.T;
    exts: M3Extension.TSet
      := M3Extension.TSet{FIRST(M3Extension.T)..LAST(M3Extension.T)}
    ): Iter RAISES {OSError.E};
(* Create a new iterator for the directory and extension set
specified.  If d = "", the handle is for the current directory.  The
"next" method returns the name of the next file in the directory with
an extension in "exts" is returned.  "name" is the unextended name of
the file, "ext" is the extension.  "TRUE" is returned if a file is
found, "FALSE" otherwise. *)


END M3Directory.
