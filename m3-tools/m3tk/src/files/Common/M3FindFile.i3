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

(* "M3FindFile" defines an abstract object type that provides some
basic file handling operations. *)

INTERFACE M3FindFile;

IMPORT Rd, OSError, M3Extension;

EXCEPTION
  Failed;

TYPE
  T = OBJECT
  METHODS
    exts(): M3Extension.TSet;
    find(name: TEXT; ext: M3Extension.T): TEXT 
        RAISES {Failed};
    openRead(name: TEXT; ext: M3Extension.T): Rd.T 
        RAISES {OSError.E, Failed};
  END;

END M3FindFile.

(* Objects of this type are used to find a file name given a module or
interface name and an extension.  The "exts" method returns the set of
extensions for which the file finder provides this lookup service.
Trying to look up a file with an extension not in the set returned by
"exts" will typically cause a checked runtime error.  The "find"
method does the work; it constructs the name of the file corresponding
to "name" and "ext" and returns it. If there is no such file it raises
"Failed". For example a simple file "find" method might just use
"M3Extension.Extend" to construct a file name and then check if the
name corresponds to an existing file. More complex "find" methods
might look for the corresponding file in some list of locations.  Note
that on some systems a file may exist but it is impossible to tell
that it exists because it is totally inaccessible. If the file
corresponding to "name" and "ext" is totally inaccessible in this way
the "find" method will raise "Failed". The "openRead" method tries to
open a reader on the pathname returned by "find(name, ext)". If there
is no such file, "Failed" is raised. If an error occurs trying to open
the file, "OSError.E" is raised. *)

