(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* "M3PathElemOS" is an operating system independent interface to
basic operations on search paths and search path elements. *)

INTERFACE M3PathElemOS;

IMPORT M3PathElemList;

TYPE T <: REFANY;

PROCEDURE Uid(dir: TEXT): T;
(* Return a handle to denote directory "dir". Two handles that
denote the same directory will return "TRUE" wwhen passed to "Equal". *)

PROCEDURE Equal(t1, t2: T): BOOLEAN;
(* Return "TRUE" iff "t1" and "t2" denote the same directory. *)

PROCEDURE EnvExpand(dir: TEXT): TEXT;
(* Expand any environment variables in "dir", according to the conventions
of the operating system. *)

PROCEDURE RemoveParentDenotations(dir: TEXT): TEXT;
(* Return a pathname with as many "parent directory" denotations
removed as possible. *)

PROCEDURE DecomposePath(path: TEXT; readOnly := FALSE): M3PathElemList.T;
(* "path" is a list of directories separated by an OS-specific
character, e.g. ":". The path is decomposed into its components
and an "M3PathElemList.T" is generated. Each member of the result is
created by a call of "M3PathElem.FromText(EnvExpand(c), c, readOnly)",
where "c" is the component pathname. *)

END M3PathElemOS.
