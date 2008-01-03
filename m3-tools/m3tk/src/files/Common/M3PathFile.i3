(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3PathFile;

IMPORT OSError, Rd;
IMPORT M3PathElem, M3PathElemList;

CONST
  FileName = "m3path"; (* default name for path file *)

PROCEDURE Read(
    dir := M3PathElem.CurrentDir;
    name := FileName;
    doTransitiveClosure := TRUE)
    : M3PathElemList.T
    RAISES {OSError.E, Rd.Failure};

(* Reads the named {\it m3path} file in the named directory. An m3path
file has a system dependent format but always contains a list of
directory names. This routine returns these names as a list of
"M3PathElem.T"s.  The names are NOT transformed to be relative to the
current directory. E.g. if one name on the list was "foo" it means
subdirectory "foo" in directory "dir" not "foo" in the current
directory.

A directory may be indicated as read-only, in which case the
"readOnly" method will return TRUE. This is intended as an
optimisation, to avoid repeated scanning of zillions of directories.

If the given m3path file does not exist (or is inaccessible so that it is
impossible to determine if it exists or not) the result will be NIL.

Read will raise "OSError.E" or "Rd.Failure" if a stream error occurs while 
opening and processing the m3path file.

If "doTransitiveClosure" then the list returned is the transitive closure
of the directories on the m3path.  Duplicates are removed, the list is
constructed breadth first. *)

END M3PathFile.
