(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* "M3PathElem" provides the abstraction of a search path element,
defined as a file system directory. *)

INTERFACE M3PathElem;

CONST
  Brand = "M3PathElem";

TYPE T <: Public;
  Public = OBJECT
  METHODS
    text(): TEXT;
    unexpanded(): TEXT;
    readOnly(): BOOLEAN;
    setReadOnly(ro: BOOLEAN := TRUE);
  END;

CONST
  CurrentDir = "";

(* An "M3PathElem.T" is a unique representative for a set of {\it
equal} directories, where equality is a file system specific notion.
For example two textually different names may denote the same
directory by the mechanism of symbolic links.  The "text" method
returns the directory name after any environment variables have been
expanded.  "unexpanded" returns the unexpanded form. "readOnly"
implies that the directory has been designated as read-only.  The
read-only status may be changed via the "setReadOnly" call.

"CurrentDir" can be used as an operating system independent denotation
for the current working directory. If it is passed to "FromText" it is
as if "Process.GetWorkingDirectory()" had been passed instead. *)

PROCEDURE Equal(e1, e2: T): BOOLEAN;
(* Returns e1 = e2. *)

PROCEDURE FromText(expanded, unexpanded: TEXT; 
                   readOnly := FALSE): T;
(* Return an "T" object for "expanded", such that "t.text()" will return
"expanded",  "t.unexpanded()" will return "unexpanded" and "t.readOnly()" 
will return "readOnly". *)

END M3PathElem.
