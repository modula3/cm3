(* Copyright (C) 1989-1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Fri Jul  9 21:30:41 PDT 1993 by mhb    *)
(*      modified on Fri Jun 11 23:03:17 PDT 1993 by meehan *)
(* modified on Fri Mar 27 02:14:10 1992 by steveg *)

(* {\em Resources\/} are arbitrary texts that are associated with
   applications.  Resources can be bundled into an application
   using the "m3bundle" facility.  They may also be found in the
   file system.

   This interface supports retrieval of resources using a {\em search
   path}.  A search path is a list of elements; each element is either
   a "Pathname.T" that refers to a directory, or a "Bundle.T",
   typically created by "m3bundle". *)

INTERFACE Rsrc;

IMPORT RefList, Rd, Thread;

TYPE Path = RefList.T; (* of Pathname.T or Bundle.T *)

EXCEPTION NotFound;

PROCEDURE Open (name: TEXT; path: Path): Rd.T
  RAISES {NotFound};
(* If "name" is an absolute pathname, then look for "name" in the
   file system: A reader is returned if
| FileRd.Open(name)
   is successeful; otherwise an exception is raised.  If "name"
   is not an absolute pathname, then search each element of
   "path", from front to back, for the first occurrence of the
   resource called "name" and return a reader on the resource.
   If the path element is a pathname "p", then a reader is
   returned if
| FileRd.Open(Pathname.Join (p, name, NIL))
   is successful.  If the path element is a bundle "b", a reader
   is returned if
| TextRd.New(Bundle.Get(b, name))
   is successful.  The "NotFound" exception is raised if no
   element of "path" yields a successful reader on "name".  It is
   a checked runtime error if "path" contains an element that is
   neither a pathname nor a bundle. *)

PROCEDURE Get (name: TEXT; path: Path): TEXT
  RAISES {NotFound, Rd.Failure, Thread.Alerted};
(* A convenience procedure to retrieve the contents of the
   resource "name" as a "TEXT". *)

(* The procedure "Get" is logically equivalent to
|  VAR rd := Open(name, path);
|  BEGIN
|    TRY
|      RETURN Rd.GetText(rd, LAST(CARDINAL))
|    FINALLY
|      Rd.Close(rd)
|    END
|  END;
   The implementation is slightly more efficient, because it
   takes advantage of "Bundle.Get" procedure which returns the
   contents of the bundle element as a "TEXT".  The "Rd.Failure"
   exception is raised if "Rd.GetText" or "Rd.Close" report a
   problem.  The "Thread.Alerted" can be raised by the call to
   "Rd.GetText". *)

PROCEDURE BuildPath (a1, a2, a3, a4: REFANY := NIL): Path;
(* Build a "Path" from the non-"NIL" elements.  Each element must be
   either a "Bundle.T" or a "TEXT".  If it is a "TEXT", it is assumed to
   be the pathname of a directory, unless it starts with a dollar
   sign, in which case it is assumed to be an environment variable whose
   value is the name of a directory; the value is retrieved using
   "Env.Get".  It is a checked runtime error if the pathname is not
   valid. *)

END Rsrc.




