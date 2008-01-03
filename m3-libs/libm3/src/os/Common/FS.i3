(* Copyright (C) 1993, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(* Last modified on Thu Jul 14 11:37:56 PDT 1994 by mcjones *)

(* The "FS" interface provides persistent storage (files) and naming
   (directories).
   \index{directory}
*)

INTERFACE FS;

IMPORT OSError, File, Pathname, Time;

PROCEDURE GetAbsolutePathname(p: Pathname.T): Pathname.T
   RAISES {OSError.E};
(* Return an absolute pathname referring to the same file or
   directory as "p". *)

(* The new pathname will not involve any symbolic links or relative
   arcs (that is, occurrences of "Pathname.Parent" or
   "Pathname.Current".
   \index{absolute pathname!from relative pathname}
   *)

(* The procedures "OpenFile" and "OpenFileReadonly" look up a pathname
   and return a file handle, which is an object allowing a file to be
   read and perhaps written.  The returned value will be of some
   subtype of "File.T", depending on the kind of object named by "p".
   If the object is a regular file, the type will be "RegularFile.T".
   If the object is a terminal, the type will be "Terminal.T".  Other,
   system-specific subtypes are also possible. Under appropriate
   conditions, "OpenFile" can create a new regular file.  "OSError.E"
   is raised if the pathname passed to "OpenFile" or
   "OpenFileReadonly" is that of a directory. *)

TYPE
  CreateOption = {Never, Ok, Always};
  AccessOption = {OnlyOwnerCanRead, ReadOnly, Default};

PROCEDURE OpenFile(
    p: Pathname.T;
    truncate: BOOLEAN := TRUE;
    create: CreateOption := CreateOption.Ok;
    template: File.T := NIL;
    access: AccessOption := AccessOption.Default): File.T
  RAISES {OSError.E};
(* Return an object permitting writing and reading an existing or
   newly-created file named "p".  *)

(* Suppose "p" names an existing regular file.  If "create = Always",
   then "OSError.E" is raised.  Otherwise, the existing file is
   opened, after truncating it to zero size if "truncate = TRUE".

   On the other hand, suppose the file named by "p" does not exist.
   If "create = Never", then "OSError.E" is raised.  Otherwise, a new
   file is created.  Normally the new file is a regular file, but some
   implementations may determine the type of the new file from the
   identity of the directory in which it is being created.  The access
   control settings of the new file are set using the values of
   "template" and "access".  If "template # NIL", then "access" is
   ignored and the new file is given the same per-file access control
   settings as "template".  If "template = NIL", the file's access
   control settings are determined by an implementation-defined
   default value, with possible restrictions determined by the value
   of "access":

   \begin{description}
   \item["OnlyOwnerCanRead"] read access is allowed only by this user
   \item["ReadOnly"] write access is allowed to no one (except via the
                     "File.T" returned by this call of "OpenFile")
   \item["Default"] the default applies with no restrictions.
   \end{description}
   \index{creating a file}
   \index{file!creation}

   A newly-created file "f" has

| buffer(f) = stable(f) = `empty sequence`
| mtime(f) = `current time`
| locked(f) = Process.NullID

   "OpenFile" doesn't change "mtime(f)" of an existing file "f".

   If "OpenFile" returns a regular file handle, say "h", then its
   initial state will be:

| type(h) = RegularFile.FileType
| readable(h) = writable(h) = TRUE
| cur(h) = 0
| file(h) = `file with pathname "p"`

   To append to an existing file, perform the call

| EVAL h.seek(Origin.End, 0)

   after opening "h". *)

PROCEDURE OpenFileReadonly(p: Pathname.T): File.T
  RAISES {OSError.E};
(* Return an object permitting reading the file named by "p". *)

(* If "p" names a regular file, the call "OpenFileReadonly(p)" returns
   a file handle "h" with

| type(h) = Atom.FromText("RegularFile")
| readable(h) = TRUE
| writable(h) = FALSE
| cur(h) = 0
| file(h) = `file with pathname "p"`

*)

PROCEDURE CreateDirectory(p: Pathname.T) RAISES {OSError.E};
(* Create a directory named by "p". *)

PROCEDURE DeleteDirectory(p: Pathname.T) RAISES {OSError.E};
(* Delete the directory named by "p".  "OSError.E" is raised if the
   directory contains entries (other than perhaps "Pathname.Current"
   and "Pathname.Parent"). *)

PROCEDURE DeleteFile(p: Pathname.T)
  RAISES {OSError.E};
(* Delete the file or device named by "p".  "OSError.E" is raised if "p"
   names a directory. *)

(* Note: Under Win32, "DeleteFile" raises "OSError.E" if "p" is open.
   Under POSIX, an open file may be deleted; the file doesn't actually
   disappear until every link (pathname) for it is deleted. *)

PROCEDURE Rename(p0, p1: Pathname.T)
  RAISES {OSError.E};
(* Rename the file or directory named "p0" as "p1". *)

(* Some implementations automatically delete an existing file named
   "p1", others raise "OSError.E".  Some implementations disallow a
   rename where "p0" and "p1" name different physical storage devices
   (different root directories or file systems).  *)

TYPE
  Iterator <: PublicIterator;
  PublicIterator = OBJECT METHODS
    next(VAR (*OUT*) name: TEXT): BOOLEAN;
    nextWithStatus(VAR (*OUT*) name: TEXT;
      VAR (*OUT*) stat: File.Status): BOOLEAN RAISES {OSError.E};
    close();
  END;

VAR (*CONST*) DirectoryFileType: File.Type;
(* Equal to {\tt Atom.FromText(\char'42Directory\char'42).} *)

PROCEDURE Iterate(p: Pathname.T): Iterator
  RAISES {OSError.E};
(* Return an iterator for the entries of the directory named by "p". *)

(* An "Iterator" supplies information about the entries in a
   directory: names and, optionally, status.  The iteration does not
   include entries corresponding to "Pathname.Current" or
   "Pathname.Parent".

   The methods have the following specifications: 

   If more entries remain, the call "i.next(n)" sets "n" to the name
   of the next one and returns "TRUE".  It returns "FALSE" without
   setting "n" if no more entries remain.

   If more entries remain, the call "i.nextWithStatus(n, s)" sets "n"
   to the name of the next one, sets "s" to the status of that entry,
   and returns "TRUE".  The value of "s.type" is "DirectoryFileType"
   if the entry is a directory.  The call returns "FALSE" without
   setting "n" or "s" if no more entries remain.

   The call "i.close()" releases the resources used by "i", after
   which time it is a checked runtime error to use "i".  Every
   iterator should be closed.

   You iterate over the entries in a directory with code like this:

| VAR
|   i := FS.Iterate(pathname);
|   name: TEXT;
| BEGIN
|   TRY
|     WHILE i.next(name) DO
|       `Process "name"`
|     END
|   FINALLY
|     i.close()
|   END
| END

   Use "nextWithStatus" instead of "next" if you would otherwise call
   "Status" (or the "File.T" "status" method) on most of the entries
   (in some implementations, "nextWithStatus" requires an extra disk
   access).

   What can be assumed if a directory is being updated concurrently
   with an iteration?  An entry that is not inserted or deleted will
   occur in the iteration at least once, and an entry that occurs in
   the iteration must have been in the directory at some moment.  *)

PROCEDURE Status(p: Pathname.T): File.Status
  RAISES {OSError.E};
(* Return information about the file or directory named by "p". *)

(* Possible values of "stat.type" include

| FS.DirectoryFileType `(a directory)`
| RegularFile.FileType `(a disk file)`
| Terminal.FileType `(a terminal)`

   If "p" is a disk file, "stat.modificationTime" and "stat.size" will
   be set.

   See also the "status" method of "File.T" and the "nextWithStatus"
   method of "Iterator".  *)

PROCEDURE SetModificationTime(
    p: Pathname.T;
    READONLY t: Time.T)
  RAISES {OSError.E};
(* Change the modification time of the file or directory named by "p"
   to "t".  *)

END FS.
