(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Mon Nov  8 17:21:09 PST 1993 by mcjones    *)
(*      modified on Tue Jul  6 13:05:58 PDT 1993 by wobber     *)
(*      modified on Sat Feb 29 08:19:34 PST 1992 by kalsow     *)
(*      modified on Mon Dec 24 01:09:54 1990 by muller         *)

(* A "Wr.T" (or ``writer'') is a character output stream.  The basic
   operation on a writer is "PutChar", which extends a writer's
   character sequence by one character. Some writers (called
   ``seekable writers'') also allow overwriting in the middle of the
   sequence.  For example, writers to random access files are
   seekable, but writers to terminals and sequential files are not.
   \index{character output stream}
   \index{output stream}
   \index{stream!output}
   \index{writer}

   Writers can be (and usually are) buffered. This means that
   operations on the writer don't immediately affect the underlying
   target of the writer, but are saved up and performed later.  For
   example, a writer to a disk file is not likely to update the disk
   after each character.

   Abstractly, a writer "wr" consists of:

| len(wr)       `a non-negative integer`
| c(wr)         `a character sequence of length "len(wr)"`
| cur(wr)       `an integer in the range "[0..len(wr)]"`
| target(wr)    `a character sequence`
| closed(wr)    `a boolean`
| seekable(wr)  `a boolean`
| buffered(wr)  `a boolean`

   These values are generally not directly represented in the data
   fields of a writer object, but in principle they determine the
   state of the writer.

   The sequence "c(wr)" is zero-based: "c(wr)[i]" is valid for "i"
   from 0 through "len(wr)-1".  The value of "cur(wr)" is the index of
   the character in "c(wr)" that will be replaced or appended by the
   next call to "PutChar".  If "wr" is not seekable, then "cur(wr)" is
   always equal to "len(wr)", since in this case all writing happens
   at the end.

   The difference between "c(wr)" and "target(wr)" reflects the
   buffering: if "wr" is not buffered, then "target(wr)" is updated to
   equal "c(wr)" after every operation; if "wr" is buffered, then
   updates to "target(wr)" can be delayed.  For example, in a writer
   to a file, "target(wr)" is the actual sequence of characters on the
   disk; in a writer to a terminal, "target(wr)" is the sequence of
   characters that have actually been transmitted.  (This sequence may
   not exist in any data structure, but it still exists abstractly.)

   If "wr" is buffered, then the assignment "target(wr) := c(wr)" can
   happen asynchronously at any time, although the procedures in this
   interface are atomic with respect to such assignments.

   Every writer is a monitor; that is, it contains an internal lock
   that is acquired and held for each operation in this interface, so
   that concurrent operations will appear atomic.  For faster,
   unmonitored access, see the "UnsafeWr" interface.

   If you are implementing a long-lived writer class, such as a pipe
   or TCP stream, the index of the writer may eventually overflow,
   causing the program to crash with a bounds fault.  We recommend
   that you provide an operation to reset the writer index, which the
   client can call periodically.

   It is useful to specify the effect of several of the procedures in
   this interface in terms of the action "PutC(wr, ch)", which outputs
   the character "ch" to the writer "wr":

| PutC(wr, ch) =
|   IF closed(wr) THEN `Cause checked runtime error` END;
|   IF cur(wr) = len(wr) THEN
|     `Extend "c(wr)" by one character, incrementing "len(wr)"`
|   END;
|   c(wr)[cur(wr)] := ch;
|   INC(cur(wr));

   "PutC" is used only in specifying the interface; it is not a real
   procedure. *)

INTERFACE Wr;

IMPORT AtomList;
FROM Thread IMPORT Alerted;

TYPE T <: ROOT;

EXCEPTION Failure(AtomList.T);

(* Since there are many classes of writers, there are many ways that a
   writer can break---for example, the network can go down, the disk
   can fill up, etc.  All problems of this sort are reported by
   raising the exception "Failure".  The documentation of each writer
   class should specify what failures the class can raise and how they
   are encoded in the argument to "Failure".

   Illegal operations (for example, writing to a closed writer) cause
   checked runtime errors. *)

VAR (*CONST*) EOL: TEXT;
(* End of line. *)

(* On POSIX, "EOL" is {\tt \char'42\char'134n\char'42}; on Win32,
   "EOL" is {\tt \char'42\char'134r\char'134n\char'42}. *)

PROCEDURE PutChar(wr: T; ch: CHAR) RAISES {Failure, Alerted};
(* Output "ch" to "wr".  More precisely, this is equivalent to: *)
(*
| PutC(wr, ch); IF NOT buffered(wr) THEN Flush(wr) END
*)
(* Many operations on a writer can wait indefinitely.  For example,
   "PutChar" can wait if the user has suspended output to his
   terminal.  These waits can be alertable, so each procedure that
   might wait includes "Thread.Alerted" in its raises clause. *)

PROCEDURE PutText(wr: T; t: TEXT) RAISES {Failure, Alerted};
(* Output "t" to "wr".  More precisely, this is equivalent to: *)
(*
| FOR i := 0 TO Text.Length(t) - 1 DO
|   PutC(wr, Text.GetChar(t, i))
| END;
| IF NOT buffered(wr) THEN Flush(wr) END

   except that, like all operations in this interface, it is atomic
   with respect to other operations in the interface. (It would be
   wrong to write "PutChar" instead of "PutC", since "PutChar" always
   flushes if the writer is unbuffered.)
*)

PROCEDURE PutString(wr: T; READONLY a: ARRAY OF CHAR)
  RAISES {Failure, Alerted};
(* Output "a" to "wr".  More precisely, other than the fact that this
   is atomic, it is equivalent to: *)
(*
| FOR i := FIRST(a) TO LAST(a) DO PutC(wr, a[i]) END;
| IF NOT buffered(wr) THEN Flush(wr) END
*)

PROCEDURE Seek(wr: T; n: CARDINAL) RAISES {Failure, Alerted};
(* Set the current position of "wr" to "n".  This is an error if "wr"
   is closed. More precisely, this is equivalent to: *)
(*
| IF wr.closed OR NOT seekable(wr) THEN
|   `Cause checked runtime error`
| END;
| cur(wr) := MIN(n, len(wr))
*)

PROCEDURE Flush(wr: T) RAISES {Failure, Alerted};
(* Perform all buffered operations.  That is, set "target(wr) :=
   c(wr)".  It is a checked runtime error if "wr" is closed. *)

PROCEDURE Close(wr: T) RAISES {Failure, Alerted};

(* Flush "wr", release any resources associated with "wr", and set
   "closed(wr) := TRUE".  The documentation for a procedure that
   creates a writer should specify what resources are released when
   the writer is closed.  This leaves "closed(wr)" equal to "TRUE"
   even if it raises an exception, and is a no-op if "wr" is closed.
   *)

PROCEDURE Length(wr: T): CARDINAL RAISES {Failure, Alerted};
PROCEDURE Index(wr: T): CARDINAL RAISES {};
PROCEDURE Seekable(wr: T): BOOLEAN RAISES {};
PROCEDURE Closed(wr: T): BOOLEAN RAISES {};
PROCEDURE Buffered(wr: T): BOOLEAN RAISES {};
(* These procedures return "len(wr)", "cur(wr)", "seekable(wr)",
   "closed(wr)", and "buffered(wr)", respectively. "Length" and
   "Index" cause a checked runtime error if "wr" is closed; the other
   three procedures do not. *)

END Wr.
