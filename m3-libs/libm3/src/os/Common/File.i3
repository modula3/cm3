(* Copyright (C) 1993, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Fri Jul 15 13:45:59 PDT 1994 by mcjones *)

(* A "File.T", or {\em file handle}, is a source and/or sink of bytes.
   File handles provide an operating-system independent way to perform
   raw I/O.  For buffered I/O, use the "FileRd" and "FileWr"
   interfaces instead.  A file handle is created using "OpenFile" or
   "OpenFileReadonly" in the "FS" interface.
   \index{file!handle}
   \index{unbuffered file I/O}
   \index{file!unbuffered I/O}
   \index{I/O!unbuffered}
*)

INTERFACE File;

IMPORT Atom, OSError, Time;

TYPE
  T <: Public;
  Public = OBJECT METHODS
    read(VAR (*OUT*) b: ARRAY OF Byte;
      mayBlock: BOOLEAN := TRUE): INTEGER RAISES {OSError.E};
    write(READONLY b: ARRAY OF Byte) RAISES {OSError.E};
    status(): Status RAISES {OSError.E};
    close() RAISES {OSError.E}
  END;
  Byte = BITS 8 FOR [0 .. 255];
  Status = RECORD
    type: Type;
    modificationTime: Time.T;
    size: CARDINAL
  END;
  Type = Atom.T;

END File.

(* Formally, a file handle "h" has the components:

| type(h)      `an atom, the type of file`
| readable(h)  `a boolean`
| writable(h)  `a boolean`
| src(h)       `(a "REF" to) a sequence of bytes`
| srcCur(h)    `an integer in the range "[0..len(src(h))]"`
| srcEof(h)    `a boolean`
| snk(h)       `(a "REF" to) a sequence of bytes`
| snkCur(h)    `an integer in the range "[0..len(snk(h))]"`

   The "src..." components are meaningful only if "readable(h)".  The
   sequence "src(h)" is zero-based: "src(h)[i]" is valid for "i" from
   "0" to "len(src(h))-1".  For some subtypes of "File.T", the
   sequence "src(h)" can grow without bound.

   The "snk..." components are meaningful only if "writable(h)".  The
   sequence "snk(h)" is zero based: "snk(h)[i]" is valid for "i" from
   "0" to "len(snk(h))-1".

   For full details on the semantics of a file handle, consult the
   interface defining the particular subtype, for example, "Pipe.T",
   "Terminal.T", or "RegularFile.T".  In the case where no exceptions
   are raised, the methods of the subtypes of "File.T" obey the
   following specifications:

   The call

| h.read(b, mayBlock)

   is equivalent to

| IF NOT readable(h) OR NUMBER(b) = 0 THEN
|   `Cause checked runtime error`
| END;
| IF srcCur(h) = len(src(h)) AND NOT srcEof(h) THEN
|   IF NOT mayBlock THEN RETURN -1 END;
|   `Block until "srcCur(h) < len(src(h)) OR srcEof(h)"`
| END;
| IF srcCur(h) = len(src(h)) THEN RETURN 0 END;
| `Choose "k" such that:`
|   1 <= k <= MIN(NUMBER(b), len(src(h))-srcCur(h));
| FOR i := 0 TO k-1 DO
|   b[i] := src(h)[srcCur(h)];
|   INC(srcCur(h))
| END;
| RETURN k

   \index{non-blocking read}

   A result of zero always means end of file.  The meaning of a
   subsequent read after end of file has been reached is undefined for
   a "File.T" but may be defined for a particular subtype.

   The call

| h.write(b)

   is equivalent to

| IF NOT writable(h) THEN `Cause checked runtime error` END;
| FOR i := 0 TO NUMBER(b)-1 DO
|   IF snkCur(h) = len(snk(h)) THEN
|     `Extend "snk(h)" by one byte`
|   END;
|   snk(h)[snkCur(h)] := b[i]
|   INC(srcCur(h))
| END;

   The "read" and "write" methods are not alertable because it isn't
   possible to alert a thread blocked in a Win32 "ReadFile" or
   "WriteFile" system call.

   The call

| h.status()

   returns a result whose "type" field contains "type(h)".  See the
   documentation for each subtype of "File.T" for more details,
   including the values of the "modificationTime" and "size" fields of
   the result, if any.

   The call

| h.close()

   is equivalent to

| readable(h) := FALSE;
| writable(h) := FALSE

   Additionally, it releases any subtype-specific resources used by
   "h".  Every file handle should be closed.

   Clients should assume that file handles are unmonitored and should
   avoid concurrent accesses to a file handle from multiple threads.
   A particular subtype of "File.T" may provide a stronger
   specification with respect to atomicity.

*)
