(* Copyright (C) 1993, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Fri Jul 15 13:48:55 PDT 1994 by mcjones *)
(*      modified on Fri Jan 15 17:25:01 PST 1993 by mjordan *)

(* A "RegularFile.T", or regular file handle, provides access to a
   persistent extensible sequence of bytes. *)

INTERFACE RegularFile;

IMPORT File, OSError;

TYPE
  T <: Public;
  Public = File.T OBJECT METHODS
    seek(origin: Origin; offset: INTEGER): INTEGER
      RAISES {OSError.E};
    flush() RAISES {OSError.E};
    lock(): BOOLEAN RAISES {OSError.E};
    unlock() RAISES {OSError.E}
  END;
  Origin = {Beginning, Current, End};

VAR (*CONST*) FileType: File.Type;
(* Equal to {\tt Atom.FromText(\char'42RegularFile\char'42).} *)

END RegularFile.

(* Like every "File.T", a regular file handle "h" has the components
   
| type(h)      `an atom, equal to "FileType"`
| readable(h)  `a boolean`
| writable(h)  `a boolean`

   A regular file handle "h" also has the components

| cur(h)       `an integer, the index of the next byte to read or write`
| file(h)      `the identity of a regular file`

   There may be distinct regular file handles "h1" and "h2" with
   "file(h1)" equal to "file(h2)", and more than one process may hold
   a single regular file handle (see "Process.Create").

   A regular file (not a handle) "f" has the components
      
| buffer(f)  `an extensible byte sequence`
| stable(f)  `an extensible byte sequence`
| mtime(f)   `a "Time.T", the last modification time`
| locked(f)  `a "Process.ID"`

   The sequences "buffer(f)" and "stable(f)" are zero-based and always
   have the same length.  "stable(f)" represents the contents of the
   file on the disk or other persistent storage medium, while
   "buffer(f)" represents write-behind caching performed by the
   operating system.  From time to time, a daemon performs

| WITH i = `some integer "i" in the range "[0..len(buffer(f))-1]"` DO
|   stable(f)[i] := buffer(f)[i]
| END

   The methods described in this interface are atomic with respect to
   the daemon.

   The meaning of the call

| h.read(b, mayBlock)

   is given by the specification of "File.T.read" together with
   these definitions, where "f = file(h)":

| src(h)    = buffer(f)
| srcCur(h) = cur(h)
| srcEof(h) = TRUE

   Because "srcEof(h)" is always "TRUE", "read" never blocks.
   However, a subsequent read can return more data if an interleaved
   write extends "buffer(f)".  If "cur(h)" is negative (because of a
   prior seek), "read" raises "OSError.E".

   The meaning of the call

| h.write(b)

   is given by the specification of "File.T.write" together with these
   definitions, where "f = file(h)":

| snk(h)    = buffer(f)
| snkCur(h) = cur(h)

   In addition, "write" sets "mtime(file(h))" to the current time.  If
   "write" is called when "cur(h) > size(f)" (because of a prior
   seek), it extends "f" with bytes of undefined value.  If "cur(h)"
   is negative, "write" raises "OSError.E".

   The call

| h.status(stat)

   is equivalent to the following, in which "stat" is a local variable
   of type "Status":

| stat.type := FileType;
| stat.modificationTime := mtime(file(h));
| stat.size := len(buffer(file(h)));
| RETURN stat

   The call

| h.seek(origin, offset)

   is equivalent to
   
| CASE origin OF
|   Origin.Beginning => cur(h) := offset
| | Origin.Current => cur(h) := cur(h)+offset
| | Origin.End => cur(h) := len(buffer(file(h)))+offset
| END;
| RETURN cur(h)

   Note that "seek" never changes the length of the file, although a
   subsequent write may do so.  Use the call "h.seek(Origin.Current,
   0)" to determine "cur(h)" without changing it.

   The call

| h.flush()

   is equivalent to

| WITH f = file(h) DO
|   FOR i := 0 TO len(buffer(f))-1 DO
|     stable(f)[i] := buffer(f)[i]
|   END
| END

   The call

| h.close()

   extends the normal action of the "close" method with

| IF locked(file(h) = Process.GetMyID() THEN
|   locked(file(h)) := Process.NullID
| END
      
   If the file h is not already locked by the calling process (i.e.,
   if locked(file(h)) # Process.GetMyID()), the call 

| h.lock()

   is equivalent to: 

| IF locked(file(h)) = Process.NullID THEN
|   locked(file(h)) := Process.GetMyID();
|   RETURN TRUE
| ELSIF locked(file(h)) = Process.GetMyID() THEN
|   RETURN TRUE
| END;
| RETURN FALSE

   In the event that h is already locked by the calling process, the result
   of h.lock() is implementation-dependent. However, clients can work around
   the undefined nature of the operation in this case by keeping track of
   locked(file(h)) explicitly. 

   The call

| h.unlock()

is equivalent to:

| IF locked(file(h)) # Process.GetMyID() THEN
|   RAISE OSError.E
| END;
| locked(file(h)) := Process.NullID

   Some implementations raise an exception if a process tries to read
   or write a file locked by another process.  You should treat this
   as a checked runtime error rather than writing code to catch and
   recover from the exception; the same applies to unlocking a file
   that you didn't lock.

   You lock a file with code like

| CONST
|   MaxTry = 3;
|   RetryInterval = 5.0D0;
| VAR try := 1;
| BEGIN
|   WHILE NOT h.lock() DO
|     IF try=MaxTry THEN `Give up` END;
|     INC(try);
|     Time.Pause(RetryInterval)
|   END;
|   TRY `Read or write "h"` FINALLY h.unlock() END
| END

   The regular file underlying a regular file handle is monitored,
   thus allowing concurrent operations.  We leave unspecified the unit
   of atomicity for reads and writes, so a set of processes sharing a
   file that needs to be updated should use the "lock" and "unlock"
   methods.  A regular file handle itself should be treated as
   unmonitored.  A client thread typically needs to perform a "seek"
   followed by a "read" or "write" as an atomic unit, which can be
   implemented with a mutex in the client.


*)
