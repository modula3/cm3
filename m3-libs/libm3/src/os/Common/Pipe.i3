(* Copyright (C) 1993, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Jul 15 12:07:50 PDT 1993 by mcjones *)

(* A "Pipe.T", or pipe, is a file handle that provides access to one
   endpoint of a unidirectional channel that is typically used to
   communicate between a parent and a child process or two sibling
   processes.  (See "Process.Create".) *)

INTERFACE Pipe;

IMPORT File, OSError;

TYPE T <: File.T;

VAR (*CONST*) FileType: File.Type;
(* Equal to {\tt Atom.FromText(\char'42Pipe\char'42).} *)

PROCEDURE Open(VAR (*OUT*) hr, hw: T) RAISES {OSError.E};
(* Create a new channel allowing bytes written to "hw" to be read from
   "hr".  *)

END Pipe.

(* Like every "File.T", a pipe "h" has the components

| type(h)      `an atom, equal to "FileType"`
| readable(h)  `a boolean`
| writable(h)  `a boolean`

   Exactly one of "readable(h)" and "writable(h)" is true (until the
   pipe is closed).

   A pipe "h" also has the component

| channel(h)   `a channel`

   If there are pipes "hw" and "hr" with "channel(hw) = channel(hr)",
   "writable(hw)", and "readable(hr)", then a process holding "hw" can
   send information to a process holding "hr".

   A channel "c" has the components

| seq(c)  `a sequence of bytes`
| w(c)    `a non-negative integer, the index of the next byte to write`
| r(c)    `a non-negative integer, the index of the next byte to read`
| nw(c)   `a non-negative integer, the number of pipes writing "c"`
| nr(c)   `a non-negative integer, the number of pipes reading "c"`

   It is possible (but not very useful) for a channel to have values of
   "nw(c)" or "nr(c)" other than zero or one (see "Process.Create").

   "Open" creates a channel "c" with

| w(c) = r(c) = 0
| nw(c) = nr(c) = 1

   and two pipes "hr" and "hw" with

| type(hr) = type(hw) = FileType
| readable(hr) = writable(hw) = TRUE
| writable(hr) = readable(hw) = FALSE
| channel(hr) = channel(hw) = c


   The meaning of the call

| h.read(b, mayBlock)

   is given by the specification of "File.T.read" together with these
   definitions, where "c = channel(h)":

| src(h)    = seq(c)
| srcCur(h) = r(c)
| srcEof(h) = (nw(c) = 0)

   Note that end-of-file is not reported until after the last pipe
   that can write on the channel is closed; subsequent reads are legal
   but always report end-of-file.

   The meaning of the call

| h.write(b)

   is given by the specification of "File.T.write" together with these
   definitions, where "c = channel(h)":

| snk(h)    = seq(c)
| snkCur(h) = w(c)

   In some implementations, a channel has a bounded buffer, so "write"
   may have to block.  If "nr(channel(h)) = 0", that is, no pipe can
   read "h"'s channel, "write" raises "OSError.E".

   The call

| h.status(stat)

   assigns "FileType" to "stat.type".  Its effect on
   "stat.modificationTime" and "stat.size" is undefined.

   The call

| h.close()

   is equivalent to

| IF readable(h) THEN
|   DEC(nr(channel(h)))
| ELSE
|   DEC(nw(channel(h)))
| END;
| readable(h) := FALSE;
| writable(h) := FALSE

   The channel connecting a pair of pipes is necessarily monitored,
   since the purpose of the channel is to allow asynchronous
   communication via the pipes.  Nevertheless, an individual pipe
   should be treated as unmonitored, thus avoiding the question of the
   unit of atomicity for reads and writes.

*)
