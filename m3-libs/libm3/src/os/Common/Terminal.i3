(* Copyright (C) 1993, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Fri Jul 15 13:49:36 PDT 1994 by mcjones *)

(* A "Terminal.T", or terminal handle, is a file handle that provides
   access to a duplex communication channel usually connected to a
   user terminal.  *)

INTERFACE Terminal;

IMPORT File;

TYPE T <: File.T;

VAR (*CONST*) FileType: File.Type;
(* Equal to {\tt Atom.FromText(\char'42Terminal\char'42).} *)

END Terminal.

(* Like every "File.T", a terminal handle "h" has the components

| type(h)      `an atom, equal to "FileType"`
| readable(h)  `a boolean`
| writable(h)  `a boolean`

   A terminal handle is readable, or writable, or both (until it is
   closed).  If it is readable, it has the component

| srcTerm(h)   `a terminal device`

   If it is writable, it has the component

| snkTerm(h)   `a terminal device`

   A terminal device "t" has the components

| seq(t)   `a sequence of bytes`
| r(t)     `a non-negative integer, the index of the next byte to read`
| w(t)     `a non-negative integer, the index of the next byte to write`
| flag(t)  `a byte reserved to mark the end-of-file in "seq(t)"`

   The meaning of the call

| h.read(b, mayBlock)

   is given by the specification of "File.T.read" together with these
   definitions, where "t = srcTerm(h)", and "k" is the number of
   occurrences of "flag(t)" in "seq(t)" up to "r(t)-1":

| src(h)    = `subsequence of "seq(t)" with all occurrences of "flag(t)"`
|             `deleted`
| srcCur(h) = r(t)-k
| srcEof(h) = (seq(t)[r(t)] = flag(t))

   When end-of-file is reported, "r(t)" is also incremented.  This
   means subsequent reads can return further data in "seq(t)".

   The meaning of the call

| h.write(b)

   is given by the specification of "File.T.write" together with these
   definitions, where "t = snkTerm(h)":

| snk(h)    = seq(t)
| snkCur(h) = w(t)

   A specific implementation may provide one or more subtypes of
   "Terminal.T" with additional methods.

   The communication channel underlying a terminal handle is
   necessarily monitored, since the purpose of the channel is to allow
   asynchronous communication between a program and a user operating a
   terminal device.  However a terminal handle itself should be
   treated as unmonitored, thus avoiding the question of the unit of
   atomicity for reads and writes.

*)




