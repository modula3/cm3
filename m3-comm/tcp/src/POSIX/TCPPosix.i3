(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by wobber *)

INTERFACE TCPPosix;

IMPORT TCP, ConnFD;

REVEAL
  TCP.T <: Public;

TYPE
  Public = ConnFD.T OBJECT
             fd    : INTEGER;
             closed: BOOLEAN   := FALSE;
           END;

(* The type "Public" reveals enough structure of the POSIX implementation
   of "TCP.T" to allow a client to perform operations directly upon the
   POSIX file descriptor in "fd".  If "closed" is "TRUE", then "fd" is no
   longer valid.  Any operations on "fd" must be performed with the
   object's mutex locked and the caller should assert that "closed" is
   "FALSE". *)

END TCPPosix.
