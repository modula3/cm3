(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue Jun 15 10:06:38 1993 by gnelson        *)
(*      modified on Thu May 27 17:20:56 PDT 1993 by swart      *)
(*      modified on Fri Feb  7 00:03:04 PST 1992 by muller     *)
(*      modified on Tue Jan 28 12:45:57 PST 1992 by kalsow     *)



(* The UnsafeRd interface is similar to Rd, but GetChar, GetSub and
Eof are the only operations that are sufficiently performance-critical
to be included.

Note that if you import this interface along with RdClass that you must
include the following line:

   REVEAL RdClass.Private <: MUTEX

in order to satisfy the constraint that the revealed supertypes of an
opaque type be totally ordered.  *)

INTERFACE UnsafeRd;
IMPORT Rd, Thread; 
FROM Thread IMPORT Alerted;
FROM Rd IMPORT Failure, EndOfFile;

REVEAL
  Rd.T <: Thread.Mutex;

PROCEDURE FastGetChar(rd: Rd.T): CHAR RAISES {EndOfFile, Failure, Alerted};
(* Like Rd.GetChar, but rd must be locked. *)

PROCEDURE FastGetSub(rd: Rd.T; VAR (*OUT*) str: ARRAY OF CHAR): CARDINAL
    RAISES {Failure, Alerted};
(* Like Rd.GetSub, but rd must be locked. *)

PROCEDURE FastEOF(rd: Rd.T): BOOLEAN RAISES {Failure, Alerted};
(* Like Rd.EOF, but rd must be locked. *)

PROCEDURE FastUnGetChar(rd: Rd.T) RAISES {};
(* Like Rd.UnGetChar, but rd must be locked. *)

PROCEDURE FastClose (rd: Rd.T) RAISES {Failure, Alerted};

END UnsafeRd.

