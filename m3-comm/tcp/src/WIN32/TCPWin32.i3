(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* by Ted Wobber                                            *)
(*                                                          *)
(* Last modified on Mon Apr 10 16:31:37 PDT 1995 by kalsow  *)
(*      modified on Mon Feb  6 16:35:23 PST 1995 by wobber  *)

INTERFACE TCPWin32;

IMPORT TCP, ConnFD, WinSock;

REVEAL
  TCP.T <: Public;

TYPE
  Public = ConnFD.T OBJECT
             sock  : WinSock.SOCKET;
             closed: BOOLEAN   := FALSE;
           END;

(* The type "Public" reveals enough structure of the Win32 implementation
   of "TCP.T" to allow a client to perform operations directly upon the
   Win32 socket in "sock".  If "closed" is "TRUE", then "sock" is no
   longer valid.  Any operations on "fd" must be performed with the
   object's mutex locked and the caller should assert that "closed" is
   "FALSE". *)

END TCPWin32.
