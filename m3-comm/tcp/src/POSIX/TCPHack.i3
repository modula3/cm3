(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by wobber *)
(* $Id: TCPHack.i3,v 1.3 2003-07-28 14:12:14 wagner Exp $ *)

INTERFACE TCPHack;

PROCEDURE RefetchError(fd: INTEGER): BOOLEAN;
  (* Awful hack to retrieve a meaningful error from a TCP accept
     socket.  Only works on Ultrix and OSF.  Leaves result
     in standard C's "errno".  Returns "TRUE" if and only if it might
     possibly have succeeded. *)

END TCPHack.
