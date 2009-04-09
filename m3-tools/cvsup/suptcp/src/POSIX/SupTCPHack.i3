(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by wobber *)
(* $Id: SupTCPHack.i3,v 1.1.1.1 2009-04-09 17:02:04 jkrell Exp $ *)

INTERFACE SupTCPHack;

PROCEDURE RefetchError(fd: INTEGER): BOOLEAN;
  (* Awful hack to retrieve a meaningful error from a TCP accept
     socket.  Only works on Ultrix and OSF.  Leaves result
     in standard C's "errno".  Returns "TRUE" if and only if it might
     possibly have succeeded. *)

END SupTCPHack.
