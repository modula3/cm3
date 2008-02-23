(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Sep 26 09:16:54 PDT 1994 by kalsow                   *)
(*      modified on Tue Mar 24 21:10:33 PST 1992 by muller                   *)
(*      modified on Tue Feb 11 22:04:30 PST 1992 by nichols@parc.xerox.com   *)

(* Little-endian version. *)

UNSAFE MODULE Uin EXPORTS Uin;
(* Only unsafe because it needs Usocket.AF_INET and Usocket is unsafe. *)

FROM Utypes IMPORT u_long, u_short, u_char;

TYPE
  A = RECORD a, b, c, d: u_char; END;
  B = RECORD d, c, b, a: u_char; END;

  C = RECORD a, b: u_char; END;
  D = RECORD b, a: u_char; END;


PROCEDURE ntohl(x: u_long): u_long =
  BEGIN
    RETURN LOOPHOLE (B { a := LOOPHOLE (x, A).a, 
      	      	         b := LOOPHOLE (x, A).b,
                         c := LOOPHOLE (x, A).c,
                         d := LOOPHOLE (x, A).d},  u_long);
  END ntohl;

PROCEDURE ntohs(x: u_short): u_short =
  BEGIN
    RETURN LOOPHOLE (D { a := LOOPHOLE (x, C).a, 
      	      	         b := LOOPHOLE (x, C).b},  u_short);
  END ntohs;

PROCEDURE htonl(x: u_long): u_long =
  BEGIN
    RETURN LOOPHOLE (A { a := LOOPHOLE (x, B).a, 
      	      	         b := LOOPHOLE (x, B).b,
                         c := LOOPHOLE (x, B).c,
                         d := LOOPHOLE (x, B).d},  u_long);
  END htonl;

PROCEDURE htons(x: u_short): u_short =
  BEGIN
    RETURN LOOPHOLE (C { a := LOOPHOLE (x, D).a, 
      	      	         b := LOOPHOLE (x, D).b},  u_short);
  END htons;

BEGIN
END Uin.
