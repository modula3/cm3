(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Little-endian version. *)

UNSAFE MODULE Uin;

FROM Ctypes IMPORT unsigned_char, unsigned_short, unsigned;

TYPE
  A = RECORD a, b, c, d: unsigned_char; END;
  B = RECORD d, c, b, a: unsigned_char; END;

  C = RECORD a, b: unsigned_char; END;
  D = RECORD b, a: unsigned_char; END;


PROCEDURE ntohl(x: unsigned): unsigned =
  BEGIN
    RETURN LOOPHOLE (B { a := LOOPHOLE (x, A).a, 
      	      	         b := LOOPHOLE (x, A).b,
                         c := LOOPHOLE (x, A).c,
                         d := LOOPHOLE (x, A).d},  unsigned);
  END ntohl;

PROCEDURE ntohs(x: unsigned_short): unsigned_short =
  BEGIN
    RETURN LOOPHOLE (D { a := LOOPHOLE (x, C).a, 
      	      	         b := LOOPHOLE (x, C).b},  unsigned_short);
  END ntohs;

PROCEDURE htonl(x: unsigned): unsigned =
  BEGIN
    RETURN LOOPHOLE (A { a := LOOPHOLE (x, B).a, 
      	      	         b := LOOPHOLE (x, B).b,
                         c := LOOPHOLE (x, B).c,
                         d := LOOPHOLE (x, B).d},  unsigned);
  END htonl;

PROCEDURE htons(x: unsigned_short): unsigned_short =
  BEGIN
    RETURN LOOPHOLE (C { a := LOOPHOLE (x, D).a, 
      	      	         b := LOOPHOLE (x, D).b},  unsigned_short);
  END htons;

BEGIN
END Uin.
