(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

UNSAFE MODULE Utime;

IMPORT Cstring, RT0u;
FROM Ctypes IMPORT char_star, int, long_star;

PROCEDURE ctime_r (clock: long_star; buf: char_star; buflen: int): char_star =
  VAR p: char_star;
  BEGIN
    INC (RT0u.inCritical);
      p := ctime (clock);
      EVAL Cstring.strncpy (buf, p, buflen);
    DEC (RT0u.inCritical);
    RETURN buf;
  END ctime_r;

PROCEDURE localtime_r (clock: long_star; res: struct_tm_star): struct_tm_star =
  VAR p: struct_tm_star;
  BEGIN
    INC (RT0u.inCritical);
      p := localtime (clock);
      res^ := p^;
    DEC (RT0u.inCritical);
    RETURN res;
  END localtime_r;

PROCEDURE gmtime_r (clock: long_star; res: struct_tm_star): struct_tm_star =
  VAR p: struct_tm_star;
  BEGIN
    INC (RT0u.inCritical);
      p := gmtime (clock);
      res^ := p^;
    DEC (RT0u.inCritical);
    RETURN res;
  END gmtime_r;

PROCEDURE asctime_r(tm: struct_tm_star; buf: char_star; buflen: int):char_star =
  VAR p: char_star;
  BEGIN
    INC (RT0u.inCritical);
      p := asctime (tm);
      EVAL Cstring.strncpy (buf, p, buflen);
    DEC (RT0u.inCritical);
    RETURN buf;
  END asctime_r;

BEGIN
END Utime.
