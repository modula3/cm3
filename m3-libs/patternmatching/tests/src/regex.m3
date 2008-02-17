UNSAFE MODULE regex EXPORTS regex;

FROM Ctypes IMPORT char_star, int;
FROM Utypes IMPORT size_t;

VAR
  grex: regex_t_star := NEW (regex_t_star);
  gbuf: ARRAY [0..255] OF CHAR;

PROCEDURE re_comp(s: char_star): char_star =
  VAR
    ret: int; res: size_t;
    buf: char_star := LOOPHOLE (0, char_star);
  BEGIN
    ret := regcomp (grex, s, REG_NOSUB + REG_NEWLINE);
    IF ret = 0 THEN
      RETURN buf;
    END;
    buf := LOOPHOLE (ADR(gbuf), char_star);
    res := regerror (ret, grex, buf, 256);
    RETURN buf;
  END re_comp;

PROCEDURE re_exec(s: char_star): int =
  VAR
    ret: int;
    pmatch: regmatch_t_star := LOOPHOLE(0, regmatch_t_star);
  BEGIN
    ret := regexec (grex, s, 0, pmatch, 0);
    IF ret = 0 THEN
      RETURN 1;
    ELSE
      RETURN 0;
    END;
  END re_exec;

BEGIN
END regex. 
