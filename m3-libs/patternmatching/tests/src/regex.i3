INTERFACE regex;

FROM Ctypes IMPORT char_star, int;
FROM Utypes IMPORT size_t, off_t;

CONST
  REG_BASIC    = 0;
  REG_EXTENDED = 1;
  REG_NOSUB    = 4;

TYPE 
  regex_t = RECORD
    a: ARRAY [0..255] OF int;
  END;

  regex_t_star = REF regex_t;

  regmatch_t = RECORD
    rm_so: off_t;
    rm_eo: off_t;
  END;

  regmatch_t_star = REF regmatch_t;

<*EXTERNAL "my_re_comp"*>
PROCEDURE re_comp(s: char_star): char_star;

<*EXTERNAL "my_re_exec"*>
PROCEDURE re_exec(s: char_star): int;

<*EXTERNAL "regcomp"*>
PROCEDURE regcomp(VAR re: regex_t_star; s: char_star; flags: int): int;

<*EXTERNAL "regexec"*>
PROCEDURE regexec(READONLY re: regex_t_star; 
                  READONLY s: char_star; nmatch: size_t;
                  READONLY pmatch: regmatch_t_star; eflags: int): int;

<*EXTERNAL "regerror"*>
PROCEDURE regerror(errcode: int; READONLY re: regex_t_star; 
                   errbuf: char_star; errbuf_size: size_t): size_t;

<*EXTERNAL "regfree"*>
PROCEDURE regfree(READONLY re: regex_t_star);

END regex.
