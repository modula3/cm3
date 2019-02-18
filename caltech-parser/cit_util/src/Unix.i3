
INTERFACE Unix;

FROM Ctypes IMPORT short, int, long, char_star,
                   char_star_star;

FROM Utypes IMPORT mode_t;

<*EXTERNAL*> PROCEDURE chmod (path: char_star; mode: mode_t): int;
<*EXTERNAL*> PROCEDURE symlink (name1, name2: char_star): int;
<*EXTERNAL*> PROCEDURE readlink (path: char_star; buf: ADDRESS; bufsize: int): int;
<*EXTERNAL*> PROCEDURE gethostname (name: char_star; namelen: int): int;

END Unix.
