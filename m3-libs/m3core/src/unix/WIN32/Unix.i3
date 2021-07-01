(* Copyright (C) 1993, Digital Equipment Corporation                  *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)

INTERFACE Unix;

FROM Cstddef IMPORT size_t;
FROM Ctypes IMPORT const_int, int, const_char_star, char_star, char_star_star, unsigned;
FROM Utypes IMPORT off_t, mode_t;

CONST

<*EXTERNAL "Unix__F_OK"*> VAR F_OK: const_int; (* existance *)
<*EXTERNAL "Unix__X_OK"*> VAR X_OK: const_int; (* executable *)
<*EXTERNAL "Unix__W_OK"*> VAR W_OK: const_int; (* writable *)
<*EXTERNAL "Unix__R_OK"*> VAR R_OK: const_int; (* readable *)
<*EXTERNAL "Unix__access"*>PROCEDURE access (path: const_char_star; mode: int): int;

<*EXTERNAL "Unix__chdir"*>PROCEDURE chdir (path: const_char_star): int;
<*EXTERNAL "Unix__close"*>PROCEDURE close (d: int): int;
<*EXTERNAL "Unix__dup"*>PROCEDURE dup (oldd: int): int;
<*EXTERNAL "Unix__dup2"*>PROCEDURE dup2 (oldd, newd: int): int;
<*EXTERNAL "Unix__execve"*>PROCEDURE execve (name: const_char_star;  argv, envp: char_star_star): int;

<*EXTERNAL "Unix__exit"*>PROCEDURE exit (i: int);
<*EXTERNAL "Unix__underscore_exit"*>PROCEDURE underscore_exit (i: int);

<*EXTERNAL "Unix__gethostname"*>PROCEDURE gethostname (name: char_star; namelen: size_t): int;
<*EXTERNAL "Unix__getcwd"*>PROCEDURE getcwd (pathname: char_star; size: size_t): char_star;

<*EXTERNAL "Unix__lseek"*>
PROCEDURE lseek (d: int; offset: off_t; whence: int): off_t;

<*EXTERNAL "Unix__tell"*>
PROCEDURE tell (d: int): off_t;

<*EXTERNAL "Unix__mkdir"*>
PROCEDURE mkdir (path: const_char_star; mode: mode_t): int;

<*EXTERNAL "Unix__O_RDONLY"*>    VAR O_RDONLY: const_int;
<*EXTERNAL "Unix__O_RDWR"*>      VAR O_RDWR: const_int;
<*EXTERNAL "Unix__O_CREAT"*>     VAR O_CREAT: const_int;
<*EXTERNAL "Unix__O_EXCL"*>      VAR O_EXCL: const_int;
<*EXTERNAL "Unix__O_TRUNC"*>     VAR O_TRUNC: const_int;
<*EXTERNAL "Unix__O_NONBLOCK"*>  VAR O_NONBLOCK: const_int;
<*EXTERNAL "Unix__O_APPEND"*>    VAR O_APPEND: const_int; (* append on each write *)
<*EXTERNAL "Unix__O_NDELAY"*>    VAR O_NDELAY: const_int; (* compat *)
<*EXTERNAL "Unix__M3_NONBLOCK"*> VAR M3_NONBLOCK: const_int; (* compat *)

<*EXTERNAL "Unix__open"*>
PROCEDURE open (name: const_char_star; flags: int; mode: mode_t): int;

CONST
  readEnd = 0;
  writeEnd = 1;
PROCEDURE pipe (VAR fildes: ARRAY [0..1] OF int): int;

<*EXTERNAL "Unix__rename"*>
PROCEDURE rename (from, to: const_char_star): int;
<*EXTERNAL "Unix__rmdir"*>
PROCEDURE rmdir (path: const_char_star): int;
<*EXTERNAL "Unix__truncate"*> 
PROCEDURE  truncate (file: const_char_star; length: off_t): int;
<*EXTERNAL "Unix__ftruncate"*>
PROCEDURE ftruncate (file: int;             length: off_t): int;
<*EXTERNAL "Unix__unlink"*>
PROCEDURE unlink (path: const_char_star): int;

<*EXTERNAL "Unix__link"*>
PROCEDURE link (name1, name2: const_char_star): int;
<*EXTERNAL "Unix__chmod"*>
PROCEDURE chmod (path: const_char_star; mode: mode_t): int;
<*EXTERNAL "Unix__fchmod"*>
PROCEDURE fchmod (fd: int; mode: mode_t): int;
<*EXTERNAL "Unix__creat"*>
PROCEDURE creat (name: const_char_star; mode: mode_t): int;

<*EXTERNAL "Unix__isatty"*>
PROCEDURE isatty (file: int): int;

<*EXTERNAL "Unix__system"*>
PROCEDURE system (string: const_char_star): int;

<*EXTERNAL Unix__sleep*> PROCEDURE sleep (a: unsigned): unsigned;

<*EXTERNAL "Unix__Assertions"*>
PROCEDURE Assertions();
END Unix.
