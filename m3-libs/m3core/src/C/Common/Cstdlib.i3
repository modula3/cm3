(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Cstdlib.i3                                            *)
(* Last modified on Tue Nov 20 04:03:35 1990 by muller         *)
(*      modified on Sat Jan 20 22:12:33 1990 by jerome         *)

INTERFACE Cstdlib;

FROM Ctypes IMPORT int, char_star, const_char_star, char_star_star, double,
                   void_star;
FROM Cstddef IMPORT size_t;

<*EXTERNAL*> PROCEDURE abort ();

<*EXTERNAL*> PROCEDURE atexit (func: PROCEDURE ()): int;

<*EXTERNAL*> PROCEDURE exit (status: int);

<*EXTERNAL*> PROCEDURE getenv (name: const_char_star): char_star;

<*EXTERNAL*> PROCEDURE system (string: const_char_star): int;

<*EXTERNAL*> PROCEDURE malloc (size: size_t): void_star;

<*EXTERNAL*> PROCEDURE free (ptr: void_star);

<*EXTERNAL*> PROCEDURE strtod (str: const_char_star;
                               ptr: char_star_star): double;

<*EXTERNAL*> PROCEDURE atof (str: const_char_star): double;

END Cstdlib.
