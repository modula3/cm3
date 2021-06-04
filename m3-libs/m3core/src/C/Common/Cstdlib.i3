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

<*EXTERNAL Cstdlib__abort*>  PROCEDURE abort ();
<*EXTERNAL Cstdlib__exit*>   PROCEDURE exit (status: int);
<*EXTERNAL Cstdlib__getenv*> PROCEDURE getenv (name: const_char_star): char_star;
<*EXTERNAL Cstdlib__system*> PROCEDURE system (string: const_char_star): int;
<*EXTERNAL Cstdlib__malloc*> PROCEDURE malloc (size: size_t): void_star;
<*EXTERNAL Cstdlib__calloc*> PROCEDURE calloc (count, size: size_t): void_star;
<*EXTERNAL Cstdlib__free*>   PROCEDURE free (ptr: void_star);
<*EXTERNAL Cstdlib__strtod*> PROCEDURE strtod (str: const_char_star;
                                               ptr: char_star_star): double;
<*EXTERNAL Cstdlib__atof*>  PROCEDURE atof (str: const_char_star): double;

END Cstdlib.
